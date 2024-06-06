import Control.Applicative
import Control.Exception (IOException, bracket, catch)
import Control.Monad
import Data.Char
import Data.List hiding (intercalate)
import Data.Maybe
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.Build
import Distribution.Simple.BuildPaths
import Distribution.Simple.GHC
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.PreProcess
import Distribution.Simple.Program
import Distribution.Simple.Program.GHC
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import qualified Distribution.Verbosity as Verbosity
import System.Cmd
import System.Directory
import System.Exit (ExitCode (..), die)
import System.FilePath (takeDirectory, takeExtension, (<.>), (</>))
import System.IO
import System.Process

-- Recover from IO exceptions
recover :: IO a -> IO a -> IO a
f `recover` h = f `Control.Exception.catch` handler
 where
  handler e =
    let _ = e :: IOException
     in h

-------------------------------------------------------------------------------
-- Filenames and constants

-- Record whether we're building the Omega library here
useInstalledOmegaFlagPath = "build" </> "UseInstalledOmega"

-- We will call 'make'
makeProgram = simpleProgram "make"

-- Our single C++ source file and corresponding object file are here
cppSourceName = "src" </> "C_omega.cc"
cppObjectName = "build" </> "C_omega.o"

-- If we're building the Omega library, it's here
omegaLibPath = "src" </> "the-omega-project" </> "omega_lib" </> "obj" </> "libomega.a"

-- Unpack the Omega library into this directory
omegaUnpackPath = "build" </> "unpack_omega"

-- Path where Cabal builds files
cabalBuildPath = "dist" </> "build"

-- Main test file
testSourceName = "test" </> "runtests.hs"

-- Extra files produced by configuration
configFiles =
  [ "configure"
  , "config.log"
  , "config.status"
  , "Makefile"
  , useInstalledOmegaFlagPath
  ]

-------------------------------------------------------------------------------
-- Helpful IO procedures

noGHCiLib =
  die $
    "Sorry, this package does not support GHCi.\n"
      ++ "Please configure with --disable-library-for-ghci to disable."

noSharedLib =
  die $
    "Sorry, this package does not support shared library output.\n"
      ++ "Please configure with --disable-shared to disable."

writeUseInstalledOmegaFlag :: Bool -> IO ()
writeUseInstalledOmegaFlag b = do
  createDirectoryIfMissing False "build"
  writeFile useInstalledOmegaFlagPath (show b)

readUseInstalledOmegaFlag :: IO Bool
readUseInstalledOmegaFlag = do
  text <-
    readFile useInstalledOmegaFlagPath
      `recover` die "Configuration file missing; try reconfiguring"
  return $! read text

-- Run a command if f1 is newer than f2, or if f2 does not exist
ifNewer :: FilePath -> FilePath -> IO () -> IO ()
ifNewer src tgt m = do
  tgt_exists <- doesFileExist tgt
  if not tgt_exists
    then m -- Run because target doesn't exist
    else do
      src_time <- getModificationTime src
      tgt_time <- getModificationTime tgt
      if src_time > tgt_time
        then m -- Run because source is newer
        else return ()

-- Attempt to remove a file, ignoring errors
lenientRemoveFile f = removeFile f `recover` return ()

lenientRemoveFiles = mapM_ lenientRemoveFile

-- Attempt to remove a directory and its contents
-- (one level of recursion only), ignoring errors
lenientRemoveDirectory f = do
  b <- doesDirectoryExist f
  when b $ do
    lenientRemoveFiles . map (f </>) =<< getDirectoryContents f
    removeDirectory f `recover` return ()

-------------------------------------------------------------------------------
-- Source distribution

-- Configure programs
runAutoconf verbosity = ifNewer "configure.ac" "configure" $ do
  rawSystemExit verbosity "autoconf" []

sDistOmega originalHook pkgDesc lbi hooks flags = do
  runAutoconf verb -- Ensure that 'configure' is created
  originalHook pkgDesc lbi hooks flags
 where
  verb = fromFlagOrDefault Verbosity.normal $ sDistVerbosity flags

-------------------------------------------------------------------------------
-- Configuration

configureOmega pkgDesc originalFlags = do
  -- Disable unsupported configuratoins
  when (flagToMaybe (configGHCiLib originalFlags) == Just True) $
    notice verbosity $
      "** Sorry, this package does not support GHCi.\n"
        ++ "** Disabling GHCi library output."

  when (flagToMaybe (configSharedLib originalFlags) == Just True) $
    notice verbosity $
      "** Sorry, this package does not support "
        ++ "shared library output.\n"
        ++ "** Disabling shared library output."

  -- Run Cabal configuration
  lbi <- confHook simpleUserHooks pkgDesc flags

  -- Run autoconf configuration
  runAutoconf verbosity
  runConfigure lbi

  -- Save this flag for later use
  writeUseInstalledOmegaFlag useInstalledOmega

  return lbi
 where
  verbosity = fromFlagOrDefault Verbosity.normal $ configVerbosity flags
  flags =
    originalFlags
      { configSharedLib = toFlag False
      , configGHCiLib = toFlag False
      }

  -- Will build the Omega library?
  useInstalledOmega =
    fromMaybe False $
      lookupFlagAssignment (mkFlagName "useinstalledomega") $
        configConfigurationsFlags flags

  -- Run 'configure' with the extra arguments that were passed to
  -- Setup.hs
  runConfigure lbi = do
    currentDir <- getCurrentDirectory

    let opts = autoConfigureOptions lbi useInstalledOmega
        configProgramName = currentDir </> "configure"

    rawSystemExit verbosity configProgramName opts

-- Configuration: extract options to pass to 'configure'
autoConfigureOptions :: LocalBuildInfo -> Bool -> [String]
autoConfigureOptions localBuildInfo useInstalledOmega =
  withOmega ++ [libdirs, includedirs]
 where
  withOmega =
    if useInstalledOmega
      then ["--with-omega"]
      else []

  libraryDescr = case library $ localPkgDescr localBuildInfo of
    Nothing -> error "Library description is missing"
    Just l -> l

  buildinfo = libBuildInfo libraryDescr

  -- Create a string "-L/usr/foo -L/usr/bar"
  ldflagsString =
    intercalate " " ["-L" ++ dir | dir <- extraLibDirs buildinfo]

  libdirs = "LDFLAGS=" ++ ldflagsString

  -- Create a string "-I/usr/foo -I/usr/bar"
  cppflagsString =
    intercalate " " ["-I" ++ dir | dir <- includeDirs buildinfo]

  includedirs = "CPPFLAGS=" ++ cppflagsString

-------------------------------------------------------------------------------
-- Building

buildOmega pkgDesc lbi userhooks flags = do
  let verb = fromFlagOrDefault Verbosity.normal $ buildVerbosity flags
  useInstalledOmega <- readUseInstalledOmegaFlag

  -- Build the C++ source file (and Omega library, if configured)
  -- Makefile's behavior is controlled by output of 'configure'
  runDbProgram verb makeProgram (withPrograms lbi) ["all"]

  -- Custom build procedure for test suite
  buildTestSuites useInstalledOmega pkgDesc lbi flags

  -- Default build procedure for hs files in library
  -- Tests are already built, so they won't be built again
  buildHook simpleUserHooks pkgDesc lbi userhooks flags

  -- Get 'ar' and 'ld' programs
  let runAr = runDbProgram verb arProgram (withPrograms lbi)

  let -- Add extra files into an archive file
      addStaticObjectFiles libName = do
        -- Add the C++ interface file
        addStaticObjectFile cppObjectName libName

        -- Add contents of libomega.a
        unless useInstalledOmega $
          transferArFiles verb runAr omegaLibPath libName
       where
        addStaticObjectFile objName libName =
          runAr ["r", libName, objName]

  -- Add other object files to libraries
  libName <- getLibraryName pkgDesc lbi

  when (withVanillaLib lbi) $
    let libPath = buildDir lbi </> mkLibName libName
     in addStaticObjectFiles libPath

  when (withProfLib lbi) $
    let libPath = buildDir lbi </> mkProfLibName libName
     in addStaticObjectFiles libPath

  when (withGHCiLib lbi) noGHCiLib
  when (withSharedLib lbi) noSharedLib

  return ()

-- | Get the library name from processing the package description file
getLibraryName :: PackageDescription -> LocalBuildInfo -> IO LibraryName
getLibraryName pkgDesc lbi =
  case find (\(component_name, _, _) -> component_name == CLibName) $
    componentsConfigs lbi of
    Just (_, clbi, _) ->
      case componentLibraries clbi of
        [nm] -> return nm
        _ -> die "Missing library name"

-- | Hide the test suite so Cabal doesn't try to use its default build
--   procedure with it.
-- hideTestComponents :: LocalBuildInfo -> LocalBuildInfo
-- hideTestComponents lbi =
--   lbi {componentsConfigs = mapMaybe removeTests $ componentsConfigs lbi}
--  where
--   removeTests (cname, clbi, deps)
--     | isTestComponent cname = Nothing
--     | otherwise = Just (cname, clbi, filter (not . isTestComponent) deps)

-- isTestComponent :: ComponentName -> Bool
-- isTestComponent (CTestName {}) = True
-- isTestComponent _ = False

-- | Get command line options for invoking GHC
genericGhcOptions ::
  Version ->
  Verbosity.Verbosity ->
  LocalBuildInfo ->
  BuildInfo ->
  ComponentLocalBuildInfo ->
  FilePath ->
  [String]
genericGhcOptions ver verb lbi bi clbi build_path =
  renderGhcOptions ver $ componentGhcOptions verb lbi bi clbi cabalBuildPath

buildTestSuites useInstalledOmega pkgDesc lbi flags =
  withTestLBI pkgDesc lbi $ \test clbi -> do
    let verb = fromFlagOrDefault Verbosity.normal $ buildVerbosity flags

    -- Run preprocessors
    writeAutogenFiles verb pkgDesc lbi
    preprocessComponent pkgDesc (CTest test) lbi False verb knownSuffixHandlers

    -- Run compiler
    (ghcProg, ghcVersion) <- configureGHC verb lbi

    let bi = testBuildInfo test
        opts = genericGhcOptions ghcVersion verb lbi bi clbi cabalBuildPath

        build_opts =
          [ "--make"
          , "-o"
          , cabalBuildPath </> testName test </> testName test
          ]

        -- If building Omega library locally, link to the local archive
        local_link_opt =
          if useInstalledOmega
            then []
            else ["-L" ++ takeDirectory omegaLibPath]

        input_opts =
          [ testSourceName -- Source file
          , cppObjectName -- Compiled C++ file
          , "-lomega" -- Omega library
          , "-lstdc++" -- C++ libarry
          ]
        all_opts = build_opts ++ opts ++ local_link_opt ++ input_opts

    createDirectoryIfMissing True (cabalBuildPath </> testName test)
    runProgram verb ghcProg all_opts

configureGHC verb lbi = do
  (ghcProg, _) <- requireProgram verb ghcProgram (withPrograms lbi)
  ghcVersion <- case programVersion ghcProg of
    Just x -> return x
    Nothing -> die "Can't determine GHC vesion"
  return (ghcProg, ghcVersion)

-- Transfer the contents of one archive to another
transferArFiles verb runAr src dst = do
  srcCan <- canonicalizePath src
  dstCan <- canonicalizePath dst

  -- Create/remove a temporary directory
  bracket createUnpackDirectory (\_ -> removeUnpackDirectory) $ \_ ->
    -- Save/restore the current working directory
    bracket getCurrentDirectory setCurrentDirectory $ \_ -> do
      -- Go to temporary directory
      setCurrentDirectory omegaUnpackPath

      -- Unpack source archive
      runAr ["x", srcCan]

      -- Find object files
      objs <- liftM (filter isObjectFile) $ getDirectoryContents "."
      when (null objs) $ warn verb "No object files found in Omega library; build may be incomplete"

      -- Insert into destination archive
      runAr (["r", dstCan] ++ objs)
 where
  isObjectFile f = takeExtension f == ".o"

  createUnpackDirectory = createDirectoryIfMissing True omegaUnpackPath
  removeUnpackDirectory = removeDirectoryRecursive omegaUnpackPath

-------------------------------------------------------------------------------
-- Cleaning

cleanOmega pkgDesc mlbi userhooks flags = do
  let verb = fromFlagOrDefault Verbosity.normal $ cleanVerbosity flags

  -- run 'make clean', which will clean the Omega library if appropriate
  pgmConf <- configureProgram verb makeProgram defaultProgramConfiguration
  makeExists <- doesFileExist "Makefile"
  when makeExists $
    runDbProgram verb makeProgram pgmConf ["clean"]

  -- Clean extra files if we don't need to save configuration
  -- (Other temp files are automatically cleaned)
  unless (fromFlag $ cleanSaveConf flags) $ do
    lenientRemoveFiles configFiles
    lenientRemoveDirectory "autom4te.cache"

  -- Do default clean procedure
  cleanHook simpleUserHooks pkgDesc mlbi userhooks flags

-------------------------------------------------------------------------------
-- Hooks

hooks =
  simpleUserHooks
    { hookedPrograms = [arProgram, makeProgram]
    , confHook = configureOmega
    , buildHook = buildOmega
    , cleanHook = cleanOmega
    --    , sDistHook = sDistOmega (sDistHook simpleUserHooks)
    }

main = defaultMainWithHooks hooks
