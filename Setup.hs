{-| This is a small harness to check the version of the Cabal library and
    build and run the real setup script, @DoSetup.hs@.
-}

import Control.Monad
import Data.Char
import Distribution.Simple.Utils(cabalVersion)
import Distribution.Version(Version(..))
import System.Environment
import System.Exit
import System.Process

-- | Pick a CPP flag based on the Cabal library version
cabalVersionCPPFlags :: [String]
cabalVersionCPPFlags = [major_flag, minor_flag]
  where
    major:minor:_ = versionBranch cabalVersion

    major_flag = "-DCABAL_MAJOR=" ++ show major
    minor_flag = "-DCABAL_MINOR=" ++ show minor

outputDir = "dist/setup"
setupPath = "dist/setup/do-setup"

buildSetup = do
  let flags = ["--make", "-XCPP"] ++ cabalVersionCPPFlags ++
              ["DoSetup.hs", "-outputdir", outputDir, "-o", setupPath]
  ec <- rawSystem "ghc" flags
  unless (ec == ExitSuccess) $
    fail "Error occured when building the setup script"

main = do
  buildSetup
  args <- getArgs
  ec <- rawSystem setupPath args
  exitWith ec
