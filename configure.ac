
# Initialization
AC_INIT(Omega, 0.1)
AC_LANG(C++)

# Compile with pre-installed Omega?
AC_ARG_WITH(omega,[[Use preinstalled Omega library]],
	[ac_with_omega=yes],
	[ac_with_omega=no])

# Check for programs
AC_PROG_CXX

AC_OMEGA_IFELSE([],[AC_MSG_NOTICE([will build the Omega library from source])])

# Check the omega library
AC_OMEGA_IFELSE([
	AC_MSG_CHECKING([whether we can include basic/bool.h])
	AC_COMPILE_IFELSE(
		[AC_LANG_SOURCE([[#include <basic/bool.h>
			]])],
		[AC_MSG_RESULT([ok])],
		[AC_MSG_FAILURE([cannot include basic/bool.h])])
])

AC_OMEGA_IFELSE([
	AC_MSG_CHECKING([whether we can include omega.h])
	AC_COMPILE_IFELSE(
		[AC_LANG_SOURCE([[#include <omega.h>
			]])],
		[AC_MSG_RESULT([ok])],
		[AC_MSG_FAILURE([cannot include omega.h])])
])

AC_OMEGA_IFELSE([
	AC_MSG_CHECKING([whether we can link with omega library])
	{
	 LIBS="${LIBS} -lomega"
	 AC_LINK_IFELSE(
		[AC_LANG_PROGRAM(
			[[#include <omega.h>
			]],
			[[omega::Relation::Null();]])],
		[AC_MSG_RESULT([yes])],
		[AC_MSG_FAILURE([cannot link with the omega library])])
	}
])


# Output
AC_SUBST(ac_with_omega)
AC_CONFIG_FILES([Makefile])
AC_OUTPUT
