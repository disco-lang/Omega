
m4_define([AC_OMEGA_IFELSE],
[if test "$ac_with_omega" == "yes" ; then
$1
:
else
$2
:
fi])