#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT(OCI, 0.3.1~dev)

AC_CHECK_FUNC(wait4,,AC_MSG_ERROR(wait4 function not available))

ocamlfind_install_dir=$(ocamlfind printconf destdir)/oci

ociuser=$(id --user --name)

AC_SUBST(ociuser)
AC_SUBST(ocamlfind_install_dir)

AC_CONFIG_FILES(.config)
AC_OUTPUT
