#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT(OCI, 0.3~dev)

# Checks for programs.
AC_PROG_INSTALL

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

ocamlfind_install_dir=$(ocamlfind printconf destdir)/oci

ociuser=$(id --user --name)

AC_SUBST(ociuser)
AC_SUBST(ocamlfind_install_dir)

AC_CONFIG_FILES(.config)
AC_OUTPUT
