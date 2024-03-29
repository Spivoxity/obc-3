#
# configure.in
# 
# This file is part of the Oxford Oberon-2 compiler
# Copyright (c) 2006--2016 J. M. Spivey
# All rights reserved
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met: 
#
# 1. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above
#    copyright notice, this list of conditions and the following
#    disclaimer in the documentation and/or other materials provided
#    with the distribution. 
# 3. The name of the author may not be used to endorse or promote products
#    derived from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS
# OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
# DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
# GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
# WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#

# Process this file with autoconf to produce a configure script.

AC_INIT([obc],[3.4alpha],[mike@cs.ox.ac.uk or http://github.com/Spivoxity/obc-3/issues])
AC_PREREQ([2.69])
AC_CONFIG_SRCDIR(compiler/tree.mli)
AC_CONFIG_AUX_DIR(scripts)
AC_CONFIG_FILES(Makefile compiler/Makefile runtime/Makefile lib/Makefile \
	test/Makefile winport/Makefile debugger/Makefile \
	lablgtk3/Makefile macport/Makefile mllib/Makefile \
        armtest/Makefile mipstest/Makefile yacc/Makefile thunder/Makefile)
AC_CONFIG_FILES(compiler/config.ml debugger/debconf.ml)
AC_CONFIG_FILES(test/regress, chmod +x test/regress)
AC_CONFIG_FILES(test/bigtest, chmod +x test/bigtest)
AC_CONFIG_FILES(test/edtest, chmod +x test/edtest)
AC_CONFIG_HEADERS(config.h)

: ${AUTOCONF=autoconf} ${AUTOHEADER=autoheader}
AC_SUBST(AUTOCONF)
AC_SUBST(AUTOHEADER)
AC_SUBST(DEPEND, 1)

# Version info
MAGIC=OBCX
AC_DEFINE_UNQUOTED(MAGIC, "$MAGIC", [Magic number for trailer])
SIG=[`echo $PACKAGE_VERSION | sed -e 's/alpha/.90/' \
        -e 's/beta\([1-9]\)/.9\1/' -e 's/[0-9]*/0&/g' \
	-e 's/0\([0-9][0-9]\)/\1/' -e 's/\.//g' -e 's/^/0x00/'`]
AC_DEFINE_UNQUOTED(SIG, $SIG, [Version signature for symbol tables])
AC_SUBST(SIG)
DOS_VERSION=`echo $PACKAGE_VERSION | sed 's/\./-/g'`

ABS_SRCDIR=`cd $srcdir; pwd`
ABS_BINDIR=`pwd`
AC_SUBST(ABS_SRCDIR)
AC_SUBST(ABS_BINDIR)

# Host dependent setup
AC_CANONICAL_HOST

AC_ARG_ENABLE(m64x32,
  AS_HELP_STRING([--disable-m64x32], [disable 64-bit runtime for amd64]))

DIRSEP="/"
LD=ld

case $host in
  *-*-linux-*)
    AC_DEFINE(LINUX, 1, [Define for Linux]);;

  *-*-mingw*|*-*-cygwin)
    AC_SUBST(WINDOWS, 1)
    AC_DEFINE(WINDOWS, 1, [Define to enable hacks for Windows])
    DIRSEP="\\\\";;

  *-*-darwin*)
    AC_SUBST(MACOS, 1)
    AC_DEFINE(MACOS, 1, [Define to enable hacks for MacOS X]);;

  *-*-freebsd*)
    AC_SUBST(FREEBSD, 1)
    AC_DEFINE(FREEBSD, 1, [Define to enable hacks for FreeBSD]);;
esac    

AC_DEFINE_UNQUOTED(DIRSEP, "$DIRSEP", [Directory separator in file names])

case $host in
  x86_64-*)
    if test "$enable_m64x32" != no; then
      AC_DEFINE(M64X32, 1, [Define for 64-bit runtime with 32-bit pointers])
      AC_DEFINE(USE_SSE, 1, [Define to use SSE2 in place of x87 floats])
    fi
    # Architecture for windows installation
    AC_SUBST(WIN_ARCH, x64);;
esac

case $host in
  i686-*|x86_64-*)
    AC_DEFINE(UNALIGNED_MEM, 1, [Define if unaligned loads and stores work]);;
esac

case $host in
  x86_64-*-linux-*)
    if test "$enable_m64x32" = no; then
      CC='gcc -m32'; LD='ld -melf_i386'
    fi;;

  x86_64-*-mingw*|*-*-cygwin) 
    WIN=win64
    AC_SUBST(ROOT, C:/msys64)
    AC_SUBST(GTKROOT, C:/msys64/mingw64)
    AC_SUBST(PKG_FLAGS, --define-variable=prefix=C:/msys64/mingw64)
    AC_SUBST(CFLAGS_JIT, [-Wl,--stack,16777216])
    if test "$enable_m64x32" = no; then
      CC=gcc32; LD=ld32
    fi;;
    
  i686-*-mingw*|*-*-cygwin) 
    AC_SUBST(ROOT, C:/msys32)
    AC_SUBST(GTKROOT, C:/msys32/mingw32)
    AC_SUBST(PKG_FLAGS, --define-variable=prefix=C:/msys32/mingw32)
    WIN=win32
    AC_SUBST(CFLAGS_JIT, [-Wl,--stack,4194304]);;
    
  x86_64-*-darwin*)
    if test "$enable_m64x32" = no; then
      CC='gcc -m32'
    fi;;
esac

AC_SUBST(WIN_INSTALL, obc-$WIN-$DOS_VERSION)
AC_SUBST(LD)

AC_ARG_ENABLE(segmem,
  AS_HELP_STRING([--enable-segmem], [use segmented addressing]))
if test "$enable_segmem" = yes; then
  AC_DEFINE(SEGMEM, 1, [Define to use segmented addressing])
fi

GTKPKGS="gtk+-3.0 gtksourceview-3.0"

if test "$MACOS" = 1; then GTKPKGS="$GTKPKGS gtk-mac-integration-gtk3"; fi
AC_SUBST(GTKPKGS)

# Checks for programs
AC_PROG_CC
AC_PROG_INSTALL
AC_PROG_MAKE_SET
AC_PROG_RANLIB

AC_CHECK_PROGS(TCLSH, tclsh)
test "$TCLSH" != "" || AC_MSG_ERROR(tclsh not found)

AC_CHECK_PROGS(DIFF, gdiff diff)
test "$DIFF" != "" || AC_MSG_ERROR(diff not found)

: ${GTKDIR=/usr}
AC_ARG_VAR(GTKDIR, [Directory to find GTK])
AC_PATH_PROG(PKG_CONFIG, pkg-config, /usr/bin/pkg-config, 
  $GTKDIR/bin$PATH_SEPARATOR$PATH)
test "$PKG_CONFIG" != "" || AC_MSG_ERROR(pkg-config not found)

# Disable fancy alias analysis if compiling with gcc
: ${RTFLAGS='${CFLAGS}'}
if test "$GCC" == yes; then
    RTFLAGS="$RTFLAGS -fno-strict-aliasing"
fi
AC_SUBST(RTFLAGS)

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_C_BIGENDIAN
AC_C_INLINE
AC_C_INDEXED_JUMPS
AC_C_UNUSED
AC_C_USED
AC_C_NORETURN
AC_FIND_PAGESIZE

# Checks for header files.
AC_CHECK_HEADERS(time.h sys/time.h)

# Checks for library functions.
AC_FUNC_VPRINTF
AC_CHECK_FUNCS(clock time gettimeofday strtoul stpcpy)
AC_CHECK_FUNCS(mmap sigprocmask) # assume they work OK for our purposes
AC_CHECK_FUNCS(getopt_long_only)

AC_CHECK_LIB(m, sinf, MATHLIB=-lm)
AC_SUBST(MATHLIB)

# Checks for X
AC_PATH_XTRA
if test "$no_x" != yes; then
  AC_SUBST(HAVE_X, 1)
  AC_SUBST(XYPLANE, XYplane)
  AC_SUBST(XLIB, "$X_LIBS -lX11")
fi

# OCaml stuff
AC_ARG_VAR(MLFLAGS, [Flags to pass to ocamlc])
if test "$WINDOWS" = 1; then
  AC_SUBST(MLLINKFLAGS, "-ccopt -L/lib/mingw")
fi

# Check ocaml version
AS_ECHO_N([checking ocaml version...])
if ! ocamlc; then echo not found; else
  OCAMLV=`ocamlc -v | sed -n '1s/.*version \([[0-9]]*\.[[0-9]]*\).*/\1/p'`
  AS_ECHO($OCAMLV)
  TMPV=`echo $OCAMLV | \
      sed -e 's/\./\.0000/' -e 's/\.0*\([[0-9]][[0-9]][[0-9]][[0-9]]\)/\1/'`
  if test $TMPV -lt 40002; then
      AC_SUBST(COMPAT, bytes.cmo)
  fi
  if test $TMPV -lt 40006; then
      AC_SUBST(SYS_COMPAT, sys_compat1.ml.in)
  else
      AC_SUBST(SYS_COMPAT, sys_compat2.ml.in)
  fi
fi


# PACKAGE OPTIONS

# Option debug
AC_ARG_ENABLE(debug,
  AS_HELP_STRING([--enable-debug], [set up for debugging]),
  if test "$enable_debug" = yes; then 
    DEBUG=1
    AC_DEFINE(DEBUG, 1, [Define to set up for debugging]) 
    : ${MLFLAGS=-g}
  fi)
AC_SUBST(DEBUG)

# Option dynlink
AC_ARG_ENABLE(dynlink,
  AS_HELP_STRING([--disable-dynlink], [do not use dynamic linking]))
if test "$enable_dynlink" != no && test "$WINDOWS" != 1; then
  DYNLINK=1
  AC_DEFINE(DYNLINK, 1, [Define if dynamic linking enabled])
  AC_SEARCH_LIBS(dlopen, dl)
  AC_TEST_RDYNAMIC(DLFLAGS="-rdynamic")
fi
AC_SUBST(DYNLINK)
AC_SUBST(DLFLAGS)

case $host in
  *-*-darwin*) 
    SHL_CFLAGS="-fPIC -bundle -undefined dynamic_lookup";;
  *)           
    SHL_CFLAGS="-fPIC"; SHL_LFLAGS="-shared";;
esac
AC_SUBST(SHL_CFLAGS)
AC_SUBST(SHL_LFLAGS)
AC_SUBST(DLEXT, [.so])
AC_DEFINE_UNQUOTED(DLEXT, "$DLEXT", [Extension for dynamic libraries])

# Option ffi
AC_ARG_ENABLE(ffi,
  AS_HELP_STRING([--disable-ffi], [do not use FFI with dynlink]))
AC_CHECK_HEADERS(ffi.h, HAVE_FFI_H=1)
if test "$enable_ffi" != no && test "$DYNLINK" = 1 \
    && test "$HAVE_FFI_H" = 1; then
  AC_DEFINE(USE_FFI, 1, [Define if FFI enabled])
  AC_SEARCH_LIBS(ffi_call, ffi)
fi

# Option gc
AC_ARG_ENABLE(gc,
  AS_HELP_STRING([--enable-gc=option],
    [Use 'yes' for Oberon GC, 'boehm' for Boehm library]))
if test "$enable_gc" = boehm; then
  GC_OBJ=gc0.o; GC_LIB=-lgc
  AC_DEFINE(USE_BOEHM, 1, [Define to use Boehm garbage collector])
elif test "$enable_gc" != no; then
  GC_OBJ=gc.o; GC_LIB=
else
  GC_OBJ=gc0.o; GC_LIB=
fi
AC_SUBST(GC_OBJ)
AC_SUBST(GC_LIB)

# Option jit
CODEPAGE=16384
FLOATOPS=1
AC_ARG_ENABLE(jit,
  AS_HELP_STRING([--disable-jit], [disable JIT translator]))
if test "$enable_segmem" == yes; then enable_jit=no; fi
if test "$enable_jit" != no; then
  case $host in
    i?86-*) 
      ENABLE_JIT=1; VM=vm386.o
      AC_DEFINE(NEED_FPINIT, 1, [Reboot FPU for each primitive]);;
    x86_64-*) 
      ENABLE_JIT=1; VM=vm386.o;;
    armv6l-*|armv7l-*|arm-*)
      ENABLE_JIT=1; VM=vmpi.o; CODEPAGE=4096
      AC_DEFINE(USE_FLUSH, 1, [Whether to flush the cache]);;
    mipsel-*)
      ENABLE_JIT=1; VM=vmomega.o; FLOATOPS=0
      AC_DEFINE(USE_FLUSH, 1);;
  esac
fi
AC_ARG_ENABLE(floatops,
  AS_HELP_STRING([--disable-floatops], [disable JIT for floating point]))
if test "$enable_floatops" = no; then FLOATOPS=0; fi
AC_SUBST(VM)
AC_SUBST(ENABLE_JIT)
AC_DEFINE_UNQUOTED([CODEPAGE], $CODEPAGE, [Size of block for JIT code])
if test $FLOATOPS = 1; then
  AC_DEFINE(FLOATOPS, 1, [Whether to use thunder for floating point])
fi

# Option debugger
AC_ARG_ENABLE(debugger,
  AS_HELP_STRING([--disable-debugger], [disable runtime debugger]))
if test "$enable_debugger" != no; then 
  AC_SUBST(ENABLE_DEBUGGER, 1)
fi
if test "$enable_debugger" = inet || test "$WINDOWS" = 1; then 
  AC_SUBST(USE_INET, 1)
  AC_DEFINE(USE_INET, 1, [Whether to use inet sockets for the debugger])
fi

OBDB1='"$LIBDIR/obdb1 -R $LIBDIR/resources"'
OBDB1_DEBUG='"$BINDIR/debugger/obdb1 -R $ROOT$TOPDIR/resources"'
if test "$MACOS" = 1; then
  OBDB1='"$LIBDIR/Obdb.app/Contents/MacOS/obdb"'
  OBDB1_DEBUG='"$BINDIR/macport/Obdb.app/Contents/MacOS/obdb"'
fi
AC_SUBST(OBDB1)
AC_SUBST(OBDB1_DEBUG)   

# GTK setup for debugger
MONOFONT="Monospace 10"
SANSFONT="Sans 10"
if test "$MACOS" = 1; then
  MONOFONT="Monospace 14"
  SANSFONT="Lucida Grande 14"
fi
AC_SUBST(MONOFONT)
AC_SUBST(SANSFONT)
AC_SUBST(RESOURCES, "blank.png breakpoint.png continue.png here.png \
oberon.lang pause.png sober.xml stepinto.png stepout.png stepover.png \
restart.png exit.png")

if test "$WINDOWS" = 1; then
  OCAMLDIR='"/cygdrive/c/Program Files/Objective Caml/lib"'
  DEB_LIBS="$LIBS -lws2_32"
else
  OCAMLDIR=`ocamlc -where`
  BASE_LIBS=$LIBS
  if test "$ENABLE_DEBUGGER" = 1; then
      AC_SEARCH_LIBS(connect, socket)
  fi
  DEB_LIBS=$LIBS
  LIBS=$BASE_LIBS
fi
AC_SUBST(OCAMLDIR)
AC_SUBST(DEB_LIBS)


# Module list

# Standard modules
AC_SUBST(STDMODS, "Files Conv In Out Err Args Bit Timer \
Strings Random Math MathL")

AC_SUBST(STDMODS_B, "Files Conv In-b Out Err Args Bit Timer \
Strings-b Random-b Math MathL")

# Modules that contain runtime checks -- recompile with -b
AC_SUBST(CHECKMODS, "In Random Strings")

# Modules containing C code
AC_SUBST(PRIMMODS, "Args Timer Files Conv Random")

# Modules with primitives in separate shared libraries:
AC_SUBST(SHLMODS, "$XYPLANE")

# All library modules
AC_SUBST(ALLMODS, "$STDMODS $SHLMODS")

# The endgame
AC_OUTPUT
