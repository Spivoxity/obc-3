/* config.h.  Generated from config.h.in by configure.  */
/* config.h.in.  Generated from configure.in by autoheader.  */

/* Define if building universal (internal helper macro) */
/* #undef AC_APPLE_UNIVERSAL_BUILD */

/* Define to set up for debugging */
/* #undef DEBUG */

/* Directory separator in file names */
#define DIRSEP "/"

/* Extension for dynamic libraries */
#define DLEXT ".so"

/* Define if dynamic linking enabled */
/* #undef DYNLINK */

/* Define to 1 if you have the `clock' function. */
#define HAVE_CLOCK 1

/* Define to 1 if you don't have `vprintf' but do have `_doprnt.' */
/* #undef HAVE_DOPRNT */

/* Define to 1 if you have the <ffi.h> header file. */
#define HAVE_FFI_H 1

/* Define to 1 if you have the `getopt_long_only' function. */
#define HAVE_GETOPT_LONG_ONLY 1

/* Define to 1 if you have the `getpagesize' function. */
#define HAVE_GETPAGESIZE 1

/* Define to 1 if you have the `gettimeofday' function. */
#define HAVE_GETTIMEOFDAY 1

/* Define if indexed jumps work. */
#define HAVE_INDEXED_JUMPS 1

/* Define to 1 if you have the <inttypes.h> header file. */
#define HAVE_INTTYPES_H 1

/* Define to 1 if you have the <memory.h> header file. */
#define HAVE_MEMORY_H 1

/* Define to 1 if you have the `mmap' function. */
#define HAVE_MMAP 1

/* Define to 1 if you have the `sigprocmask' function. */
#define HAVE_SIGPROCMASK 1

/* Define to 1 if you have the <stdint.h> header file. */
#define HAVE_STDINT_H 1

/* Define to 1 if you have the <stdlib.h> header file. */
#define HAVE_STDLIB_H 1

/* Define to 1 if you have the `stpcpy' function. */
#define HAVE_STPCPY 1

/* Define to 1 if you have the <strings.h> header file. */
#define HAVE_STRINGS_H 1

/* Define to 1 if you have the <string.h> header file. */
#define HAVE_STRING_H 1

/* Define to 1 if you have the `strtoul' function. */
#define HAVE_STRTOUL 1

/* Define to 1 if you have the <sys/stat.h> header file. */
#define HAVE_SYS_STAT_H 1

/* Define to 1 if you have the <sys/time.h> header file. */
#define HAVE_SYS_TIME_H 1

/* Define to 1 if you have the <sys/types.h> header file. */
#define HAVE_SYS_TYPES_H 1

/* Define to 1 if you have the `time' function. */
#define HAVE_TIME 1

/* Define to 1 if you have the <time.h> header file. */
#define HAVE_TIME_H 1

/* Define to 1 if you have the <unistd.h> header file. */
#define HAVE_UNISTD_H 1

/* Define to 1 if you have the `vprintf' function. */
#define HAVE_VPRINTF 1

/* Define for Linux */
#define LINUX 1

/* Log2 of page size */
#define LOG_PAGESIZE 12

/* Define for 64-bit runtime with 32-bit pointers */
#define M64X32 1

/* Define to enable hacks for MacOS X */
/* #undef MACOS */

/* Magic number for trailer */
#define MAGIC "OBCX"

/* Reboot FPU for each primitive */
#define NEED_FPINIT 1

/* Attribute for functions that don't return */
#define NORETURN __attribute__ ((noreturn))

/* Define to the address where bug reports for this package should be sent. */
#define PACKAGE_BUGREPORT "mike@cs.ox.ac.uk"

/* Define to the full name of this package. */
#define PACKAGE_NAME "obc"

/* Define to the full name and version of this package. */
#define PACKAGE_STRING "obc 3.1alpha"

/* Define to the one symbol short name of this package. */
#define PACKAGE_TARNAME "obc"

/* Bug tracker */
#define PACKAGE_TRACKER "https://bitbucket.org/Spivey/obc-3/issues"

/* Define to the home page for this package. */
#define PACKAGE_URL ""

/* Define to the version of this package. */
#define PACKAGE_VERSION "3.1alpha"

/* Page size */
#define PAGESIZE 4096

/* Version signature for symbol tables */
#define SIG 0x00030190

/* Define to 1 if you have the ANSI C header files. */
#define STDC_HEADERS 1

/* Define if unaligned loads and stores work */
#define UNALIGNED_MEM 1

/* Attribute for unused variables */
#define UNUSED __attribute__ ((unused))

/* Attribute for used variables */
#define USED __attribute__ ((used))

/* Define if FFI enabled */
/* #undef USEFFI */

/* Whether to flush the cache */
/* #undef USE_FLUSH */

/* Whether to use inet sockets for the debugger */
/* #undef USE_INET */

/* Define to enable hacks for Windows */
/* #undef WINDOWS */

/* Define WORDS_BIGENDIAN to 1 if your processor stores words with the most
   significant byte first (like Motorola and SPARC, unlike Intel). */
#if defined AC_APPLE_UNIVERSAL_BUILD
# if defined __BIG_ENDIAN__
#  define WORDS_BIGENDIAN 1
# endif
#else
# ifndef WORDS_BIGENDIAN
/* #  undef WORDS_BIGENDIAN */
# endif
#endif

/* Define to 1 if the X Window System is missing or not being used. */
/* #undef X_DISPLAY_MISSING */

/* Define to empty if `const' does not conform to ANSI C. */
/* #undef const */

/* Define to `__inline__' or `__inline' if that's what the C compiler
   calls it, or to nothing if 'inline' is not supported under any name.  */
#ifndef __cplusplus
/* #undef inline */
#endif

#define GCOV 1
