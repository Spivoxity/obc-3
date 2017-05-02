/*
 * debug.c
 * 
 * This file is part of the Oxford Oberon-2 compiler
 * Copyright (c) 2006--2016 J. M. Spivey
 * All rights reserved
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
 * OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
 * OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 * ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#define OBXDEB 1
#include "obx.h"
#include "keiko.h"
#include <ctype.h>

#ifdef WINDOWS
  #include <winsock2.h>
  #include <windows.h>
#else
  #include <signal.h>
  #include <sys/types.h>
  #include <sys/socket.h>
  #ifdef USE_INET
    #include <netinet/in.h>
    #include <netinet/tcp.h>
    #include <arpa/inet.h>
  #else
    #include <sys/un.h>
  #endif
#endif

static int deb_sock;

#define SPACE 32768

static mybool running = FALSE;

static void debug_exit(void);

/* The socket stuff is easy to do on Unix, where sockets are file
   descriptors that can be wrapped in a stdio channel.  But Windows
   makes things difficult ... */

static void debug_va_message(char *fmt, va_list va) {
     char buf[256];
     fflush(stdout);
     vsprintf(buf, fmt, va);
     strcat(buf, "\n");
     send(deb_sock, buf, strlen(buf), 0);
}

/* debug_message -- send message to debugger */
void debug_message(char *fmt, ...) {
     va_list va;
     va_start(va, fmt);
     debug_va_message(fmt, va);
     va_end(va);
}

/* debug_command -- receive command from debugger */
char *debug_command(void) {
     static char buf[256];
     static int start = 0, len = 0;
     char *mark;
     int nread;

     /* buf[start..len) contains characters left over from the previous
	command. Usually there will be none, so speed doesn't matter. */
     if (start < len)
	  memcpy(buf, buf+start, len-start);
     len -=start; start = 0;

     for (;;) {
	  buf[len] = '\0';
	  mark = strchr(buf, '\n');
	  if (mark != NULL) {
	       /* Return a complete command */
	       start = mark-buf+1;
	       return buf;
	  }
	  
	  nread = recv(deb_sock, buf+len, 256-len, 0);

	  if (nread <= 0)
	       /* Connection has been closed */
	       return NULL;

	  len += nread;
     }
}

#define MAXWORDS 16

static int split_line(char *line, char **words) {
     int nwords = 0;
     char *s;

     s = line; 
     while (*s == ' ' || *s == '\t' || *s == '\r') s++;
     if (*s == '\n' || *s == '!' || *s == '\0') return 0;

     /* Set the words array */
     while (1) {
	  while (*s == ' ' || *s == '\t' || *s == '\r') s++;
	  if (*s == '\0') panic("line too long");
	  if (*s == '\n') break;
	  if (nwords == MAXWORDS) panic("too many words");
	  words[nwords++] = s;
	  while (! isspace((int) *s) && *s != '\0') s++;
	  if (*s == '\n') { *s = '\0'; break; }
	  if (*s == '\0') panic("line too long");
	  *s++ = '\0';
     }

     return nwords;
}

mybool one_shot = FALSE;
mybool intflag = FALSE;

/* debug_break -- conduct breakpoint dialogue */
void debug_break(value *cp, value *bp, uchar *pc, char *fmt, ...) {
     char *cmd;
     char *arg[MAXWORDS];
     int nargs;
     va_list va;

     running = FALSE;
     one_shot = FALSE;

     /* %p may print NULL as "(nil)", so use %#x instead. */
     debug_message("regs %#x %#x %#x",
		   address(cp), address(bp), address(pc));

     va_start(va, fmt);
     debug_va_message(fmt, va);
     va_end(va);

     for (;;) {
	  cmd = debug_command();
	  if (cmd == NULL) debug_exit();

	  nargs = split_line(cmd, arg);

	  if (nargs == 0) continue;

#define cmd(name, n) else if (strcmp(arg[0], name) == 0 && nargs == n)
#define intarg(i) strtoul(arg[i], NULL, 0)

	  cmd("run", 1) break;

	  cmd("step", 1) {
	       one_shot = TRUE;
	       break;
	  }

	  cmd("quit", 1) debug_exit();

	  cmd("peek1", 2) {
	       unsigned addr = intarg(1);
	       debug_message("value %#x", *ptrcast(uchar, addr));
	  }

	  cmd("peek2", 2) {
	       unsigned addr = intarg(1);
	       debug_message("value %#x", *ptrcast(unsigned short, addr));
	  }

	  cmd("peek4", 2) {
	       unsigned addr = intarg(1);
	       debug_message("value %#x", *ptrcast(unsigned, addr));
	  }

	  cmd("breakpt", 3) {
	       uchar *addr = ptrcast(uchar, intarg(1));
	       if (*addr != K_LNUM_2 && *addr != K_BREAK_2)
		    panic("*breakpoint not at line boundary");
	       *addr = (intarg(2) ? K_BREAK_2 : K_LNUM_2);
	  }

	  else panic("*unknown command %s(%d)", arg[0], nargs);
     }

     running = TRUE;
     intflag = FALSE;
}

#ifdef WINDOWS

/* debug_connect -- establish socket connection */
static void debug_connect(char *sockname) {
     int sock;
     WSADATA wsaData;
     struct sockaddr_in addr;
     int flag = 1;

     if (WSAStartup(MAKEWORD(2, 2), &wsaData) != NO_ERROR) {
	  fprintf(stderr, "WSAStartup() failed\n");
	  exit(1);
     }

     sock = socket(PF_INET, SOCK_STREAM, 0);
     if (sock == INVALID_SOCKET) { 
	  fprintf(stderr, "socket() failed: %d\n", WSAGetLastError());
	  exit(1);
     }

     setsockopt(sock, IPPROTO_TCP, TCP_NODELAY, (char *) &flag, sizeof(int));
     addr.sin_family = AF_INET;
     addr.sin_port = htons(atoi(sockname));
     addr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);

     if (connect(sock, (struct sockaddr *) &addr, sizeof(addr)) 
	 == SOCKET_ERROR) {
	  fprintf(stderr, "connect() failed: %d\n", WSAGetLastError());
	  exit(1);
     }

     deb_sock = sock;
}

/* ctrl_handler -- handler for console interrupt */
static BOOL CALLBACK ctrl_handler(DWORD event) {
     if (event != CTRL_C_EVENT && event != CTRL_BREAK_EVENT) return FALSE;
     if (running) intflag = TRUE;
     return TRUE;
}

#else

/* debug_connect -- establish socket connection */
static void debug_connect(char *sockname) {
     int sock;
#ifdef USE_INET
     struct sockaddr_in addr;
     int flag = 1;

     sock = socket(PF_INET, SOCK_STREAM, 0);
     if (sock < 0) { perror("socket"); exit(1); }
     setsockopt(sock, IPPROTO_TCP, TCP_NODELAY, &flag, sizeof(int));
     addr.sin_family = AF_INET;
     addr.sin_port = htons(atoi(sockname));
     addr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
#else
     struct sockaddr_un addr;

     sock = socket(PF_UNIX, SOCK_STREAM, 0);
     if (sock < 0) { perror("socket"); exit(1); }
     addr.sun_family = AF_UNIX;
     strcpy(addr.sun_path, sockname);
#endif

     if (connect(sock, (struct sockaddr *) &addr, sizeof(addr)) < 0) {
	  perror("connect"); exit(1);
     }

     deb_sock = sock;
}

/* intr_handler -- handler for interrupt signal */
static void intr_handler(int sig) {
     signal(sig, intr_handler);

     if (!running) return;

     if (prim_bp == NULL)
	  intflag = TRUE;
     else {
	  value *cp = valptr(prim_bp[CP]);
	  debug_break(cp, prim_bp, NULL, "interrupt");
     }
}
#endif

/* debug_init -- initialise conversation with debugger */
void debug_init(void) {
     if (debug_socket == NULL) panic("unknown socket");
     debug_connect(debug_socket);

#ifdef WINDOWS
     SetConsoleCtrlHandler(ctrl_handler, TRUE);
#else
     signal(SIGINT, intr_handler);
#endif

     debug_message("hello %s", PACKAGE_VERSION);
     running = TRUE;
}

/* debug_exit -- shut down and exit */
static void debug_exit(void) {
#ifdef WINDOWS
     WSACleanup();
#endif
     exit(0);
}
