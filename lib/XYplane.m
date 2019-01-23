(*
 * XYplane.m
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
 *)

(** Very simple black-and-white graphics for X windows *)
MODULE XYplane;

IMPORT SYSTEM;

CONST 
  (** erase -- bit value to write in white *)
  erase* = 0; 
  (** draw -- bit value to write in black *)
  draw* = 1;
  (** W -- width of the graphics window *)
  W* = 640; 
  (** H -- height of the graphics window *)
  H* = 480;

(** Clear -- clear the window to all white *)
PROCEDURE Clear* IS "XYplane_clear";

(** Dot -- draw a dot at coordinates (x, y) *)
PROCEDURE Dot*(x, y, mode: INTEGER) IS "XYplane_draw";

(** IsDot -- return the current colour of the dot at (x, y) *)
PROCEDURE IsDot*(x, y: INTEGER): BOOLEAN IS "XYplane_isdot";

(** Key -- return the last key pressed, or 0X if none *)
PROCEDURE Key*(): CHAR IS "XYplane_key";

(** Open -- create the graphics window *)
PROCEDURE Open* IS "XYplane_open";

BEGIN
  SYSTEM.LOADLIB("@XYplane");
END XYplane.

--CODE--

#include "obx.h"
#include <stdlib.h>
#include <string.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

#define W 640
#define H 480
#define NBITS 32

static Display *display = NULL;
static int screen;
static Window window;
static GC fg, bg;
static XImage *image;
static int *bitmap;

static void fatal(const char *msg) {
     fprintf(stderr, "Fatal error in XYplane: %s\n", msg);
     exit(2);
}

void XYplane_clear(void) {
     if (display != NULL) {
	  XFillRectangle(display, window, bg, 0, 0, W, H);
	  memset(bitmap, 0, H * ((W+NBITS-1)/NBITS) * sizeof(int));
     }
}

void XYplane_draw(int x, int y, int m) {
     if (display != NULL && x >= 0 && x < W && y >= 0 && y < H) {
	  XPutPixel(image, x, H-y-1, m);
	  XDrawPoint(display, window, (m == 1 ? fg : bg), x, H-y-1);
     }
}

int XYplane_isdot(int x, int y) {
     if (display == NULL || x < 0 || x >= W || y < 0 || y >= H) 
	  return 0;
     else
	  return XGetPixel(image, x, H-y-1);
}

char XYplane_key() {
     XEvent event;
     char buf[16];
     KeySym keysym;
     int n, x, y ,w, h;

     if (display == NULL) return '\0';

     XFlush(display);

     while (XEventsQueued(display, QueuedAfterReading) > 0) {
	  XNextEvent(display, &event);
	  switch (event.type) {
	  case KeyPress:
	       n = XLookupString(&(event.xkey), buf, 16, &keysym, NULL);
	       if (n > 0) return buf[0];
	       break;

	  case Expose:
	       x = event.xexpose.x; y = event.xexpose.y;
	       w = event.xexpose.width; h = event.xexpose.height;
	       if (x+w > W) w = W-x;
	       if (y+h > H) h = H-y;
	       if (w >= 0 && h >= 0)
		    XPutImage(display, window, fg, image, x, y, x, y, w, h);
	       break;
	  }
     }

     return '\0';
}

void XYplane_open(void) {
     Visual *visual;
     XGCValues values;
     XEvent event;
     Window parent;
     int fgColour, bgColour;

     /* Opening a second time is the same as clearing */
     if (display != NULL) {
	  XYplane_clear(); return;
     }

     display = XOpenDisplay(NULL);
     if (display == NULL) fatal("couldn't open display");

     screen = XDefaultScreen(display);
     parent = XRootWindow(display, screen);
     visual = XDefaultVisual(display, screen); 
     fgColour = XBlackPixel(display, screen);
     bgColour = XWhitePixel(display, screen);
     window = XCreateSimpleWindow(display, parent, 0, 0, W, H, 0, 0, bgColour);
     XStoreName(display, window, "XYplane");
     XSelectInput(display, window, KeyPressMask|ExposureMask);
     XMapWindow(display, window);
     XFlush(display);

     bitmap = (int * ) calloc(H * ((W+NBITS-1)/NBITS), sizeof(int));
     if (bitmap == NULL) fatal("couldn't allocate bitmap");
     image = XCreateImage(display, visual, 1, XYBitmap, 0, (char * ) bitmap, 
		          W, H, NBITS, 0);
     XMaskEvent(display, ExposureMask, &event);

     values.foreground = fgColour;
     values.background = bgColour;
     fg = XCreateGC(display, parent, GCForeground|GCBackground, &values);
     values.foreground = bgColour;
     values.background = fgColour;
     bg = XCreateGC(display, parent, GCForeground|GCBackground, &values);
}

