/*
 * debprims.c
 * 
 * This file is part of the Oxford Oberon-2 compiler
 * Copyright (c) 2008 J. M. Spivey
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

#include "config.h"
#include <caml/mlvalues.h>
#include <assert.h>

#ifdef WINDOWS
#include <windows.h>
#else
#include <sys/types.h>
#include <signal.h>
#endif

CAMLprim value ml_wake_child(value pid) {
#ifdef WINDOWS
     GenerateConsoleCtrlEvent(CTRL_C_EVENT, 0);
#else
     kill(Int_val(pid), SIGINT);
#endif
     return Val_unit;
}

#include <gdk/gdk.h>
#include "../lablgtk3/wrappers.h"

CAMLprim value ml_rgba_parse(value arg) {
     const char *spec = String_val(arg);
     GdkRGBA rgba;
     assert(gdk_rgba_parse(&rgba, spec));
     return Val_copy(rgba);
}

#ifdef MACOS
#include <glib.h>
#include <glib-object.h>
#include <gtkosxapplication.h>

#include "ml_glib.h"
#include "ml_gobject.h"
#include "ml_gtk.h"
#endif

CAMLprim value ml_gtk_set_platform_menubar(value x, value y)
{
#ifdef MACOS
    GtkMenuShell *menubar = GTK_MENU_SHELL(GtkWidget_val(x));
    GtkMenuItem *about = GTK_MENU_ITEM(GtkWidget_val(y));
    GtkosxApplication *app = g_object_new(GTKOSX_TYPE_APPLICATION, NULL); 

    gtk_widget_hide((GtkWidget *) menubar);
    gtkosx_application_set_menu_bar(app, menubar);
    gtkosx_application_insert_app_menu_item(app, (GtkWidget *) about, 0);
    gtkosx_application_ready(app);
#endif

    return Val_unit;
}
