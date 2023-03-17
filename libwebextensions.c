// SPDX-FileCopyrightText: Atlas Engineer LLC
// SPDX-License-Identifier: BSD-3-Clause

#include <glib.h>
#include <webkit2/webkit-web-extension.h>

int initialize_lisp(int argc, char *argv[], char *envp[]);
void on_init (void *extension);
void on_page_created (void *extension, void *page, void *user_data);
void on_message (void *page, void *message, void *user_data);

/** user_message_received_callback
 *
 * A callback for "user-message-received" signal.
 *
 * Merely calls on-message Lisp function with page, message, and
 * user-data as args.
 */
static gboolean
user_message_received_callback (WebKitWebPage     *web_page,
                                WebKitUserMessage *message,
                                gpointer           user_data)
{
        on_message(web_page, message, user_data);
}

/** web_page_created_callback
 *
 * A callback for "page-created" signal.
 *
 * Calls Lisp function on-page-created with the extension, page, and
 * user-data objects as arguments.
 *
 * Add a "user-message-received" signal callback.
 */
static void
web_page_created_callback (WebKitWebExtension *extension,
                           WebKitWebPage      *web_page,
                           gpointer            user_data)
{
        on_page_created(extension, web_page, user_data);
        g_signal_connect (web_page, "user-message-received",
                          G_CALLBACK (user_message_received_callback),
                          NULL);
}

/** webkit_web_extension_initialize_with_user_data
 *
 * A usual entry point for any WebKit extension. Calls Lisp function
 * on-init (with the extension as the sole argument) and attaches
 * several signal handlers.
 *
 * Print "The WebExtensions support library is loading" before loading.
 * Print "The WebExtensions support library is loaded" when all the
 * handlers are set up.
 */
G_MODULE_EXPORT void
webkit_web_extension_initialize (WebKitWebExtension *extension)
{
        initialize_lisp(4, (char*[]) {"", "--core", "libwebextensions.core", "--noinform"},
                        (char**) NULL);
        on_init(extension);
        g_print("The WebExtensions support library is loading\n");
        g_signal_connect (extension, "page-created",
                          G_CALLBACK (web_page_created_callback),
                          NULL);
}
