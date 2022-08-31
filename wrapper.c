/* SPDX-FileCopyrightText: Atlas Engineer LLC
 * SPDX-License-Identifier: BSD-3-Clause */

#include <glib-2.0/glib.h>
#include <webkit2/webkit-web-extension.h>
#include <libguile.h>

G_MODULE_EXPORT void
webkit_web_extension_initialize (WebKitWebExtension *extension)
{
        scm_init_guile();
        char *code = "(define-module (webkit-webextensions)\n  #:use-module (system foreign)\n  #:use-module (system foreign-library)\n  #:export (entry-webextensions))\n\n;; Implies that the .so this is called from is linked against WebKit.\n(define lib (load-foreign-library #f #:search-system-paths? #t))\n\n;; (define (g-signal-connect instance signal handler)\n;;   ((foreign-library-function lib \"g_signal_connect_data\"\n;;                              #:arg-types (list '* '* '* '* '* int)\n;;                              #:return-type '*)\n;;    instance\n;;    (if (pointer? signal)\n;;        signal\n;;        (string->pointer signal))\n;;    handler\n;;    %null-pointer %null-pointer 0))\n\n(define jsc-make-context\n  (foreign-library-function lib \"jsc_context_new\" #:return-type '*))\n\n(define (jsc-make-null context)\n  ((foreign-library-function lib \"jsc_value_new_null\"\n                             #:return-type '*\n                             #:arg-types (list '*))\n   context))\n\n;; (define (jsc-null? arg)\n;;   (= 1 ((foreign-library-function lib \"jsc_value_is_null\"\n;;                                   #:return-type unsigned-int\n;;                                   #:arg-types (list '*))\n;;         arg)))\n\n;; (define (json->jsc json #:optional (context (make-jsc-context)))\n;;   ((foreign-library-function lib \"jsc_value_new_from_json\"\n;;                              #:return-type '*\n;;                              #:arg-types (list '* '*))\n;;    context (if (pointer? json)\n;;                json\n;;                (string->pointer json))))\n\n(define (jsc->json jsc-value)\n  (pointer->string\n   ((foreign-library-function lib \"jsc_value_to_json\"\n                              #:return-type '*\n                              #:arg-types (list '* int))\n    jsc-value 0)))\n\n;; (define (jsc->string jsc-value)\n;;   (pointer->string\n;;    ((foreign-library-function lib \"jsc_value_to_string\"\n;;                               #:return-type '*\n;;                               #:arg-types (list '*)) jsc-value)))\n\n(define (entry-webextensions)\n  (display (jsc->json (jsc-make-null (jsc-make-context))))\n  (display \"\\n\"))\n";
        scm_eval_string(scm_from_locale_string(code));
        scm_call_0(scm_c_private_ref("webkit-webextensions", "entry-webextensions"));
        g_print("Loaded!");
}
