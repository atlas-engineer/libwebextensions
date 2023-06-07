;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(define-module (webkit-webextensions)
  #:use-module (system vm program)
  #:use-module (system foreign)
  #:use-module (system foreign-library)
  #:use-module (srfi srfi-1)
  #:export (entry-webextensions))

;; When developing, try:
;; (define lib (load-foreign-library "/gnu/store/9hijxiihm6l9260wmjsnk6qndh5asdf6-webkitgtk-2.38.5/lib/libwebkit2gtk-4.1.so"))
(define lib #f)

;;; General utilities (Glib and FFI)

(define (pointer/false pointer)
  "Return #f is the POINTER is NULL.
Otherwise return the POINTER itself.

Useful to dispatch NULL/non-NULL pointers on the Scheme-side."
  (if (eq? %null-pointer pointer)
      #f
      pointer))

(define (string->pointer* string)
  "Smarter string->pointer.
Converts string to pointers and leaves pointers intact."
  (cond
   ((string? string)
    (string->pointer string))
   ((pointer? string)
    string)
   (else (error "Cannot ensure string pointer for value" string))))

(define (pointer->string* pointer)
  "Smarter pointer->string.
Turns null pointers into #f, instead of erroring."
  (let ((pointer (pointer/false pointer)))
    (when pointer
      (pointer->string pointer))))

(define* (procedure->pointer*
          procedure
          #:optional (arg-types (when (procedure? procedure)
                                  (make-list (car (procedure-minimum-arity procedure)) '*)))
          (return-type '*))
  "Smarter procedure->pointer.
Converts procedures to pointers and leaves pointers intact.
Also defaults ARG-TYPES and RETURN-TYPE to pointers. In case of
ARG-TYPES tries to guess the PROCEDURE arity and generate a reasonable
arglist."
  (cond
   ((and (procedure? procedure)
         (not arg-types))
    (error "Cannot ensure procedure pointer without arg types"))
   ((procedure? procedure)
    (procedure->pointer return-type procedure arg-types))
   ((pointer? procedure)
    procedure)
   (else (error "Cannot ensure procedure pointer for value" procedure))))

(define (foreign-fn name args return-type)
  "Wrapper around `foreign-library-function' for ease of throwaway C calls."
  (foreign-library-function
   lib name
   #:return-type return-type
   #:arg-types args))

(define* (g-print format-string . args)
  "Print values using Glib primitives."
  (apply (pointer->procedure void
                             (foreign-library-pointer lib "g_print")
                             (cons '*
                                   (map (lambda (a)
                                          (cond
                                           ((or (string? a)
                                                (pointer? a))
                                            '*)
                                           ((number? a)
                                            int64)))
                                        args)))
         (string->pointer format-string)
         (map (lambda (a) (if (string? a)
                              (string->pointer a)
                              a))
              args)))

(define (make-g-variant string-or-nothing)
  "Create and return a new maybe string (ms) GVariant."
  ((foreign-fn "g_variant_new" '(* *) '*)
   (string->pointer "ms")
   (if string-or-nothing
       (string->pointer* string-or-nothing)
       %null-pointer)))

(define (g-variant-string g-variant)
  "Fetch the G-VARIANT string, if there's one.
G-VARIANT is implied to be a maybe string GVariant."
  (let* ((maybe (when (pointer/false g-variant)
                  (pointer/false
                   ((foreign-fn "g_variant_get_maybe" '(*) '*) g-variant)))))
    (when maybe
      (pointer->string
       ((foreign-fn "g_variant_get_string" '(*) '*) maybe)))))

(define* (g-signal-connect instance signal handler #:optional (data #f))
  "Connect HANDLER (pointer to procedure) to SIGNAL of INSTANCE."
  ((foreign-fn "g_signal_connect_data" (list '* '* '* '* '* int) '*)
   instance
   (string->pointer* signal)
   handler (or data %null-pointer) %null-pointer 0))

(define (make-g-async-callback procedure)
  "Turn PROCEDURE into a pointer suitable for GAsyncCallback."
  (procedure->pointer* procedure '(* * *) void))

(define (procedure-maximum-arity procedure)
  (let ((arity (procedure-minimum-arity procedure)))
    (+ (car arity) (cadr arity))))

;;; JSCore bindings

;; JSCContext

(define (jsc-make-context)
  "Create a new empty JSCContext."
  ((foreign-fn "jsc_context_new" '() '*)))

(define (jsc-context jsc)
  "Get the context of JSC value."
  (pointer/false ((foreign-fn "jsc_value_get_context" '(*) '*) jsc)))

(define (jsc-context-current)
  "Get the current context.
Only makes sense in method/function/property callbacks."
  (pointer/false ((foreign-fn "jsc_context_get_current" '() '*))))

(define (jsc-context-get/make)
  "Get the current context, or create it if not present."
  (or (jsc-context-current)
      (jsc-make-context)))

(define* (jsc-context-evaluate code #:optional (context (jsc-context-get/make)))
  "Evaluate CODE in CONTEXT.
Returns raw JSCValue resulting from CODE evaluation."
  ((foreign-fn "jsc_context_evaluate" `(* * ,int) '*)
   context (string->pointer* code) -1))

(define (jsc-context-exception context)
  "Return the last JSCException in CONTEXT."
  (pointer/false ((foreign-fn "jsc_context_get_expression" '(*) '*) context)))

(define* (jsc-context-value name #:optional (context (jsc-context-get/make)))
  "Returns the JSCValue (as a pointer to JSCValue) bound to NAME in CONTEXT."
  ((foreign-fn "jsc_context_get_value" '(* *) '*)
   context (string->pointer* name)))

(define* (jsc-context-register-class context name #:optional (parent-class %null-pointer))
  "Return a class (JSCClass pointer) registered in CONTEXT under NAME.
Inherits from PARENT-CLASS, if any."
  ((foreign-fn "jsc_context_register_class" '(* * * * *) '*)
   context
   (string->pointer* name)
   parent-class
   %null-pointer
   %null-pointer))

;; JSCClass

(define* (jsc-class-add-constructor class name #:optional (callback #f))
  "Add a constructor to CLASS with CALLBACK called on object initialization.
If NAME is #f, use CLASS name.

CALLBACK is generated with JSCValue arguments and JSCValue return
type. Using the underlying jsc_class_add_constructor is better for
cases where specifying other GTypes makes more sense.

When CALLBACK is not provided, it's implied to be a zero-argument
function doing nothing."
  (let ((jsc-type ((foreign-fn "jsc_value_get_type" '() '*)))
        (number-of-args (if callback
                            (procedure-maximum-arity callback)
                            0)))
    (apply
     (foreign-fn "jsc_class_add_constructor"
                 (append `(* * * * * * ,unsigned-int)
                         (make-list number-of-args '*))
                 '*)
     class
     (string->pointer* name)
     (procedure->pointer* callback (make-list number-of-args '*))
     %null-pointer
     %null-pointer
     jsc-type
     number-of-args
     (make-list number-of-args jsc-type))))

(define* (jsc-class-add-method class name callback)
  "Add a NAMEd method to CLASS object.

CALLBACK should be a function with minimum one argument—the instance
of CLASS. Keyword/rest arguments are not supported."
  (let ((jsc-type ((foreign-fn "jsc_value_get_type" '() '*)))
        (number-of-args (procedure-maximum-arity callback)))
    (apply
     (foreign-fn "jsc_class_add_method"
                 (append `(* * * * * * ,unsigned-int)
                         (make-list number-of-args '*))
                 '*)
     class
     (string->pointer* name)
     (procedure->pointer* callback (make-list number-of-args '*))
     %null-pointer
     %null-pointer
     jsc-type
     number-of-args
     (make-list number-of-args jsc-type))))

(define* (jsc-class-add-property class name getter-callback setter-callback)
  "Add a NAME property to JSCClass CLASS."
  ((foreign-fn "jsc_class_add_property"
               `(* * * * * * * *)
               '*)
   class
   (string->pointer* name)
   ((foreign-fn "jsc_value_get_type" '() '*))
   (procedure->pointer* getter-callback)
   (procedure->pointer* setter-callback)
   %null-pointer
   %null-pointer))

(define (jsc-class-name class)
  "Returns string name of CLASS."
  (pointer->string* ((foreign-fn "jsc_class_get_name" '(*) '*) class)))
(define (jsc-class-parent class)
  "Returns raw JSCClass parent on CLASS."
  ((foreign-fn "jsc_class_get_parent" '(*) '*) class))

;; JSCValue

(define (jsc-value-context value)
  "Get context of the VALUE."
  ((foreign-fn "jsc_value_get_context" '(*) '*) value))

;; NOTE: Don't use undefined when passing objects to/from browser:
;; JSON doesn't support undefined!
(define* (jsc-make-undefined #:optional (context (jsc-context-get/make)))
  ((foreign-fn "jsc_value_new_undefined" '(*) '*) context))
(define (jsc-undefined? jsc)
  (positive? ((foreign-fn "jsc_value_is_undefined" '(*) unsigned-int) jsc)))

(define* (jsc-make-null #:optional (context (jsc-context-get/make)))
  ((foreign-fn "jsc_value_new_null" '(*) '*) context))
(define (jsc-null? jsc)
  (positive? ((foreign-fn "jsc_value_is_null" '(*) unsigned-int) jsc)))

(define* (jsc-make-number num #:optional (context (jsc-context-get/make)))
  ;; Don't call it with complex numbers!!!
  (if (or (and (complex? num)
               (positive? (imag-part num)))
          (and (rational? num)
               (> (denominator num) 1)))
      (error "Cannot create JSC number out of complex/ratio number:" num)
      ((foreign-fn "jsc_value_new_number" (list '* double) '*) context num)))
(define (jsc-number? jsc)
  (positive? ((foreign-fn "jsc_value_is_number" '(*) unsigned-int) jsc)))
(define (jsc->number jsc)
  (let ((double ((foreign-fn "jsc_value_to_double" '(*) double) jsc)))
    (if (integer? double)
        ((foreign-fn "jsc_value_to_int32" '(*) int32) jsc)
        double)))

(define* (jsc-make-boolean value #:optional (context (jsc-context-get/make)))
  ((foreign-fn "jsc_value_new_boolean" (list '* unsigned-int) '*)
   context (if value 1 0)))
(define (jsc-boolean? jsc)
  (positive? ((foreign-fn "jsc_value_is_boolean" '(*) unsigned-int) jsc)))
(define (jsc->boolean jsc)
  (positive? ((foreign-fn "jsc_value_to_boolean" '(*) unsigned-int) jsc)))

(define* (jsc-make-string str #:optional (context (jsc-context-get/make)))
  ((foreign-fn "jsc_value_new_string" (list '* '*) '*)
   context (string->pointer* str)))
(define (jsc-string? jsc)
  (positive? ((foreign-fn "jsc_value_is_string" '(*) unsigned-int) jsc)))
(define (jsc->string jsc)
  (pointer->string*
   ((foreign-fn "jsc_value_to_string" (list '*) '*) jsc)))

(define (jsc-property object property-name)
  ((foreign-fn "jsc_value_object_get_property" '(* *) '*)
   object (string->pointer* property-name)))
(define (jsc-property? object property-name)
  ((foreign-fn "jsc_value_object_has_property" '(* *) '*)
   object (string->pointer* property-name)))
(define (jsc-property-set! object property-name value)
  ((foreign-fn "jsc_value_object_set_property" '(* * *) void)
   object (string->pointer* property-name)
   value))
(define (jsc-property-delete! object property-name)
  ((foreign-fn "jsc_value_object_set_property" '(* *) void)
   object (string->pointer* property-name)))

(define (jsc-array? jsc)
  (positive? ((foreign-fn "jsc_value_is_array" '(*) unsigned-int))))

(define (jsc-object? obj)
  (positive? ((foreign-fn "jsc_value_is_object" '(*) unsigned-int) obj)))
(define (jsc-instance-of? obj parent-name)
  (positive? ((foreign-fn "jsc_value_object_is_instance_of" '(* *) unsigned-int)
              obj (string->pointer* parent-name))))

(define* (jsc-make-function name callback #:optional (context (jsc-context-get/make)))
  "Create a function with CALLBACK and bind it to NAME.
If NAME is #f, create an anonymous function."
  (let ((jsc-type ((foreign-fn "jsc_value_get_type" '() '*)))
        (number-of-args (procedure-maximum-arity callback)))
    (apply
     (foreign-fn "jsc_value_new_function"
                 (append `(* * * * * * ,unsigned-int)
                         (make-list number-of-args '*))
                 '*)
     context
     (if name
         (string->pointer* name)
         %null-pointer)
     (procedure->pointer* callback (make-list number-of-args '*))
     %null-pointer
     %null-pointer
     jsc-type
     number-of-args
     (make-list number-of-args jsc-type))))
(define (jsc-function? obj)
  (positive? ((foreign-fn "jsc_value_is_function" '(*) unsigned-int) obj)))

(define (apply-with-args function-name initial-args args)
  "Helper for function application functions.
Applies FUNCTION-NAME to INITIAL-ARGS and ARGS."
  (let ((jsc-type ((foreign-fn "jsc_value_get_type" '() unsigned-int))))
    (apply
     (foreign-fn function-name
                 (append
                  (make-list (length initial-args) '*)
                  (fold (lambda (a l)
                          (append l (list unsigned-int '*)))
                        '()
                        args)
                  (list unsigned-int))
                 '*)
     (append initial-args
             (fold (lambda (a l)
                     (append l (list jsc-type a)))
                   '()
                   args)
             ;; G_TYPE_NONE (hopefully portable)
             (list 4)))))

(define* (jsc-function-call function #:rest args)
  (apply-with-args "jsc_value_function_call" (list function) args))
(define* (jsc-constructor-call constructor #:rest args)
  (apply-with-args "jsc_value_constructor_call" (list constructor) args))
(define* (jsc-object-call-method object name #:rest args)
  (apply-with-args "jsc_value_object_invoke_method" (list object (string->pointer* name)) args))

;; JSC-related conversion utilities.

(define* (scm->jsc object #:optional (context (jsc-context-get/make)))
  "Convert a Scheme OBJECT to JSCValue.
Does not support converting to function.
Converts alists to objects.
Converts vectors and proper lists to arrays."
  (cond
   ((eq? #:null object) (jsc-make-null context))
   ((eq? #:undefined object) (jsc-make-undefined context))
   ((symbol? object) (scm->jsc (symbol->string object)))
   ((keyword? object) (scm->jsc (keyword->symbol object)))
   ((boolean? object) (jsc-make-boolean object context))
   ((number? object) (jsc-make-number object context))
   ((string? object) (jsc-make-string object context))
   ((vector? object) (jsc-make-array object context))
   ;; Dotted alist
   ((and (list? object)
         (not (list? (cdr (car object)))))
    (jsc-make-object %null-pointer object context))
   ((list? object) (jsc-make-array object context))
   (else (error "scm->jsc: unknown value passed" object))))

;; Defining here because it depends on scm->jsc.
(define* (jsc-context-value-set! name value #:optional (context (jsc-context-get/make)))
  "Set the NAMEd value in CONTEXT to a VALUE.
VALUE can be a Scheme value or a pointer to JSCValue."
  ((foreign-fn "jsc_context_set_value" '(* * *) '*)
   context (string->pointer* name)
   (if (pointer? value)
       value
       (scm->jsc value))))

(define* (jsc->scm object)
  "Convert JSCValue OBJECT to Scheme thing.
Does not support objects and functions yet."
  (cond
   ((not (pointer? object))
    (error "jsc->scm: passed non-pointer."))
   ((jsc-null? object) #:null)
   ((jsc-undefined? object) #:undefined)
   ((jsc-boolean? object) (jsc->boolean object))
   ((jsc-string? object) (jsc->string object))
   ((jsc-number? object) (jsc->number object))
   ((jsc-array? object) (jsc->list object))
   ((jsc-object? object) (error "jsc->scm: object conversion not implemented yet"))))

;; Defining here because it depends on jsc->scm.
(define* (jsc-context-evaluate* code #:optional (context (jsc-context-get/make)))
  "Evaluate CODE in CONTEXT, but return Scheme value."
  (jsc->scm (jsc-context-evaluate code context)))

(define* (jsc-make-array list-or-vector #:optional (context (jsc-context-get/make)))
  "Transform LIST-OR-VECTOR to a JSC array.
LIST-OR-VECTOR should be a list or vector, and its elements should be
JSC value pointers."
  (let ((contents (if (vector? list-or-vector)
                      (vector->list list-or-vector)
                      list-or-vector))
        (arr ((foreign-fn "jsc_value_new_array_from_garray" '(* *) '*)
              context %null-pointer)))
    (when (positive? (length contents))
      (do ((idx 0 (1+ idx)))
          ((>= idx (length contents)))
        (let ((value (list-ref contents idx)))
          (jsc-property-set! arr (string->pointer (number->string idx))
                             (if (pointer? value)
                                 value
                                 (scm->jsc value))))))
    arr))

(define (jsc->list object)
  "Convert OBJECT JSCValue array into a Scheme list."
  (let rec ((idx 0))
    (if (zero? ((foreign-fn "jsc_value_object_has_property" '(* *) unsigned-int)
                object (string->pointer (number->string idx))))
        '()
        (cons (jsc->scm (jsc-property object (number->string idx)))
              (rec (1+ idx))))))

(define* (jsc-make-object class contents #:optional (context (jsc-context-get/make)))
  "Create a JSCValue object with CLASS and CONTENTS (alist) inside it.
If CLASS is #f, no class is used."
  (let* ((class (or class %null-pointer))
         (obj ((foreign-fn "jsc_value_new_object" '(* * *) '*)
               context %null-pointer class)))
    (when (positive? (length contents))
      (do ((idx 0 (1+ idx)))
          ((>= idx (length contents)))
        (let ((value (cdr (list-ref contents idx))))
          (jsc-property-set! obj (string->pointer* (car (list-ref contents idx)))
                             (if (pointer? value)
                                 value
                                 (scm->jsc value))))))
    obj))

;; jsc Scheme types: boolean?, pair?, symbol?, number?, char?, string?, vector?, port?, procedure?
;; Guile ones: hash-table? and objects (any predicate for those? record? maybe)

(define* (json->jsc json #:optional (context (jsc-context-get/make)))
  "Parse JSON into proper JSCValue."
  ((foreign-fn "jsc_value_new_from_json" '(* *) '*)
   context (string->pointer* json)))

(define (jsc->json jsc-value)
  "Convert a JSC-VALUE into a JSON string.
BEWARE: undefined is not supported (due to JSON standard excluding it)
and leads to weird behaviors."
  (pointer->string*
   ((foreign-fn "jsc_value_to_json" (list '* int) '*)
    jsc-value 0)))

;;; Threading primitives

(define* (jsc-make-error message #:optional (context (jsc-context-get/make)))
  "Create a JS error with MESSAGE."
  (jsc-constructor-call
   (jsc-context-value "Error" context)
   (scm->jsc message)))

(define *id* 0)
(define (get-id)
  "Create a new ID."
  (let ((id *id*))
    (set! *id* (+ 1 id))
    id))
(define *callback-table* (make-hash-table))

(define* (jsc-make-promise success #:key failure default (context (jsc-context-get/make)))
  "Create a JS promise with SUCCESS running on completion and FAILURE on error.
SUCCESS/FAILURE is determined based on `*callback-table*' data.
When the result fetching timeouts (10 seconds), calls SUCCESS on
DEFAULT."
  (let ((id (get-id)))
    (jsc-constructor-call
     (jsc-context-value "Promise" context)
     (jsc-make-function
      %null-pointer (lambda (suc fail)
                      (let check-result ((attempts 0))
                        (let ((data (hash-ref *callback-table* id)))
                          (cond
                           ((and data (jsc-instance-of? data "Error"))
                            (jsc-function-call failure data))
                           (data
                            (jsc-function-call success data))
                           ((> attempts 10)
                            (jsc-function-call success default))
                           (else
                            (sleep 0.1)
                            (check-result (+ 1 attempts)))))))
      context))))


;;; Webkit extensions API

;; Table from property name to the injection function.
(define apis (make-hash-table))

(define* (define-api property class #:rest methods)
  "Register the WebExtensions JS API.

Puts the API initialization function (with the context as the sole
argument) into the `apis' table.

PROPERTY is the name under which the API is added to browser
object (i.e. \"browser.bookmarks\" for \"bookmarks\" PROPERTY).

CLASS is the name of the class API is generated from.

METHODS is a property list of name+callback for class methods."
  (hash-set!
   apis property
   (lambda (context)
     (let* ((class-obj (jsc-context-register-class context class))
            (constructor (jsc-class-add-constructor class-obj class (lambda () #f))))
       (letrec ((add-methods
                 (lambda (meths)
                   (unless (null? meths)
                     (jsc-class-add-method class-obj (car meths) (cadr meths))
                     (add-methods (cddr meths))))))
         (add-methods methods)
         (jsc-context-value-set! class constructor context)
         (jsc-property-set!
          (jsc-context-value "browser" context)
          property
          (jsc-constructor-call constructor)))))))

;; ContextMenu and ContextMenuItem
(define (make-context-menu)
  ((foreign-fn "webkit_context_menu_new" '() '*)))

(define (context-menu-event menu)
  ((foreign-fn "webkit_context_menu_get_event" '(*) '*) menu))
(define (context-menu-length menu)
  ((foreign-fn "webkit_context_menu_get_n_items" '(*) unsigned-int) menu))

(define (context-menu-append! menu item)
  ((foreign-fn "webkit_context_menu_append" '(* *) void)
   menu item))
(define (context-menu-prepend! menu item)
  ((foreign-fn "webkit_context_menu_prepend" '(* *) void)
   menu item))
(define (context-menu-insert! menu item index)
  ((foreign-fn "webkit_context_menu_insert" `(* * ,unsigned-int) void)
   menu item index))
(define (context-menu-remove! menu item)
  ((foreign-fn "webkit_context_menu_remove" '(* *) void)
   menu item))
(define (context-menu-remove-all! menu)
  ((foreign-fn "webkit_context_menu_remove" '(*) void) menu))
(define (context-menu-move! menu item index)
  ((foreign-fn "webkit_context_menu_move_item" `(* * ,unsigned-int) void)
   menu item index))

(define (context-menu-first menu)
  ((foreign-fn "webkit_context_menu_first" '(*) '*) menu))
(define (context-menu-last menu)
  ((foreign-fn "webkit_context_menu_last" '(*) '*) menu))
(define (context-menu-ref menu index)
  ((foreign-fn "webkit_context_menu_get_item_at_position" `(* ,unsigned-int) '*)
   menu index))

(define (context-menu-item-action item)
  ((foreign-fn "webkit_context_menu_item_get_gaction" `(*) '*) item))
(define (context-menu-item-stock-action item)
  ((foreign-fn "webkit_context_menu_item_get_stock_action" `(*) unsigned-int) item))
(define (context-menu-item-separator? item)
  (positive? ((foreign-fn "webkit_context_menu_item_is_separator" `(*) unsigned-int) item)))
(define (make-context-menu-separator)
  ((foreign-fn "webkit_context_menu_item_new_separator" `() '*)))
(define (make-context-menu-submenu label submenu)
  ((foreign-fn "webkit_context_menu_item_new_with_submenu" `(* *) '*)
   (string->pointer* label) submenu))

(define* (make-context-menu-item label #:optional (action 1000))
  "Make a new context menu item with text LABEL.
ACTION can be:
- An integer (ContextMenuAction) for predefined action.
- Or a procedure on (action parameter), in which case it's set as the
callback for the item.

Defaults to 1000 (WEBKIT_CONTEXT_MENU_ACTION_CUSTOM)."
  (let ((item ((foreign-fn "webkit_context_menu_item_new_from_stock_action_with_label"
                           `(,unsigned-int *) '*)
               (if (procedure? action)
                   action
                   1000)
               (string->pointer* label))))
    (when (procedure? action)
      (g-signal-connect (context-menu-item-action item) "activate" (procedure->pointer* action '(* *) void)))
    item))

;; UserMessage

(define* (make-message name #:optional (params (make-g-variant #f)))
  ((foreign-fn "webkit_user_message_new" '(* *) '*)
   (string->pointer* name)
   (cond
    ((string? params)
     (make-g-variant params))
    ((pointer? params)
     params)
    (else %null-pointer))))

(define (message-name message)
  (pointer->string*
   ((foreign-fn "webkit_user_message_get_name" '(*) '*)
    message)))

(define (message-params message)
  ((foreign-fn "webkit_user_message_get_parameters" '(*) '*) message))

(define* (message-reply message
                        #:optional (reply (make-message (message-name message)
                                                        (make-g-variant #f))))
  ((foreign-fn "webkit_user_message_send_reply" '(*) void)
   message
   reply))

;; WebPage

(define (page-id page)
  ((foreign-fn "webkit_web_page_get_id" '(*) uint64) page))

(define *page* #f)

(define* (page-send-message message
                            #:optional (callback %null-pointer) (page *page*))
  ((foreign-fn "webkit_web_page_send_message_to_view" '(* * * * *) void)
   page message %null-pointer (make-g-async-callback callback) %null-pointer))

;; WebExtension

(define *extension* #f)

(define* (extension-get-page page-id #:optional (extension *extension*))
  ((foreign-fn "webkit_web_extension_get_page"
               (list '* unsigned-int) '*)
   extension page-id))

(define* (extension-send-message
          message #:optional (callback %null-pointer) (extension *extension*))
  ((foreign-fn "webkit_web_extension_send_message_to_context" '(* * * * *) void)
   extension message %null-pointer (make-g-async-callback callback) %null-pointer))


;;; Entry point and signal processors

(define (message-received-callback page message)
  (g-print "Got a message %s with content \n%s\n"
           (message-name message)
           (or (g-variant-string (message-params message)) ""))
  (message-reply message)
  1)

(define (page-created-callback extension page)
  (set! *page* page)
  (g-print "Page %i created!\n" (page-id page))
  (g-signal-connect
   page "user-message-received"
   (procedure->pointer*
    message-received-callback '(* *) unsigned-int))
  (g-print "User message handler installed!\n"))

(define (entry-webextensions extension-ptr)
  (g-signal-connect
   extension-ptr "page-created"
   (procedure->pointer* page-created-callback '(* *) void))
  (display "WebExtensions Library handlers installed.\n"))
