;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(define-module (webkit-webextensions)
  #:use-module (system vm program)
  #:use-module (system foreign)
  #:use-module (system foreign-library)
  #:use-module (srfi srfi-1) ;; List processing.
  #:use-module (srfi srfi-2) ;; and-let*
  #:use-module (srfi srfi-9) ;; Record types.
  #:export (entry-webextensions))

;; When developing, try:
;; (define lib (load-foreign-library "/gnu/store/9hijxiihm6l9260wmjsnk6qndh5asdf6-webkitgtk-2.38.5/lib/libwebkit2gtk-4.1.so"))
(define lib #f)

;;; General utilities (Glib and FFI)

(define (pointer/false pointer)
  "Return #f is the POINTER is NULL or #f.
Otherwise return the POINTER itself.

Useful to dispatch NULL/non-NULL pointers on the Scheme-side."
  (if (or (eq? %null-pointer pointer)
          (eq? #f pointer))
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
  (and-let* ((pointer (pointer/false pointer)))
    (pointer->string pointer)))

(define (procedure-ffi-arglist procedure)
  (make-list (car (procedure-minimum-arity procedure)) '*))

(define* (procedure->pointer*
          procedure
          #:optional (arg-types (when (procedure? procedure)
                                  (procedure-ffi-arglist procedure)))
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

;; FIXME: Some of the `foreign-fn' -> `foreign-library-function' ->
;; `pointer->procedure' fails with wrong ARGS value. Check all the
;; `foreign-fn' all sites and ensure proper arglist.
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
  (and-let* ((g-variant (pointer/false g-variant))
             (class (integer->char
                     ((foreign-fn "g_variant_classify" '(*) unsigned-int) g-variant)))
             (get (lambda (name g-variant)
                    (pointer/false ((foreign-fn name '(*) '*) g-variant)))))
    (cond
     ((and (eq? class #\m)
           (get "g_variant_get_maybe" g-variant))
      (pointer->string
       (get "g_variant_get_string"
            (get "g_variant_get_maybe" g-variant) )))
     ((eq? class #\s)
      (get "g_variant_get_string" g-variant))
     (else
      (get "g_variant_get_string" g-variant)))))

(define* (g-signal-connect instance signal handler #:optional (data #f))
  "Connect HANDLER (pointer to procedure) to SIGNAL of INSTANCE."
  ((foreign-fn "g_signal_connect_data" (list '* '* '* '* '* int) '*)
   instance
   (string->pointer* signal)
   handler (or data %null-pointer) %null-pointer 0))

(define (make-g-async-callback procedure)
  "Turn PROCEDURE into a pointer suitable for GAsyncCallback.
PROCEDURE should have
- 2 requred arguments.
- 2 required and 1 optional argument.
- Or 3 requred arguments."
  (if (pointer/false procedure)
      (procedure->pointer* procedure (procedure-ffi-arglist procedure) void)
      %null-pointer))

(define (procedure-maximum-arity procedure)
  "Get the maximum possible number of _positional_ arguments for PROCEDURE.
Counts required and optional arguments, in other words."
  (let ((arity (procedure-minimum-arity procedure)))
    (+ (car arity) (cadr arity))))

;;; JSCore bindings

;; JSCContext

(define (make-jsc-context)
  "Create a new empty JSCContext."
  ((foreign-fn "jsc_context_new" '() '*)))

(define (jsc-context-current)
  "Get the current context.
Only makes sense in method/function/property callbacks.
Returns #f outside of them."
  (pointer/false ((foreign-fn "jsc_context_get_current" '() '*))))

(define (jsc-context-get/make)
  "Get the current context, or create it if not present."
  (or (jsc-context-current)
      (make-jsc-context)))

(define (jsc-context-global-object context)
  "Returns the JSCValue pointer for CONTEXT."
  ((foreign-fn "jsc_context_get_global_object" '(*) '*) context ))

(define* (jsc-context-evaluate code #:optional (context (jsc-context-get/make)))
  "Evaluate CODE in CONTEXT.
Returns raw JSCValue resulting from CODE evaluation."
  ((foreign-fn "jsc_context_evaluate" `(* * ,unsigned-int) '*)
   context (string->pointer* code) -1))

(define (jsc-context-exception context)
  "Return the last JSCException in CONTEXT."
  (pointer/false ((foreign-fn "jsc_context_get_expression" '(*) '*) context)))

(define* (jsc-context-value name #:optional (context (jsc-context-get/make)))
  "Returns the JSCValue (as a pointer to JSCValue) bound to NAME in
CONTEXT."
  ((foreign-fn "jsc_context_get_value" '(* *) '*)
   context (string->pointer* name)))

(define* (jsc-class-register!
          name #:optional (context (jsc-context-get/make)) (parent-class %null-pointer))
  "Return a class (JSCClass pointer) registered in CONTEXT under NAME.
Inherits from PARENT-CLASS (JSCClass pointer), if any."
  ((foreign-fn "jsc_context_register_class" '(* * * * *) '*)
   context
   (string->pointer* name)
   parent-class
   %null-pointer
   %null-pointer))

;; JSCClass

(define (jsc-class-name class)
  "Returns string name of CLASS."
  (pointer->string* ((foreign-fn "jsc_class_get_name" '(*) '*) class)))

(define (jsc-class-parent class)
  "Returns raw JSCClass pointer to the parent of CLASS."
  ((foreign-fn "jsc_class_get_parent" '(*) '*) class))

(define* (jsc-class-make-constructor class #:optional (name %null-pointer) callback)
  "Create a constructor for CLASS with CALLBACK called on object initialization.

If NAME is not provided, use CLASS name.

CALLBACK is generated with JSCValue arguments and JSCValue return
type. Using the underlying jsc_class_add_constructor is better for
cases where specifying other GTypes makes more sense.

When CALLBACK is not provided, it's implied to be a zero-argument
function doing nothing.

NOTE: The returned JSCValue pointer should be set to a global value of
NAME via `jsc-context-value-set!' to become usable."
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
     (procedure->pointer*
      (or callback
          (lambda ()
            (make-jsc-object class '())))
      (make-list number-of-args '*))
     %null-pointer
     %null-pointer
     jsc-type
     number-of-args
     (make-list number-of-args jsc-type))))
(define* (jsc-constructor-call constructor #:rest args)
  (apply-with-args "jsc_value_constructor_call" (list constructor) args))

(define* (jsc-class-add-method! class name callback)
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

(define* (jsc-class-add-property! class name getter-callback setter-callback)
  "Add a NAME property to JSCClass CLASS.

GETTER-CALLBACK should be a procedure with one argument—a CLASS instance.

SETTER-CALLBACK should be a procedure with two arguments—a CLASS
instance and the new value of the property."
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

;; JSCValue

(define (jsc-context value)
  "Get the context of JSC VALUE.
Guaranteed to return a non-NULL pointer, because any JSCValue has a
context it belongs to."
  ((foreign-fn "jsc_value_get_context" '(*) '*) value))

;; NOTE: Don't use undefined when passing objects to/from browser:
;; JSON doesn't support undefined!
(define* (make-jsc-undefined #:optional (context (jsc-context-get/make)))
  ((foreign-fn "jsc_value_new_undefined" '(*) '*) context))
(define (jsc-undefined? value)
  (positive? ((foreign-fn "jsc_value_is_undefined" '(*) unsigned-int) value)))

(define* (make-jsc-null #:optional (context (jsc-context-get/make)))
  ((foreign-fn "jsc_value_new_null" '(*) '*) context))
(define (jsc-null? value)
  (positive? ((foreign-fn "jsc_value_is_null" '(*) unsigned-int) value)))


(define* (make-jsc-number num #:optional (context (jsc-context-get/make)))
  ;; Don't call it with complex numbers!!!
  (if (real? num)
      ((foreign-fn "jsc_value_new_number" (list '* double) '*)
       context (exact->inexact num))
      (error "Cannot create JSC number out of non-real number:" num)))
(define (jsc-number? jsc)
  (positive? ((foreign-fn "jsc_value_is_number" '(*) unsigned-int) jsc)))
(define (jsc->number jsc)
  ;; No int32 conversions, because most Guile operations convert
  ;; floats to ints when necessary.
  ;; TODO: Maybe call inexact->exact on the result?
  ((foreign-fn "jsc_value_to_double" '(*) double) jsc))

(define* (make-jsc-boolean value #:optional (context (jsc-context-get/make)))
  ((foreign-fn "jsc_value_new_boolean" (list '* unsigned-int) '*)
   context (if value 1 0)))
(define (jsc-boolean? jsc)
  (positive? ((foreign-fn "jsc_value_is_boolean" '(*) unsigned-int) jsc)))
(define (jsc->boolean jsc)
  (positive? ((foreign-fn "jsc_value_to_boolean" '(*) unsigned-int) jsc)))

(define* (make-jsc-string str #:optional (context (jsc-context-get/make)))
  ((foreign-fn "jsc_value_new_string" '(* *) '*)
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
  ((foreign-fn "jsc_value_object_delete_property" '(* *) void)
   object (string->pointer* property-name)))

(define* (make-jsc-array list-or-vector #:optional (context (jsc-context-get/make)))
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
(define (jsc-array? jsc)
  (positive? ((foreign-fn "jsc_value_is_array" '(*) unsigned-int))))
(define (jsc->list object)
  "Convert OBJECT JSCValue array into a Scheme list."
  (let rec ((idx 0))
    (if (zero? ((foreign-fn "jsc_value_object_has_property" '(* *) unsigned-int)
                object (string->pointer (number->string idx))))
        '()
        (cons (jsc->scm (jsc-property object (number->string idx)))
              (rec (1+ idx))))))

(define* (make-jsc-object class contents #:optional (context (jsc-context-get/make)))
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
                             (scm->jsc value)))))
    obj))
(define (jsc-object? obj)
  (positive? ((foreign-fn "jsc_value_is_object" '(*) unsigned-int) obj)))
(define (jsc-instance-of? obj parent-or-name)
  "Check whether OBJ is an instance of PARENT-OR-NAME.
PARENT-OR-NAME is either a JSCClass object or a string name thereof."
  (positive? ((foreign-fn "jsc_value_object_is_instance_of" '(* *) unsigned-int)
              obj (string->pointer*
                   (if (pointer? parent-or-name)
                       (jsc-class-name parent-or-name)
                       parent-or-name)))))
(define* (jsc-object-call-method object name #:rest args)
  (apply-with-args "jsc_value_object_invoke_method" (list object (string->pointer* name)) args))

(define* (make-jsc-function name callback #:optional (context (jsc-context-get/make)))
  "Create a function with CALLBACK and bind it to NAME.
If NAME is #f, create an anonymous function.
Implies that CALLBACK returns a valid JSCValue. If it doesn't, returns
a JSCValue for undefined."
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
     (procedure->pointer*
      (lambda* (#:rest args)
        (let ((value (apply callback args)))
          (if (pointer? value)
              value
              (make-jsc-undefined))))
      (make-list number-of-args '*))
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
  (let ((jsc-type ((foreign-fn "jsc_value_get_type" '() '*))))
    (apply
     (foreign-fn function-name
                 (append
                  (make-list (length initial-args) '*)
                  (make-list (* 2 (length args)) '*)
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

;; JSC-related conversion utilities.

(define* (scm->jsc object #:optional (context (jsc-context-get/make)))
  "Convert a Scheme OBJECT to JSCValue.
Converts alists to objects.
Converts vectors and proper lists to arrays.
Converts procedures to anonymous functions.

If the OBJECT is a pointer, this pointer is implied to be a JSCValue
already and is returned."
  (cond
   ((pointer? object) object)
   ((eq? #:null object) (make-jsc-null context))
   ((eq? #:undefined object) (make-jsc-undefined context))
   ((symbol? object) (scm->jsc (symbol->string object)))
   ((keyword? object) (scm->jsc (keyword->symbol object)))
   ((boolean? object) (make-jsc-boolean object context))
   ((number? object) (make-jsc-number object context))
   ((string? object) (make-jsc-string object context))
   ((vector? object) (make-jsc-array object context))
   ;; Dotted alist
   ((and (list? object)
         (not (list? (cdr (car object)))))
    (make-jsc-object %null-pointer object context))
   ((list? object) (make-jsc-array object context))
   ((procedure? object) (make-jsc-function #f object context))
   (else (error "scm->jsc: unknown value passed" object))))

;; Defining here because it depends on scm->jsc.
(define* (jsc-context-value-set! name value #:optional (context (jsc-context-get/make)))
  "Set the NAMEd value in CONTEXT to a VALUE.
VALUE can be a Scheme value or a pointer to JSCValue."
  ((foreign-fn "jsc_context_set_value" '(* * *) void)
   context (string->pointer* name)
   (scm->jsc value)))

(define* (jsc->scm object)
  "Convert JSCValue OBJECT to a Scheme value.
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

;; Scheme types: boolean?, pair?, symbol?, number?, char?, string?, vector?, port?, procedure?
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
   ((foreign-fn "jsc_value_to_json" (list '* unsigned-int) '*)
    jsc-value 0)))

;;; Threading primitives

(define* (make-jsc-error message #:optional (context (jsc-context-get/make)))
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

(define* (make-jsc-promise success #:key failure default (context (jsc-context-get/make)))
  "Create a JS promise with SUCCESS running on completion and FAILURE on error.
SUCCESS/FAILURE is determined based on `*callback-table*' data.
When the result fetching timeouts (10 seconds), calls SUCCESS on
DEFAULT."
  (let ((id (get-id)))
    (jsc-constructor-call
     (jsc-context-value "Promise" context)
     (make-jsc-function
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
     (let* ((class-obj (jsc-class-register! context class))
            (constructor (jsc-class-make-constructor class-obj)))
       (letrec ((add-methods
                 (lambda (meths)
                   (unless (null? meths)
                     (jsc-class-add-method! class-obj (car meths) (cadr meths))
                     (add-methods (cddr meths))))))
         (add-methods methods)
         (jsc-context-value-set! class constructor context)
         (jsc-property-set!
          (jsc-context-value "browser" context)
          property
          (jsc-constructor-call constructor)))))))

(define (inject-browser context)
  (let* ((class (jsc-class-register! "Browser" context))
         (constructor (jsc-class-make-constructor class)))
    (jsc-context-value-set! context "Browser" constructor)
    (jsc-context-value-set! context "browser" (make-jsc-object class '()))))

;;; ContextMenu and ContextMenuItem

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
                        #:optional (reply (make-message (message-name message))))
  ((foreign-fn "webkit_user_message_send_reply" '(* *) void)
   message
   reply))

;; WebPage

(define *page* #f)

(define* (page-id #:optional (page *page*))
  (when page
    ((foreign-fn "webkit_web_page_get_id" '(*) uint64) page)))

(define* (page-send-message message
                            #:optional (callback %null-pointer) (page *page*))
  ((foreign-fn "webkit_web_page_send_message_to_view" '(* * * * *) void)
   page message %null-pointer (make-g-async-callback callback) %null-pointer))

(define (page-main-frame page)
  "Get the main WebKitFrame associated with PAGE."
  ((foreign-fn "webkit_web_page_get_main_frame" '(*) '*)
   page))

;; WebKitURIRequest & WebKitURIResponse

(define (make-request uri)
  ((foreign-fn "webkit_uri_request_new" '(*) '*)
   (string->pointer* uri)))

(define (request-uri request)
  (pointer->string*
   ((foreign-fn "webkit_uri_request_get_uri" '(*) '*) request)))

(define (request-uri-set! request uri)
  ((foreign-fn "webkit_uri_request_set_uri" '(* *) void)
   request (string->pointer* uri)))

(define (request-method request)
  (pointer->string*
   ((foreign-fn "webkit_uri_request_get_http_method" '(*) '*) request)))

(define *headers* (make-hash-table))

(define (parse-soup-headers headers)
  (set! *headers* (make-hash-table))
  (when (pointer/false headers)
    ((foreign-fn "soup_message_headers_foreach" '(* *) void)
     headers
     (procedure->pointer*
      (lambda (name value)
        (g-print "Started processing header%s\n" (pointer->string* name))
        (and-let* ((actual-name (pointer->string* name))
                   (actual-value
                    (pointer->string*
                     ((foreign-fn "soup_message_headers_get_list"
                                  '(* *) '*)
                      headers name))))
          (hash-set! *headers* actual-name actual-value)))
      '(* *) void)))
  (hash-map->list (lambda (key value) (cons key value)) *headers*))

(define (request-headers request)
  (parse-soup-headers
   ((foreign-fn "webkit_uri_request_get_http_headers" '(*) '*) request)))

;; TODO: Test whether it works.
(define* (request-header-set! request name #:optional (value "") append?)
  "Set a header with NAME to VALUE.
Appends the header if APPEND? or if NAME is not set yet.
Otherwise replaces NAME value to VALUE."
  (let* ((headers (request-headers request))
         (name-ptr (string->pointer* name))
         (value-ptr (string->pointer* value))
         (prev-value (pointer->string*
                      ((foreign-fn "soup_message_headers_get_list" '(* *) '*)
                       headers name-ptr))))
    ((foreign-fn
      (cond
       (append? "soup_message_headers_append")
       (prev-value "soup_message_headers_replace")
       (else "soup_message_headers_append"))
      '(* * *) void)
     headers name-ptr value-ptr)))

(define (response-uri response)
  (pointer->string*
   ((foreign-fn "webkit_uri_response_get_uri" '(*) '*) response)))

(define (response-content-length response)
  ((foreign-fn "webkit_uri_response_get_content_length" '(*) uint64) response))

(define (response-headers response)
  (parse-soup-headers
   ((foreign-fn "webkit_uri_response_get_http_headers" '(*) '*) response)))

(define (response-mime-type response)
  (pointer->string*
   ((foreign-fn "webkit_uri_response_get_mime_type" '(*) '*) response)))

(define (response-mime-type response)
  (pointer->string*
   ((foreign-fn "webkit_uri_response_get_mime_type" '(*) '*) response)))

(define (response-status-code response)
  ((foreign-fn "webkit_uri_response_get_status_code" '(*) unsigned-int) response))

(define (response-suggested-filename response)
  (pointer->string*
   ((foreign-fn "webkit_uri_response_get_suggested_filename" '(*) '*) response)))

;; URI parsing

(define (parse-uri uri-string)
  "Parses URI-STRING and returns a list of (strings, unless stated otherwise):
- Scheme.
- User.
- Password.
- Auth params.
- Host.
- Port.
- Path.
- Query params (alist).
- Fragment."
  (and-let* ((g-uri (pointer/false
                     ((foreign-fn "g_uri_parse" `(* ,unsigned-int *) '*)
                      (string->pointer* uri-string)
                      ;; The most lenient set of settings.
                      (+ 1    ;; G_URI_FLAGS_PARSE_RELAXED
                         2    ;; G_URI_FLAGS_HAS_PASSWORD
                         4    ;; G_URI_FLAGS_HAS_AUTH_PARAMS
                         16   ;; G_URI_FLAGS_NON_DNS
                         256) ;; G_URI_FLAGS_SCHEME_NORMALIZE
                      %null-pointer)))
             (get (lambda (fn)
                    (pointer->string* ((foreign-fn fn '(*) '*) g-uri)))))
    (list (get "g_uri_get_scheme")
          (get "g_uri_get_user")
          (get "g_uri_get_password")
          (get "g_uri_get_auth_params")
          (get "g_uri_get_host")
          ((foreign-fn "g_uri_get_port" '(*) unsigned-int) g-uri)
          (get "g_uri_get_path")
          (and-let* ((query-hash
                      (pointer/false
                       ((foreign-fn "g_uri_parse_params"
                                    `(* ,int * ,unsigned-int *)
                                    '*)
                        ((foreign-fn "g_uri_get_query" '(*) '*) g-uri)
                        -1
                        (string->pointer* "&;")
                        (+
                         2  ;; G_URI_PARAMS_WWW_FORM
                         4) ;; G_URI_PARAMS_PARSE_RELAXED
                        %null-pointer)))
                     (params (make-hash-table)))
            ((foreign-fn "g_hash_table_foreach" '(* * *) void)
             query-hash
             (procedure->pointer*
              (lambda (key value)
                (hash-set! params (pointer->string* key) (pointer->string* value)))
              '(* *)
              void)
             %null-pointer)
            (hash-map->list (lambda (key value) (cons key value)) params))
          (get "g_uri_get_fragment"))))

;; ScriptWorld

(define (script-world-default)
  "Get the ScriptWorld for the main frame of the page.
Should? always return a pointer to ScriptWorld."
  ((foreign-fn "webkit_script_world_get_default" '() '*)))

(define* (make-script-world #:optional name)
  (if name
      ((foreign-fn "webkit_script_world_new_with_name" '(*) '*)
       (string->pointer* name))
      ((foreign-fn "webkit_script_world_new" '() '*))))

(define (script-world-name world)
  "Always returns a string name of the WORLD."
  (pointer->string*
   ((foreign-fn "webkit_script_world_get_name" '(*) '*) world)))

;; WebKitFrame

(define (frame-id frame)
  ((foreign-fn "webkit_frame_get_id" '(*) uint64)
   frame))

(define (frame-uri frame)
  (pointer->string*
   ((foreign-fn "webkit_frame_get_uri" '(*) '*)
    frame)))

(define (frame-main? frame)
  (positive?
   ((foreign-fn "webkit_frame_is_main_frame" '(*) unsigned-int)
    frame)))

(define* (frame-jsc-context frame #:optional world)
  (if (pointer/false world)
      ((foreign-fn "webkit_frame_get_js_context_for_script_world"
                   '(* *) '*)
       frame world)
      ((foreign-fn "webkit_frame_get_js_context"
                   '(*) '*)
       frame)))

;; WebKitWebExtension

(define *extension* #f)

(define* (extension-get-page page-id #:optional (extension *extension*))
  ((foreign-fn "webkit_web_extension_get_page" (list '* unsigned-int) '*)
   extension page-id))

(define* (extension-send-message
          message #:optional (callback %null-pointer) (extension *extension*))
  ((foreign-fn "webkit_web_extension_send_message_to_context" '(* * * * *) void)
   extension message %null-pointer (make-g-async-callback callback) %null-pointer))


;;; WebExtension representation

(define *web-extensions* (make-hash-table))

(define-record-type <web-extension>
  (%make-web-extension name jsc world)
  web-extension?
  (name we-name)
  (jsc we-jsc)
  (world we-world)
  (browser we-browser we-browser-set!))

(define (make-web-extension name jsc)
  (%make-web-extension name jsc (make-script-world name)))

(define (we-context web-extension)
  (frame-jsc-context (page-main-frame *page*) (we-world web-extension)))

;;; Entry point and signal processors

(define (message-received-callback page message)
  (g-print "Got a message '%s' with content \n'%s'\n"
           (message-name message)
           (or (g-variant-string (message-params message)) ""))
  (and-let* ((string (g-variant-string (message-params message)))
             (jsc (json->jsc string)))
    (cond
     ((equal? string "addExtension")
      (hash-set! *web-extensions* (jsc-property jsc "name")
                 (make-web-extension (jsc-property jsc "name") jsc)))))
  (message-reply message)
  1)

(define (send-request-callback page request redirected-response)
  (g-print "Sending a request to '%s'\n" (request-uri request))
  (g-print "Headers are: %s\n"
           (format #f "~s" (request-headers request)))
  ;; Watch out: this one if NULL more often than not!
  (when (pointer/false redirected-response)
    (g-print "Got a redirection response for '%s' and status %i"
             (response-uri redirected-response) (response-status-code redirected-response)))
  ;; 1 = Stop processing, terminate the view.
  ;; 0 = Continue processing.
  0)

(define (page-created-callback extension page)
  (set! *page* page)
  (g-print "Page %i created!\n" (page-id page))
  (g-signal-connect
   page "user-message-received"
   (procedure->pointer*
    message-received-callback '(* *) unsigned-int))
  (g-print "User message handler installed!\n")
  (g-signal-connect
   page "send-request"
   (procedure->pointer*
    send-request-callback '(* * *) unsigned-int))
  (g-print "Request handler installed!\n"))

(define (entry-webextensions extension-ptr)
  (g-signal-connect
   extension-ptr "page-created"
   (procedure->pointer* page-created-callback '(* *) void))
  (g-signal-connect
   (script-world-default) "window-object-cleared"
   (procedure->pointer*
    (lambda (world page frame)
      (let ((context (frame-jsc-context (page-main-frame page) (script-world-default))))
        (jsc-context-value-set!
         "try_injecting_js_into_default_world"
         (make-jsc-function
          #f (lambda ()
               (g-print "Callback in!"))
          context)
         context)
        (inject-browser context)))
    '(* * *) void))
  (display "WebExtensions Library handlers installed.\n"))
