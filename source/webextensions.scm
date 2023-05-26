;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(define-module (webkit-webextensions)
  #:use-module (system foreign)
  #:use-module (system foreign-library)
  #:use-module (srfi srfi-1)
  #:export (entry-webextensions))

;; When developing, try:
;; (define lib (load-foreign-library "/path/to/lib/libwebkit2gtk-4.1.so"))
(define lib #f)

;;; General utilities (Glib and FFI)

(define (pointer/false value)
  (if (eq? %null-pointer value)
      #f
      value))

(define (string->pointer* string)
  "Smarter string->pointer.
Converts string to pointers and leaves pointers intact."
  (cond
   ((string? string)
    (string->pointer string))
   ((pointer? string)
    string)
   (else (error "Cannot ensure string pointer for value" string))))

(define* (procedure->pointer* procedure #:optional arg-types (return-type '*))
  "Smarter procedure->pointer.
Converts procedures to pointers and leaves pointers intact."
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
   (if (string? string-or-nothing)
       string-or-nothing
       %null-pointer)))

(define (g-variant-string g-variant)
  (let ((maybe (pointer/false
                ((foreign-fn "g_variant_get_maybe" '(*) '*) g-variant))))
    (when maybe
      (pointer->string
       ((foreign-fn "g_variant_string" '(*) '*) g-variant)))))

(define* (g-signal-connect instance signal handler #:optional (data #f))
  "Connect HANDLER (pointer to procedure) to SIGNAL of INSTANCE."
  ((foreign-fn "g_signal_connect_data" (list '* '* '* '* '* int) '*)
   instance
   (string->pointer* signal)
   handler (or data %null-pointer) %null-pointer 0))

;;; JSCore bindings

;; JSCContext

(define jsc-make-context
  (foreign-fn "jsc_context_new" '() '*))

(define (jsc-context jsc)
  (pointer/false ((foreign-fn "jsc_value_get_context" '(*) '*) jsc)))

(define (jsc-context-current)
  (pointer/false ((foreign-fn "jsc_context_get_current" '() '*))))

(define (jsc-context-get/make)
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
  "Returns the JSCValue (as a pointer) bound to NAME in CONTEXT."
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

(define* (jsc-class-add-constructor class name callback number-of-args)
  "Add a constructor to CLASS with CALLBACK called on object initialization.
If NAME is #f, use CLASS name.

CALLBACK is generated with NUMBER-OF-ARGS JSCValue inputs and JSCValue
return type. Using the underlying jsc_class_add_constructor is better
for cases where specifying other GTypes makes more sense."
  (let ((jsc-type ((foreign-fn "jsc_value_get_type" '() '*))))
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

(define* (jsc-class-add-method class name callback number-of-args)
  (let ((jsc-type ((foreign-fn "jsc_value_get_type" '() '*))))
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
  ((foreign-fn "jsc_class_add_property"
               `(* * * * * * * *)
               '*)
   class
   (string->pointer* name)
   ((foreign-fn "jsc_value_get_type" '() '*))
   (procedure->pointer* getter-callback '(*))
   (procedure->pointer* setter-callback '(*))
   %null-pointer
   %null-pointer))

(define (jsc-class-name class)
  "Returns string name of CLASS."
  (pointer->string ((foreign-fn "jsc_class_get_name" '(*) '*) class)))
(define (jsc-class-parent class)
  "Returns raw JSCClass parent on CLASS."
  ((foreign-fn "jsc_class_get_parent" '(*) '*) class))

;; JSCValue

(define (jsc-value-context value)
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
  (pointer->string
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

(define* (jsc-make-array list-or-vector #:optional (context (jsc-context-get/make)))
  ;; Transform LIST-OR-VECTOR to a JSC array.
  ;; LIST-OR-VECTOR should be a list or vector, and its elements
  ;; should be JSC value pointers.
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
(define (jsc-object? obj)
  (positive? ((foreign-fn "jsc_value_is_object" '(*) unsigned-int) obj)))
(define (jsc-instance-of? obj parent-name)
  (positive? ((foreign-fn "jsc_value_object_is_instance_of" '(* *) unsigned-int)
              obj (string->pointer* parent-name))))

(define* (jsc-make-function name callback number-of-args #:optional (context (jsc-context-get/make)))
  (let ((jsc-type ((foreign-fn "jsc_value_get_type" '() '*))))
    (apply
     (foreign-fn "jsc_value_new_function"
                 (append `(* * * * * * ,unsigned-int)
                         (make-list number-of-args '*))
                 '*)
     context
     (string->pointer* name)
     (procedure->pointer* callback (make-list number-of-args '*))
     %null-pointer
     %null-pointer
     jsc-type
     number-of-args
     (make-list number-of-args jsc-type))))
(define (jsc-function? obj)
  (positive? ((foreign-fn "jsc_value_is_function" '(*) unsigned-int) obj)))

(define (apply-with-args function-name initial-args args)
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
  ((foreign-fn "jsc_context_set_value" '(* * *) '*)
   context (string->pointer* name)
   (if (pointer? value)
       value
       (scm->jsc value))))

(define* (jsc->scm object)
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

;; jsc Scheme types: boolean?, pair?, symbol?, number?, char?, string?, vector?, port?, procedure?
;; Guile ones: hash-table? and objects (any predicate for those? record? maybe)

(define* (json->jsc json #:optional (context (jsc-context-get/make)))
  ((foreign-fn "jsc_value_new_from_json" '(* *) '*)
   context (string->pointer* json)))

(define (jsc->json jsc-value)
  (pointer->string*
   ((foreign-fn "jsc_value_to_json" (list '* int) '*)
    jsc-value 0)))

;;; Threading primitives

(define* (jsc-make-error message #:optional (context (jsc-context-get/make)))
  (jsc-constructor-call
   (jsc-context-value "Error" context)
   (scm->jsc message)))

(define *id* 0)
(define (get-id)
  (let ((id *id*))
    (set! *id* (+ 1 id))
    id))
(define *callback-table* (make-hash-table))

(define* (jsc-make-promise success #:key failure default (context (jsc-context-get/make)))
  (let ((id (get-id)))
    (jsc-constructor-call
     (jsc-context-value "Promise" context)
     (jsc-make-function
      %null-pointer (lambda (suc fail)
                      (let check-result ((attempts 0))
                        (let ((data (hash-ref *callback-table* id)))
                          (cond
                           ((and data (jsc-instance-of? data "Error")))
                           (data data)
                           ((> attempts 10)
                            (jsc-make-null))
                           (else
                            (check-result (+ 1 attempts)))))))
      2 context))))


;;; Webkit extensions API

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

;; WebPage

(define (page-id page)
  ((foreign-fn "webkit_web_page_get_id" '(*) uint64) page))

(define *page* #f)

;;; Entry point

(define (entry-webextensions extension-ptr)
  (g-signal-connect
   extension-ptr "page-created"
   (procedure->pointer
    void
    (lambda (extension page)
      (set! *page* page)
      (g-print "Page %i created!\n" (page-id page)))
    '(* *)))
  (display "WebExtensions Library handlers installed.\n"))
