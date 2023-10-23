;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;;; This file is huge, and thus more inspectable if you enable Hideshow
;;; (or other folding mode) on geiser-mode-hook (or whatever
;;; Scheme/Lisp mode you use):
;;;
;;; (add-hook 'geiser-mode-hook 'hs-minor-mode)
(define-module (webkit-webextensions)
  #:use-module (system vm program)
  #:use-module (system foreign)
  #:use-module (system foreign-library)
  #:use-module (srfi srfi-1) ;; List processing.
  #:use-module (srfi srfi-2) ;; and-let*
  #:use-module (srfi srfi-9) ;; Record types.
  #:export (entry-webextensions))

;;; When developing, try (obviously with machine-specific location):
;; (define lib (load-foreign-library "/gnu/store/r5jhh445ydvsdyicpw7ly8fdy5svvnh5-webkitgtk-2.40.5/lib/libwebkit2gtk-4.1.so"))
(define lib #f)

;;; General utilities (Glib and FFI)

(define (false? object)
  (not object))

(define (typecheck name value . predicates)
  "Check that VALUE conforms to at least one of PREDICATES.
Return the VALUE if successful, error otherwise.
NAME is a literal symbol for the function that type check happens in."
  (let check ((preds predicates))
    (cond
     ((null-list? preds)
      (error (format #f "~&In ~a: the value ~s is not one of ~{~a~^, ~}."
                     name value (map procedure-name predicates))))
     ((apply (car preds) value '())
      value)
     (else
      (check (cdr preds))))))

(define (pointer/false? pointer)
  "Type predicate for `pointer/false' inputs."
  (or (false? pointer)
      (pointer? pointer)))

(define (pointer/false pointer)
  "Return #f is the POINTER is NULL or #f.
Otherwise return the POINTER itself.

Useful to dispatch NULL/non-NULL pointers on the Scheme-side."
  (typecheck 'pointer/false pointer pointer/false?)
  (if (or (eq? %null-pointer pointer)
          (false? pointer))
      #f
      pointer))

(define (string->pointer* string)
  "Smarter string->pointer.
Converts string to pointers and leaves pointers intact."
  (typecheck 'string->pointer* string string? pointer/false?)
  (cond
   ((string? string)
    (string->pointer string))
   ((pointer? string)
    string)
   ((false? string)
    %null-pointer)))

(define (pointer->string* pointer)
  "Smarter pointer->string.
Turns null pointers into #f, instead of erroring."
  (typecheck 'pointer->string* pointer pointer/false?)
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
  (typecheck 'procedure->pointer* procedure procedure? pointer?)
  (cond
   ((and (procedure? procedure)
         (not arg-types))
    (error "Cannot ensure procedure pointer without arg types"))
   ((procedure? procedure)
    (procedure->pointer return-type procedure arg-types))
   ((pointer? procedure)
    procedure)))

;; FIXME: Some of the `foreign-fn' -> `foreign-library-function' ->
;; `pointer->procedure' fails with wrong ARGS value. Check all the
;; `foreign-fn' call sites and ensure proper arglist.
(define (foreign-fn name args return-type)
  "Wrapper around `foreign-library-function' for ease of throwaway C calls."
  (foreign-library-function
   lib name
   #:return-type return-type
   #:arg-types args))

(define* (g-print format-string . args)
  "Print values using Glib primitives."
  ((pointer->procedure void
                       (foreign-library-pointer lib "g_print")
                       '(* *))
   (string->pointer "%s\n")
   (string->pointer (apply format #f format-string (or args '())))))

(define (make-g-variant string-or-nothing)
  "Create and return a new maybe string (ms) GVariant."
  (typecheck 'make-g-variant string-or-nothing string? pointer/false?)
  ((foreign-fn "g_variant_new" '(* *) '*)
   (string->pointer "ms")
   (string->pointer* string-or-nothing)))

(define (g-variant-string g-variant)
  "Fetch the G-VARIANT string, if there's one.
G-VARIANT is implied to be a maybe/string GVariant."
  (typecheck 'g-variant-string g-variant pointer? false?)
  (g-print "GVariant is ~s" g-variant)
  (and-let* ((g-variant (pointer/false g-variant))
             (class (integer->char
                     ((foreign-fn "g_variant_classify" '(*) unsigned-int) g-variant)))
             (_ (g-print "Class is ~s" class))
             (get-string (lambda (g-var)
                           (pointer->string*
                            (pointer/false ((foreign-fn "g_variant_get_string" '(* *) '*)
                                            g-var %null-pointer))))))
    (cond
     ((eq? class #\m)
      (and-let* ((maybe ((foreign-fn "g_variant_get_maybe" '(*) '*) g-variant))
                 (string (get-string maybe)))
        string))
     ((eq? class #\s)
      (get-string g-variant))
     (else
      (get-string g-variant)))))

(define* (g-signal-connect instance signal handler #:optional (data #f))
  "Connect HANDLER (pointer to procedure) to SIGNAL of INSTANCE."
  ((foreign-fn "g_signal_connect_data" (list '* '* '* '* '* int) '*)
   instance
   (string->pointer* signal)
   handler (or data %null-pointer) %null-pointer 0))

(define (procedure-of-arity? arity)
  (lambda (p)
    (and (procedure? p)
         (= arity (procedure-maximum-arity p) (car (procedure-minimum-arity p))))))

(define (make-g-async-callback callback finish-fn)
  "Wrap CALLBACK into a pointer suitable for GAsyncCallback.

CALLBACK is called with:
- an object that initiated the async operation.
- and the GAsyncResult already processed.

CALLBACK doesn't have to be a procedure. If it's NULL or #f, the async
callback does nothing (and is NULL).

FINISH-FN should be one of:
- String (name of the _finish foreign function).
- Procedure on object and GAsyncResult."
  (typecheck 'make-g-async-callback finish-fn string? procedure?)
  (typecheck 'make-g-async-callback callback pointer? false? (procedure-of-arity? 2))
  (g-print "Creating a GAsyncCallback for ~s finishing with ~a" callback finish-fn)
  (if (procedure? callback)
      (procedure->pointer* (lambda (object result)
                             (g-print "GAsyncCallback entered with ~s and ~s"
                                      object result)
                             (let ((final-result
                                    (cond
                                     ((string? finish-fn)
                                      (g-print "Finish-fn is a string")
                                      ((foreign-fn finish-fn '(* * *) '*)
                                       object result %null-pointer))
                                     ((procedure? finish-fn)
                                      (g-print "Finish-fn is a procedure")
                                      (finish-fn object result)))))
                               (g-print "Final result is ~s, callback is ~s"
                                        final-result callback)
                               (callback object final-result))
                             (g-print "GAsyncCallback terminated"))
                           '(* *) void)
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

(define* (jsc-context-evaluate% code #:optional (context (jsc-context-get/make)))
  "Evaluate CODE in CONTEXT.
Returns raw JSCValue resulting from CODE evaluation."
  ((foreign-fn "jsc_context_evaluate" `(* * ,int) '*)
   context (string->pointer* code) -1))

(define* (jsc-context-evaluate code #:optional (context (jsc-context-get/make)))
  "Evaluate CODE in CONTEXT, but return Scheme value."
  (jsc->scm (jsc-context-evaluate% code context)))

(define* (jsc-context-value-set! name value #:optional (context (jsc-context-get/make)))
  "Set the NAMEd value in CONTEXT to a VALUE.
VALUE can be a Scheme value or a pointer to JSCValue."
  ((foreign-fn "jsc_context_set_value" '(* * *) void)
   context (string->pointer* name)
   (scm->jsc value)))

(define (jsc-context-exception context)
  "Return the last JSCException in CONTEXT."
  (pointer/false ((foreign-fn "jsc_context_get_exception" '(*) '*) context)))

(define (jsc-context-exception-clear! context)
  "Clear the last raised exception."
  ((foreign-fn "jsc_context_clear_exception" '(*) void) context))

(define* (jsc-context-value% name #:optional (context (jsc-context-get/make)))
  "Returns the JSCValue bound to NAME in CONTEXT."
  ((foreign-fn "jsc_context_get_value" '(* *) '*)
   context (string->pointer* name)))

(define* (jsc-context-value name #:optional (context (jsc-context-get/make)))
  "Returns the Scheme value for value bound to NAME in CONTEXT."
  (jsc->scm (jsc-context-value% name context)))

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

;; JSCException

(define (jsc-exception-name exception)
  (pointer->string*
   (pointer/false ((foreign-fn "jsc_exception_get_name" '(*) '*) exception))))

(define (jsc-exception-message exception)
  (pointer->string*
   (pointer/false ((foreign-fn "jsc_exception_get_message" '(*) '*) exception))))

(define (jsc-exception-report exception)
  (pointer->string*
   (pointer/false ((foreign-fn "jsc_exception_report" '(*) '*) exception))))

;; JSCClass

(define (jsc-class-name class)
  "Returns string name of CLASS."
  (pointer->string* ((foreign-fn "jsc_class_get_name" '(*) '*) class)))

(define (jsc-class-parent class)
  "Returns raw JSCClass pointer to the parent of CLASS."
  ((foreign-fn "jsc_class_get_parent" '(*) '*) class))

(define* (jsc-class-make-constructor class #:key (name %null-pointer) (callback #f)
                                     (number-of-args (if callback (procedure-maximum-arity callback) 0)))
  "Create a constructor for CLASS with CALLBACK called on object initialization.

If NAME is not provided, use CLASS name.

CALLBACK is generated with JSCValue arguments and JSCValue return
type. Using the underlying jsc_class_add_constructor is better for
cases where specifying other GTypes makes more sense.

When CALLBACK is not provided, it's implied to be a zero-argument
function doing nothing.

NOTE: The returned JSCValue pointer should be set to a global value of
NAME via `jsc-context-value-set!' to become usable."
  (let ((jsc-type ((foreign-fn "jsc_value_get_type" '() '*))))
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

(define* (jsc-class-add-method! class name callback #:key (number-of-args (procedure-maximum-arity callback)))
  "Add a NAMEd method to CLASS object.

CALLBACK should be a JSCValue-returning function with minimum one
argument—the instance of CLASS. Keyword arguments are not
supported. NUMBER-OF-ARGS (optional, defaults to CALLBACK maximum arg
number) mandates how much arguments CALLBACK has on the JavaScript
side."
  (let ((jsc-type ((foreign-fn "jsc_value_get_type" '() '*))))
    (apply
     (foreign-fn "jsc_class_add_method"
                 (append `(* * * * * * ,unsigned-int)
                         (make-list number-of-args '*))
                 '*)
     class
     (string->pointer* name)
     (procedure->pointer*
      (lambda* (#:rest args)
        (scm->jsc (apply callback args)))
      (make-list number-of-args '*))
     %null-pointer
     %null-pointer
     jsc-type
     number-of-args
     (make-list number-of-args jsc-type))))

(define* (jsc-class-add-property! class name getter-callback #:optional setter-callback)
  "Add a NAME property to JSCClass CLASS.

GETTER-CALLBACK should be a procedure with one argument—a CLASS
instance. It can return:
- A JSCValue.
- Or a Scheme value (which will be converted to JSCValue automatically
via `scm->jsc').

It is recommended that GETTER-CALLBACK returns JSCValue, though—
`scm->jsc' is not perfect.

SETTER-CALLBACK should be a procedure with two arguments—a CLASS
instance and the new value of the property. In case SETTER-CALLBACK is
not provided, generate a dummy one doing nothing.

WARNING: Ensure that SETTER-CALLBACK returns a JSCValue!"
  ((foreign-fn "jsc_class_add_property"
               `(* * * * * * *)
               '*)
   class
   (string->pointer* name)
   ((foreign-fn "jsc_value_get_type" '() '*))
   (procedure->pointer* (lambda (instance)
                          (scm->jsc (getter-callback instance))))
   (procedure->pointer* (lambda (instance value)
                          (if (procedure? setter-callback)
                              (scm->jsc (setter-callback instance value))
                              (make-jsc-null))))
   %null-pointer
   %null-pointer))

;; JSCValue

(define (jsc-context value)
  "Get the context of JSC VALUE.
Guaranteed to return a non-NULL pointer, because any JSCValue has a
context it belongs to."
  ((foreign-fn "jsc_value_get_context" '(*) '*) value))

(define (jsc? obj)
  (positive? ((foreign-fn "g_type_check_instance_is_a" '(* *) unsigned-int)
              obj ((foreign-fn "jsc_value_get_type" '() '*)))))

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

(define (ensure-index obj)
  (typecheck 'ensure-index obj pointer? string? integer?)
  (string->pointer*
   (if (integer? obj)
       (number->string obj)
       obj)))

(define (jsc-properties object)
  "Get a list of strings for all the properties in OBJECT."
  (let* ((ffi-props ((foreign-fn "jsc_value_object_enumerate_properties" '(*) '*) object))
         (props (if (null-pointer? ffi-props)
                    '()
                    (let destructure ((idx 0))
                      (if (null-pointer? (last (parse-c-struct ffi-props (make-list (+ 1 idx) '*))))
                          (map pointer->string* (parse-c-struct ffi-props (make-list idx '*)))
                          (destructure (+ 1 idx)))))))
    ((foreign-fn "g_strfreev" '(*) void) ffi-props)
    props))
(define (jsc-property% object property-name)
  "Get a PROPERTY-NAME-named JSCValue of property from OBJECT.

- PROPERTY-NAME is
  - String.
  - Integer.
  - Or pointer.

BEWARE: if you need a Scheme return value, use `jsc-property'
instead."
  ((foreign-fn "jsc_value_object_get_property" '(* *) '*)
   object (ensure-index property-name)))
(define (jsc-property object property-name)
  "Get the Scheme value for PROPERTY-NAME-named property of OBJECT."
  (jsc->scm (jsc-property% object property-name)))
(define (jsc-property? object property-name)
  (positive?
   ((foreign-fn "jsc_value_object_has_property" '(* *) unsigned-int)
    object (ensure-index property-name))))
(define (jsc-property-set! object property-name value)
  "Set the PROPERTY-NAME-d property of OBJECT to VALUE.
- OBJECT is JSCValue.
- PROPERTY-NAME is
  - String.
  - Integer.
  - Or pointer.
- VALUE can be a Scheme value or JSCValue."
  ((foreign-fn "jsc_value_object_set_property" '(* * *) void)
   object (ensure-index property-name)
   (scm->jsc value)))
(define (jsc-property-delete! object property-name)
  ((foreign-fn "jsc_value_object_delete_property" '(* *) void)
   object (ensure-index property-name)))

(define* (make-jsc-array list-or-vector #:optional (context (jsc-context-get/make)))
  "Transform LIST-OR-VECTOR to a JSC array.
LIST-OR-VECTOR should be a list or vector, and its elements should be
JSC value pointers."
  (typecheck 'make-jsc-array list-or-vector list? vector?)
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
  (positive? ((foreign-fn "jsc_value_is_array" '(*) unsigned-int) jsc)))
(define (jsc->list% object)
  "Convert OBJECT (JSC array) to Scheme list.
But! don't convert array elements, leaving them JSCValues."
  (let rec ((idx 0))
    (g-print "Running jsc->list%")
    (if (jsc-property? object idx)
        (begin
          (g-print "Getting property ~s" idx)
          (cons (jsc-property% object idx)
                (rec (1+ idx))))
        '())))
(define (jsc->list object)
  "Convert OBJECT JSCValue array and its element into a Scheme list."
  (map jsc->scm (jsc->list% object)))

(define* (make-jsc-object class contents #:optional (context (jsc-context-get/make)))
  "Create a JSCValue object with CLASS and CONTENTS (alist) inside it.
If CLASS is #f, no class is used."
  (typecheck 'make-jsc-object class false? pointer?)
  (typecheck 'make-jsc-object contents null-list? list? hash-table?)
  (let* ((class (or class %null-pointer))
         (obj ((foreign-fn "jsc_value_new_object" '(* * *) '*)
               context %null-pointer class))
         (contents (if (hash-table? contents)
                       (hash-map->list (lambda (key value) (cons key value))
                                       contents)
                       contents)))
    (when (positive? (length contents))
      (do ((idx 0 (1+ idx)))
          ((>= idx (length contents)))
        (let ((value (cdr (list-ref contents idx))))
          (jsc-property-set! obj (string->pointer* (car (list-ref contents idx)))
                             value))))
    obj))
(define (jsc-object? obj)
  (positive? ((foreign-fn "jsc_value_is_object" '(*) unsigned-int) obj)))
(define (jsc-instance-of? obj parent-or-name)
  "Check whether OBJ is an instance of PARENT-OR-NAME.
PARENT-OR-NAME is either a JSCClass object or a string name thereof."
  (typecheck 'jsc-instance-of? parent-or-name string? pointer?)
  (positive? ((foreign-fn "jsc_value_object_is_instance_of" '(* *) unsigned-int)
              obj (string->pointer*
                   (if (pointer? parent-or-name)
                       (jsc-class-name parent-or-name)
                       parent-or-name)))))
(define* (jsc-object-call-method object name #:rest args)
  (apply-with-args "jsc_value_object_invoke_method" (list object (string->pointer* name)) args))
(define (jsc->alist obj)
  "Turn OBJ (a JS object) into an alist of properties."
  (let destructure ((properties (jsc-properties obj)))
    (if (null-list? properties)
        '()
        (append (cons (cons (car properties)
                            (jsc-property obj (car properties)))
                      (destructure (cdr properties)))))))

(define* (make-jsc-function
          name callback #:key (context (jsc-context-get/make)) (number-of-args (procedure-maximum-arity callback)))
  "Create a function with CALLBACK and bind it to NAME.
If NAME is #f or NULL, create an anonymous function.
Implies that CALLBACK returns a valid JSCValue. If it doesn't, try to
convert it with `scm->jsc'."
  (let ((jsc-type ((foreign-fn "jsc_value_get_type" '() '*))))
    (apply
     (foreign-fn "jsc_value_new_function"
                 (append `(* * * * * * ,unsigned-int)
                         (make-list number-of-args '*))
                 '*)
     context
     (cond
      ((pointer? name)
       name)
      ((string? name)
       (string->pointer* name))
      (else %null-pointer))
     (procedure->pointer*
      (lambda* (#:rest args)
        (scm->jsc (apply callback args)))
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
  ;; FAILS here, initial value for Promise success breaks it.
  (let* ((_ (g-print "First initial value is an ~a"
                     (jsc-type-of (car initial-args))))
         (jsc-type ((foreign-fn "jsc_value_get_type" '() '*)))
         (context (jsc-context (first initial-args)))
         (_ (g-print "Context is ~s" context))
         (final-args (fold (lambda (a l)
                             (append l (list jsc-type (scm->jsc a))))
                           '()
                           args))
         (_ (g-print "Args are ~s" final-args))
         (_ (g-print "Types are ~s" (map (lambda (x)
                                           (if (eq? jsc-type x)
                                               #:type
                                               (jsc-type-of x)))
                                         final-args)))
         (value
          (apply
           (foreign-fn function-name
                       (append
                        (make-list (length initial-args) '*)
                        (make-list (length final-args) '*)
                        (list unsigned-int))
                       '*)
           (append initial-args
                   final-args
                   ;; G_TYPE_NONE (hopefully portable)
                   (list 4))))
         (_ (g-print "Got value ~s of type ~a" value (jsc-type-of value))))
    ;; (and-let* ((exception (pointer/false (jsc-context-exception context))))
    ;;   (error (string-append
    ;;           "JS " (jsc-exception-name exception) " in " function-name ": "
    ;;           (jsc-exception-message exception) "\n"
    ;;           (jsc-exception-report exception))))
    value))

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
  (typecheck
   'scm->jsc
   object
   pointer? symbol? boolean? keyword? number? string? vector? list? procedure?)
  (cond
   ((pointer? object) object)
   ((eq? #:null object) (make-jsc-null context))
   ((eq? #:undefined object) (make-jsc-undefined context))
   ((boolean? object) (make-jsc-boolean object context))
   ((symbol? object) (scm->jsc (symbol->string object) context))
   ((keyword? object) (scm->jsc (keyword->symbol object) context))
   ((boolean? object) (make-jsc-boolean object context))
   ((number? object) (make-jsc-number object context))
   ((string? object) (make-jsc-string object context))
   ((vector? object) (make-jsc-array object context))
   ;; Dotted alist
   ((and (list? object)
         (not (null-list? object))
         (list? (car object))
         (not (list? (cdr (car object)))))
    (make-jsc-object %null-pointer object context))
   ((list? object) (make-jsc-array object context))
   ((procedure? object) (make-jsc-function #f object #:context context))))

(define (jsc-type-of object)
  (cond
   ;; If it's not a pointer, then it's a Scheme value already. Return
   ;; it as is.
   ((not (pointer? object)) #:unknown)
   ((zero? ((foreign-fn "g_type_check_instance_is_a"
                        `(* ,unsigned-int) unsigned-int)
            object ((foreign-fn "jsc_value_get_type" '() unsigned-int))))
    #:unknown)
   ((jsc-null? object) #:null)
   ((jsc-undefined? object) #:undefined)
   ((jsc-boolean? object) #:boolean)
   ((jsc-string? object) #:string)
   ((jsc-number? object) #:number)
   ((jsc-array? object) #:array)
   ((jsc-function? object) #:function)
   ((jsc-object? object) #:object)
   (else #:unknown)))

(define* (jsc->scm object)
  "Convert JSCValue OBJECT to a Scheme value.
Does not support objects and functions yet."
  (g-print "Try converting object ~s of type ~s to Scheme val"
           object (jsc-type-of object))
  (case (jsc-type-of object)
    ((#:unknown) object)
    ((#:null) #:null)
    ((#:undefined) #:undefined)
    ((#:boolean) (jsc->boolean object))
    ((#:string) (jsc->string object))
    ((#:number) (jsc->number object))
    ((#:array) (jsc->list object))
    ((#:object) (jsc->alist object))
    (else (error "Cannot convert " object " to a Scheme value"))))

;; Scheme types: boolean?, pair?, symbol?, number?, char?, string?, vector?, port?, procedure?
;; Guile ones: hash-table? and objects (any predicate for those? record? maybe)
;; FIXME: Transition to hash-tables—alists vs. lists ambiguity.
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
   (jsc-context-value% "Error" context)
   (scm->jsc message)))

(define* (make-message-promise name args #:key (context (jsc-context-get/make)))
  "Create a JS promise waiting on NAME message reply.
Sends the message with NAME name and ARGS as content."
  (g-print "Sending a message to page")
  (let ((result-obj #f))
    (page-send-message
     ;; Should be easier with alist/hash, but the `scm->jsc' algo is
     ;; imperfect.
     (let* ((payload
             (make-jsc-object #f '() context)))
       (jsc-property-set!
        payload "extension" (jsc-context-value "EXTENSION" context))
       (jsc-property-set!
        payload "args" (scm->jsc args context))
       (make-message name (jsc->json payload)))
     (lambda (page reply)
       (g-print "Message replied to")
       (let ((data (json->jsc (g-variant-string (message-params reply)) context)))
         (g-print "Got ~s data" data)
         (cond
          ((not (or (jsc-object? data)
                    (jsc-null? data)))
           (error (format
                   #f "Not a JS object: ~s (~s), cannot pass it to Promise callback"
                   data (jsc-type-of data))))
          ;; If there was an error, then browser
          ;; returns {"error" : "error message"}
          (else
           (set! result-obj data))))))
    (jsc-function-call
     ;; Keep in sync with promise.js.
     (jsc-context-evaluate% "function closure (check) {
    function rec (success, failure) {
        var value = check();
        console.log(\"Got \" + JSON.stringify(value) + \" value\");
        if (value === null) {
            setTimeout(() => {
                console.log(\"Timeout fired\");
                rec(success, failure);
            },
                       100);
        } else {
            if (value.hasOwnProperty(\"error\")) {
                let error = new Error(value.error);
                failure(error);
            } else if (value.hasOwnProperty(\"results\")) {
                success(...value.results);
            } else {
                let mismatch = new Error(\"Value passed to Promise callback is malformed: \"
                                       + JSON.stringify(value)
                                       + \" and missing results/error field.\");
                failure(mismatch);
            }
        }
    }
    return new Promise(rec);
} closure" context)
     (make-jsc-function
      #f (lambda ()
           (if result-obj
               result-obj
               (make-jsc-null context)))
      #:context context))))

;;; WebExtensions Events

(define *events* (list))

(define-record-type <event>
  (make-event% name callback context)
  event?
  ;; Name of the event prefixed with the API it belongs to
  ;; i.e. "tabs.onMoved".
  (name event-name)
  ;; A callback that's called with
  ;; - Event (Scheme record object).
  ;; - Listener (JSC function).
  ;; - Listener initial args (Scheme list of JSCValues).
  ;; - And provided args (Scheme list of JSCValues).
  ;; Useful to add custom behavior based on what args are passed to
  ;; the constructor. For instance, only running listener when
  ;; matching filter (provided initially) matches the (provided at
  ;; call site) update data in tabs.onUpdated API.
  (callback event-callback event-callback-set!)
  ;; A JSCContext this event is injected into.
  (context event-context)
  ;; A list of (FUNCTION . ARGS) pairs, where FUNCTION is JSCValue
  ;; function pointer, and ARGS is a JSC array args provided when
  ;; initializing
  (listeners event-listeners event-listeners-set!))

(define (make-event name callback context)
  (let ((event (make-event% name callback context)))
    (set! *events* (cons event *events*))
    event))

(define (event-run event args)
  "Run all EVENT listeners on ARGS (JSC array).
Implicitly uses `event-callback' and `event-listeners'."
  (map (lambda (l)
         ((event-callback event) event (car l) (cdr l) args))
       (event-listeners event))
  #f)

(define (default-event-callback event listener initial-args args)
  ;; Ignoring event and its initial args.
  (apply jsc-function-call listener args)
  (make-jsc-null (jsc-context listener)))

;;; Webkit extensions API

;; Table from browser subproperty name to the injection function.
(define *apis* (make-hash-table))

(define* (define-api property class #:rest methods)
  "Register the WebExtensions JS API.

Puts the API initialization function (with the context as the sole
argument) into the Scheme-side `*apis*' table.

PROPERTY is the name under which the API is added to browser
object (i.e. \"browser.bookmarks\" for \"bookmarks\" PROPERTY).

CLASS is the string name of the class API is generated from.

METHODS is a list of (NAME TYPE FUNCTION &OPTIONAL SETTER-OR-NARGS),
where TYPE is one of:
- #:PROPERTY---FUNCTION is a getter, SETTER-OR-NARGS is a setter.
  - In case SETTER-OR-NARGS is not provided, generate dummy setter.
  - In case FUNCTION is an atom, create getter returning the atom.
- #:METHOD---FUNCTION acting on the instance of CLASS. Set the number
  of args (including the class instance!) for FUNCTION to be
  SETTER-OR-NARGS, when provided.
- As a special #:METHOD case, FUNCTION can to be #T. This means
  sending a message and returning a Promise that will be resolved with
  browser reply to the message.
- #:EVENT---FUNCTION is an event callback. If #T, use the
  `default-event-callback'. SETTER-OR-NARGS is unused.

WARNING: Ensure that FUNCTION and SETTER-OR-NARGS (when present and a
procedure) return a JSCValue!"
  (typecheck 'define-api property string?)
  (typecheck 'define-api class string?)
  (hash-set!
   *apis* property
   (lambda (context)
     (g-print "Injecting ~s API into context ~s" property context)
     (let* ((class-obj (jsc-class-register! class context))
            (constructor (jsc-class-make-constructor class-obj)))
       (g-print "Constructor for ~s created" class)
       (letrec ((add-methods/properties
                 (lambda (meths/props)
                   (unless (null? meths/props)
                     (let* ((meth/prop (car meths/props))
                            (name (list-ref meth/prop 0))
                            (type (list-ref meth/prop 1))
                            (function (list-ref meth/prop 2))
                            (setter-or-number-of-args
                             (when (= 4 (length meth/prop))
                               (list-ref meth/prop 3))))
                       (typecheck 'define-api/add-methods/properties name string?)
                       (typecheck 'define-api/add-methods/properties function
                                  procedure? pointer? boolean?)
                       (cond
                        ((and (eq? #:method type) (eq? #t function))
                         (g-print "Adding ~s Promise method" name)
                         (jsc-class-add-method!
                          class-obj name
                          ;; FIXME: Methods should not have
                          ;; optional/rest arguments!!!
                          (lambda* (instance #:rest args)
                            (g-print "Running the ~s method" name)
                            (let ((context (jsc-context instance)))
                              (make-message-promise
                               (string-append property "." name)
                               ;; If the argument is not provided,
                               ;; it's undefined, which might break
                               ;; everything message processing
                               (map (lambda (a)
                                      (if (jsc-undefined? a)
                                          (make-jsc-null context)
                                          a))
                                    args)
                               #:context context)))
                          #:number-of-args (or setter-or-number-of-args 1)))
                        ((eq? #:method type)
                         (g-print "Adding ~s method" name)
                         (jsc-class-add-method!
                          class-obj name function
                          #:number-of-args (or setter-or-number-of-args
                                               (procedure-maximum-arity function)
                                               1)))
                        ((eq? #:property type)
                         (g-print "Adding ~s property" name)
                         (jsc-class-add-property! class-obj name function setter-or-number-of-args))
                        ((eq? #:event type)
                         (g-print "Adding ~s event" name)
                         ;; FIXME: This hard-codes a lot of
                         ;; logic. There should be a way to
                         ;; encapsulate that into event
                         ;; construction. But it's too ugly for that
                         ;; at the moment (see `inject-events').
                         (let* ((callback
                                 (if (eq? #t function)
                                     default-event-callback
                                     function))
                                (event (jsc-constructor-call
                                        (jsc-context-value% "ExtEvent" context)
                                        (make-jsc-string (string-append property "." name))
                                        (make-jsc-number
                                         (pointer-address
                                          (scm->pointer callback))
                                         context))))
                           (jsc-class-add-property!
                            class-obj name (lambda (instance) event)))))
                       (add-methods/properties (cdr meths/props)))))))
         (add-methods/properties methods)
         (jsc-context-value-set! class constructor context)
         (jsc-property-set!
          (jsc-context-value% "browser" context)
          property
          (jsc-constructor-call constructor)))))))

(define-api "tabs" "Tabs"
  (list "TAB_ID_NONE" #:property (lambda (instance) -1))
  (list "create" #:method #t 2)
  ;; tabs.get, tabs.getAllInWindow, tabs.getCurrent, tabs.getSelected
  ;; are all subsets of tabs.query. Any way to call tabs.query and
  ;; post-process the result instead of spawning new messages?
  (list "query" #:method #t 2)
  (list "executeScript" #:method #t 3)
  (list "insertCSS" #:method #t 3)
  (list "removeCSS" #:method #t 3)
  (list "get" #:method #t 2)
  (list "getCurrent" #:method #t 1)
  (list "print" #:method #t 2))

(define-api "runtime" "Runtime"
  (list "getPlatformInfo" #:method #t 1)
  (list "getBrowserInfo" #:method #t 1)
  (list "getURL" #:method
        (lambda (instance path)
          (let ((path-string (jsc->string path)))
            (string-append
             "web-extension://"
             (jsc-context-value "EXTENSION" (jsc-context instance))
             (if (string-prefix? "/" path-string)
                 ""
                 "/")
             path-string)))
        2))

(define-api "management" "Management"
  (list "getSelf" #:method #t 1))

(define (inject-browser context)
  (g-print "Injecting browser into ~s" context)
  (let* ((class (jsc-class-register! "Browser" context))
         (constructor (jsc-class-make-constructor class)))
    (jsc-context-value-set! "Browser" constructor context)
    (jsc-context-value-set! "browser" (make-jsc-object class '()) context)
    (g-print "Browser injected into ~s" context)))

;; ;; Pointer constructor test code. Leave until ExtEvents are fully
;; ;; tested.
;; (let* ((context (make-jsc-context))
;;        (class (jsc-class-register! "Aaa" context))
;;        (jsc-type ((foreign-fn "jsc_value_get_type" '() unsigned-int)))
;;        (g-type-pointer 68)
;;        (constructor
;;         ((foreign-fn "jsc_class_add_constructor"
;;                      (append `(* * * * * ,unsigned-int ,unsigned-int ,unsigned-int))
;;                      '*)
;;          ;; Class and (automatic) class name.
;;          class %null-pointer
;;          ;; Constructor callback
;;          (procedure->pointer*
;;           (lambda (arg)
;;             (let ((hash (make-hash-table)))
;;               (hash-set! hash "arg" (jsc->scm arg))
;;               (scm->pointer hash))))
;;          ;; User data and GNotifyDestroy
;;          %null-pointer %null-pointer
;;          ;; Return type and arg types.
;;          g-type-pointer 1 jsc-type)))
;;   (jsc-class-add-method!
;;    class "aaaArg"
;;    (lambda* (hash)
;;      (let ((hash (pointer->scm hash)))
;;        (scm->jsc (hash-ref hash "arg")))))
;;   (jsc->scm (jsc-object-call-method
;;              (jsc-constructor-call constructor (scm->jsc 4 context))
;;              "aaaArg")))

(define (inject-events context)
  (g-print "Injecting event class into ~s" context)
  (let* ((class (jsc-class-register! "ExtEvent" context))
         (jsc-type ((foreign-fn "jsc_value_get_type" '() '*)))
         (g-type-pointer 68)
         (constructor
          ((foreign-fn "jsc_class_add_constructor"
                       (append `(* * * * * ,unsigned-int ,unsigned-int *))
                       '*)
           ;; Class and (automatic) class name.
           class %null-pointer
           ;; Constructor callback
           (procedure->pointer*
            (lambda (name callback-address)
              (scm->pointer
               (make-event
                (jsc->string name)
                (pointer->scm
                 (make-pointer
                  ;; REVIEW: Is JS number precision enough for 32-bit
                  ;; pointer? (because most compilers have pointers
                  ;; at 32-bit?)
                  (inexact->exact
                   ;; FIXME: The logic is: we can't pass pointers to
                   ;; JS constructors (I tried), only return them; we
                   ;; can safely pass JS numbers for pointer
                   ;; addresses, though.
                   (jsc->number callback-address))))
                (jsc-context-current)))))
           ;; User data
           %null-pointer
           ;; GNotifyDestroy
           (procedure->pointer*
            (lambda (event-ptr)
              (remove! (lambda (elem)
                         (eq? (pointer->scm event-ptr)
                              elem))
                       *events*))
            '(*)
            void)
           ;; Return type and arg num&types.
           g-type-pointer 1
           ;; Means that the callback has to be a JS function.
           jsc-type)))
    (g-print "Constructor created")
    (jsc-context-value-set! "ExtEvent" constructor context)
    ;; FIXME: It's a separate method because we need to: (1) get the
    ;; event method, not the JavaScript representation of it, and (2)
    ;; pass it JavaScript values. Thus the method bridging two worlds.
    (jsc-class-add-method!
     class "run" (lambda (event args)
                   (event-run (pointer->scm event) args)))
    (jsc-class-add-method!
     class "addListener"
     (lambda* (event listener #:rest args)
       (let ((event (pointer->scm event)))
         (event-listeners-set!
          event
          (cons (cons listener (remove jsc-undefined? args))
                (event-listeners event))))
       (make-jsc-null))
     ;; To be safe, because addListener can have arbitrary number of
     ;; args (across all the APIs it's 2 args at most, though).
     #:number-of-args 10)
    (g-print "addListener method added")
    (jsc-class-add-method!
     class "hasListener"
     (lambda (event listener)
       (let ((event (pointer->scm event)))
         (make-jsc-boolean (memq listener (map car (event-listeners event)))))))
    (g-print "hasListener method added")
    (jsc-class-add-method!
     class "removeListener"
     (lambda (event listener)
       (let ((event (pointer->scm event)))
         (event-listeners-set!
          event
          (filter-map
           (lambda (listener+args)
             (if (eq? (car listener+args) listener)
                 #f
                 listener+args))
           (event-listeners event))))
       (make-jsc-null)))
    (g-print "removeListener method added")
    (g-print "ExtEvent injected into ~s" context)))

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
  (typecheck 'make-context-menu-item action integer? procedure?)
  (let ((item ((foreign-fn "webkit_context_menu_item_new_from_stock_action_with_label"
                           `(,unsigned-int *) '*)
               (if (procedure? action)
                   1000
                   action)
               (string->pointer* label))))
    (when (procedure? action)
      (g-signal-connect (context-menu-item-action item) "activate" (procedure->pointer* action '(* *) void)))
    item))

;; UserMessage

(define* (make-message name #:optional (params (make-g-variant #f)))
  (g-print "Making user message ~s with params ~s" name params)
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
  (typecheck 'page-send-message callback pointer? false? (procedure-of-arity? 2))
  (g-print "Sending page message ~s with callback ~s" message callback)
  ((foreign-fn "webkit_web_page_send_message_to_view" '(* * * * *) void)
   page message %null-pointer
   (make-g-async-callback
    (lambda (object reply-message)
      (let* ((params (message-params reply-message))
             (_ (g-print "Got a reply with ~s params" (message-params reply-message)))
             (params-string (g-variant-string params)))
        (g-print "Got a reply with contents ~s" params-string)
        (callback object reply-message)))
    "webkit_web_page_send_message_to_view_finish")
   %null-pointer)
  (g-print "Message sent to page in page-send-message"))

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

(define (char-visible? c)
  "Whether the char is printable/visible (i.e. not a control char.)"
  (let ((i (char->integer c)))
    (not
     ;; All the non-printable ASCII chars.
     (or (<= 0 i 8)
         ;; Tab.
         ;; Line feed.
         (= i 11)
         (= i 12)
         ;; Carriage return.
         (<= 14 i 31)))))

(define (parse-soup-headers headers)
  (set! *headers* (make-hash-table))
  (when (pointer/false headers)
    ((foreign-fn "soup_message_headers_foreach" '(* *) void)
     headers
     (procedure->pointer*
      (lambda (name value)
        (g-print "Started processing header ~s" (pointer->string* name))
        (and-let* ((actual-name (pointer->string* name))
                   (name-proper?
                    (string-every
                     (lambda (c)
                       ;; Even though HTTP standard allows much
                       ;; more characters, everyone seems to use
                       ;; alphanumeric kebab-case ones. So ignore
                       ;; all the rest.
                       ;;
                       ;; FIXME: Maybe parse it by the
                       ;; standard? Web is too chaotic to not
                       ;; prove the Law of Implicit APIs.
                       (or (char-alphabetic? c) (char-numeric? c) (char=? c #\-)))
                     actual-name))
                   (actual-value
                    (pointer->string*
                     ((foreign-fn "soup_message_headers_get_list"
                                  '(* *) '*)
                      headers name)))
                   (value-proper? (string-every char-visible? actual-value)))
          (let ((hash-val (hash-ref *headers* actual-name '())))
            (hash-set! *headers* actual-name (cons actual-value hash-val)))))
      '(* *) void)))
  (hash-map->list (lambda (key value) (cons key value)) *headers*))

(define (request-headers% request)
  "Return a pointer to REQUEST headers (as SoupMessageHeaders structure)."
  ((foreign-fn "webkit_uri_request_get_http_headers" '(*) '*) request))
(define (request-headers request)
  "Return an alist of headers from REQUEST.
Alist heads are string names of headers.
Alist tails are lists of string for values of headers."
  (parse-soup-headers (request-headers% request)))

;; TODO: Test whether it works.
(define* (request-header-set! request name #:optional (value "") append?)
  "Set a header with NAME to VALUE.
Appends the header if APPEND? or if NAME is not set yet.
Otherwise replaces NAME value to VALUE."
  (typecheck 'request-header-set! request pointer?)
  (let* ((headers (request-headers% request))
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
  "Return an alist of headers from RESPONSE.
Alist heads are string names of headers.
Alist tails are lists of string for values of headers."
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
- Port (integer).
- Path (normalized to '/' if empty).
- Query string.
- Parsed query params (alist).
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
          ((foreign-fn "g_uri_get_port" '(*) int) g-uri)
          (get "g_uri_get_path")
          (get "g_uri_get_query")
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

(define (match-pattern pattern)
  "Return a procedure matching a string URL against PATTERN."
  (cond
   ((equal? pattern "<all_urls>")
    (lambda (url)
      (member (car (parse-uri url)) '("http" "https" "ws" "wss" "ftp" "data" "file"))))
   ((equal? pattern "*://*/*")
    (lambda (url)
      (member (car (parse-uri url)) '("http" "https" "ws" "wss"))))
   (else
    (let* ((split (lambda (string char)
                    (string-split string (lambda (c) (eq? char c)))))
           (scheme+everything-else (split pattern #\:))
           (scheme (car scheme+everything-else))
           (everything-else (substring (cadr scheme+everything-else) 2))
           (split-on-slashes (split everything-else #\/))
           (host (car split-on-slashes))
           (path (substring everything-else (string-length host)))
           (path+query (split path #\?))
           (query (if (> (length path+query) 1)
                      (cadr path+query)
                      ""))
           (path (car path+query))
           (scheme-matcher (if (string=? "*" scheme)
                               (lambda (uri) #t)
                               (lambda (uri)
                                 (equal? scheme (car (parse-uri uri))))))
           (host-matcher (cond
                          ((string=? host "*")
                           (lambda (uri) #t))
                          ((string=? scheme "file")
                           (lambda (uri) #t))
                          ((string-prefix? "*." host)
                           (lambda (uri)
                             (string-suffix? (substring host 2) (list-ref uri 4))))
                          (else
                           (lambda (uri)
                             (string-suffix? (substring host 2) (list-ref uri 4))))))
           ;; TODO
           (path-matcher #f)
           (query-matcher #f))
      (list scheme host path)))))

;; ScriptWorld

(define (script-world-default)
  "Get the ScriptWorld for the main frame of the page.
Should? always return a pointer to ScriptWorld."
  ((foreign-fn "webkit_script_world_get_default" '() '*)))

(define* (make-script-world #:optional name)
  "Creates a new ScriptWorld with NAME.

NOTE: the set of allowed characters in NAME is uncertain."
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

(define* (frame-jsc-context frame #:optional (world (script-world-default)))
  ((foreign-fn "webkit_frame_get_js_context_for_script_world"
               '(* *) '*)
   frame world))

;; WebKitWebExtension

(define *extension* #f)

(define* (extension-get-page page-id #:optional (extension *extension*))
  ((foreign-fn "webkit_web_extension_get_page" (list '* unsigned-int) '*)
   extension page-id))

(define* (extension-send-message
          message #:optional (callback #f) (extension *extension*))
  ((foreign-fn "webkit_web_extension_send_message_to_context" '(* * * * *) void)
   extension message %null-pointer
   (make-g-async-callback callback "webkit_web_extension_send_message_to_context_finish")
   %null-pointer))

;;; WebExtension representation

(define *web-extensions* (make-hash-table))

(define-record-type <web-extension>
  (make-web-extension% name jsc world)
  web-extension?
  (name we-name)
  (jsc we-jsc)
  (world we-world)
  (browser we-browser we-browser-set!)
  (permissions we-permissions we-permissions-set!))

(define (make-web-extension jsc)
  (let* ((name (jsc-property jsc "name"))
         (world (make-script-world name))
         (extension (make-web-extension% name jsc world)))
    (when (jsc-property? jsc "permissions")
      (we-permissions-set! extension (jsc-property jsc "permissions")))
    (hash-set! *web-extensions* name extension)
    (let ((inject-frame-and-world
           (lambda (f w)
             (g-print "Injecting the extension API into ~s world"
                      (script-world-name w))
             (let ((context (frame-jsc-context f w)))
               (g-print "Tabs is ~s" (hash-ref *apis* "tabs"))
               ;; This is to identify which extension the context
               ;; belongs to. Otherwise it's almost impossible to find
               ;; the extension given the context.
               (jsc-context-value-set! "EXTENSION" name context)
               (inject-browser context)
               (inject-events context)
               ((hash-ref *apis* "tabs") context)
               ((hash-ref *apis* "runtime") context)
               ((hash-ref *apis* "management") context)))))
      (g-print "Making extension ~s" name)
      (inject-frame-and-world (page-main-frame *page*) world)
      (g-print "APIs injected into the current context")
      (g-signal-connect
       world "window-object-cleared"
       (procedure->pointer*
        (lambda (w page frame)
          (inject-frame-and-world frame w))
        '(* * *) void))
      (g-print "Window object cleared callback set"))))

(define (we-context web-extension)
  (frame-jsc-context (page-main-frame *page*) (we-world web-extension)))

(define (context->web-extension context)
  (hash-ref *web-extensions* (jsc-context-value "EXTENSION" context)))

;;; Entry point and signal processors

(define (message-received-callback object message)
  (let* ((name (message-name message))
         (param-string (or (g-variant-string (message-params message)) ""))
         (param-jsc (json->jsc param-string)))
    (g-print "Got a message '~s' with content
'~s'"
             name
             param-string)
    (cond
     ((string=? name "addExtension")
      (g-print "Building extension with '~s' name and ~s contents"
               (jsc-property param-jsc "name")
               (jsc->alist param-jsc))
      ;; TODO: de-inject the extension.
      (hash-set! *web-extensions* (jsc-property param-jsc "name")
                 (make-web-extension param-jsc))
      (message-reply message))
     ((string=? name "event")
      (g-print "Got ~s event" (jsc-property param-jsc "name"))
      (map
       (lambda (event)
         (when (string=? (event-name event) (jsc-property param-jsc "name"))
           (event-run event (jsc->list% (jsc-property% param-jsc "args")))))
       *events*)))
    1))

(define (send-request-callback page request redirected-response)
  ;; (g-print "Sending a request to '~s'" (request-uri request))
  ;; (g-print "Headers are: ~s"
  ;;          (request-headers request))
  ;; Watch out: this one if NULL more often than not!
  (when (pointer/false redirected-response)
    (g-print "Got a redirection response for '~s' and status ~d"
             (response-uri redirected-response) (response-status-code redirected-response)))
  ;; 1 = Stop processing, terminate the view.
  ;; 0 = Continue processing.
  0)

(define (page-created-callback extension page)
  (set! *page* page)
  (g-print "Page ~d created!" (page-id page))
  (g-signal-connect
   page "user-message-received"
   (procedure->pointer*
    message-received-callback '(* *) unsigned-int))
  (g-print "User message handler installed!")
  ;; (g-signal-connect
  ;;  page "send-request"
  ;;  (procedure->pointer*
  ;;   send-request-callback '(* * *) unsigned-int))
  (g-print "Request handler installed!"))

(define (entry-webextensions extension)
  (set! *extension* extension)
  (debug-enable)
  (g-signal-connect
   extension "page-created"
   (procedure->pointer* page-created-callback '(* *) void))
  (g-signal-connect
   extension "user-message-received"
   (procedure->pointer*
    message-received-callback '(* *) unsigned-int))
  (g-print "WebExtensions Library handlers installed."))
