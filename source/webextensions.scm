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
  ((foreign-fn "jsc_value_get_context" '(*) '*) jsc))

(define jsc-context-current
  (foreign-fn "jsc_context_get_current" '() '*))

(define* (jsc-context-evaluate code #:optional (context (jsc-make-context)))
  "Evaluate CODE in CONTEXT.
Returns raw JSCValue resulting from CODE evaluation."
  ((foreign-fn "jsc_context_evaluate" `(* * ,int) '*)
   context (string->pointer* code) -1))

(define* (jsc-context-evaluate* code #:optional (context (jsc-make-context)))
  "Evaluate CODE in CONTEXT, but return Scheme value."
  (jsc->scm (jsc-context-evaluate code context)))

(define (jsc-context-exception context)
  "Return the last JSCException in CONTEXT."
  (let ((exception ((foreign-fn "jsc_context_get_expression" '(*) '*) context)))
    (if (eq? %null-pointer exception)
        #f
        exception)))

(define* (jsc-context-value name #:optional (context (jsc-make-context)))
  "Returns the JSCValue (as a pointer) bound to NAME in CONTEXT."
  ((foreign-fn "jsc_context_get_value" '(* *) '*)
   context (string->pointer* name)))

(define* (jsc-context-value-set! name value #:optional (context (jsc-make-context)))
  ((foreign-fn "jsc_context_set_value" '(* * *) '*)
   context (string->pointer* name)
   (if (pointer? value)
       value
       (scm->jsc value))))

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

(define* (jsc-make-undefined #:optional (context (jsc-make-context)))
  ((foreign-fn "jsc_value_new_undefined" '(*) '*) context))
(define (jsc-undefined? jsc)
  (positive? ((foreign-fn "jsc_value_is_undefined" '(*) unsigned-int) jsc)))

(define* (jsc-make-null #:optional (context (jsc-make-context)))
  ((foreign-fn "jsc_value_new_null" '(*) '*) context))
(define (jsc-null? jsc)
  (positive? ((foreign-fn "jsc_value_is_null" '(*) unsigned-int) jsc)))

(define* (jsc-make-number num #:optional (context (jsc-make-context)))
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

(define* (jsc-make-boolean value #:optional (context (jsc-make-context)))
  ((foreign-fn "jsc_value_new_boolean" (list '* unsigned-int) '*)
   context (if value 1 0)))
(define (jsc-boolean? jsc)
  (positive? ((foreign-fn "jsc_value_is_boolean" '(*) unsigned-int) jsc)))
(define (jsc->boolean jsc)
  (positive? ((foreign-fn "jsc_value_to_boolean" '(*) unsigned-int) jsc)))

(define* (jsc-make-string str #:optional (context (jsc-make-context)))
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
(define (jsc-property-set! object property-name value)
  ((foreign-fn "jsc_value_object_set_property" '(* * *) void)
   object (string->pointer* property-name)
   value))

(define* (jsc-make-array list-or-vector #:optional (context (jsc-make-context)))
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

(define* (jsc-make-object class contents #:optional (context (jsc-make-context)))
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

(define* (jsc-make-function name callback number-of-args #:optional (context (jsc-make-context)))
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
  (let ((jsc-type ((foreign-fn "jsc_value_get_type" '() '*))))
    (apply
     (foreign-fn function-name
                 (append (make-list (length initial-args) '*)
                         (reduce (lambda (l a)
                                   (append l (list unsigned-int '*)))
                                 '()
                                 args)
                         (list unsigned-int))
                 '*)
     (append initial-args
             (reduce (lambda (l a)
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

(define* (scm->jsc object #:optional (context (jsc-make-context)))
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

;; jsc Scheme types: boolean?, pair?, symbol?, number?, char?, string?, vector?, port?, procedure?
;; Guile ones: hash-table? and objects (any predicate for those? record? maybe)

(define* (json->jsc json #:optional (context (jsc-make-context)))
  ((foreign-fn "jsc_value_new_from_json" '(* *) '*)
   context (string->pointer* json)))

(define (jsc->json jsc-value)
  (pointer->string
   ((foreign-fn "jsc_value_to_json" (list '* int) '*)
    jsc-value 0)))

;;; Webkit extensions API

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
