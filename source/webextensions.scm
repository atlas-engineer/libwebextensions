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
   (if (pointer? signal)
       signal
       (string->pointer signal))
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
   context (string->pointer code) -1))

(define* (jsc-context-evaluate* code #:optional (context (jsc-make-context)))
  "Evaluate CODE in CONTEXT, but return Scheme value."
  (jsc->scm (jsc-context-evaluate code context)))

;; JSCValue

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
  (if (string? str)
      ((foreign-fn "jsc_value_new_string" (list '* '*) '*)
       context (string->pointer str))
      (error "Cannot make a string out of" str)))
(define (jsc-string? jsc)
  (positive? ((foreign-fn "jsc_value_is_string" '(*) unsigned-int) jsc)))
(define (jsc->string jsc)
  (pointer->string
   ((foreign-fn "jsc_value_to_string" (list '*) '*) jsc)))

(define (jsc-property object property-name)
  ((foreign-fn "jsc_value_object_get_property" '(* *) '*)
   object (if (string? property-name)
              (string->pointer property-name)
              property-name)))
(define (jsc-property-set! object property-name value)
  ((foreign-fn "jsc_value_object_set_property" '(* * *) void)
   object (if (string? property-name)
              (string->pointer property-name)
              property-name)
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
  ;; CONTENTS should be a dotted alist from strings to JSCValue-s.
  (let ((obj ((foreign-fn "jsc_value_new_object" '(* * *) '*)
              context %null-pointer class)))
    (when (positive? (length contents))
      (do ((idx 0 (1+ idx)))
          ((>= idx (length contents)))
        (let ((value (cdr (list-ref contents idx))))
          (jsc-property-set! obj (string->pointer (car (list-ref contents idx)))
                             (if (pointer? value)
                                 value
                                 (scm->jsc value))))))
    obj))
(define (jsc-object? obj)
  (positive? ((foreign-fn "jsc_value_is_object" '(*) unsigned-int) obj)))

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
   context (if (pointer? json)
               json
               (string->pointer json))))

(define (jsc->json jsc-value)
  (pointer->string
   ((foreign-fn "jsc_value_to_json" (list '* int) '*)
    jsc-value 0)))

;; Webkit extensions API

(define (page-id page)
  ((foreign-fn "webkit_web_page_get_id" '(*) uint64) page))

(define *page* #f)

;; Entry point

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
