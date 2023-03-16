;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(define-module (webkit-webextensions)
  #:use-module (system foreign)
  #:use-module (system foreign-library)
  #:export (entry-webextensions))

;; When developing, try:
;; (define lib (load-foreign-library "/path/to/lib/libwebkit2gtk-4.1.so"))
(define lib #f)

(define (foreign-fn name args return-type)
  (foreign-library-function
   lib name
   #:return-type return-type
   #:arg-types args))

(define jsc-make-context
  (foreign-fn "jsc_context_new" '() '*))

(define (jsc-context jsc)
  ((foreign-fn "jsc_value_get-context" '(*) '*) jsc))

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
  ((foreign-fn "jsc_value_new_number" (list '* double) '*) context num))
(define (jsc-number? jsc)
  (positive? ((foreign-fn "jsc_value_is_number" '(*) unsigned-int) jsc)))
(define (jsc->double jsc)
  ((foreign-fn "jsc_value_to_double" '(*) double) jsc))
(define (jsc->int32 jsc)
  ((foreign-fn "jsc_value_to_int32" '(*) int32) jsc))

(define* (jsc-make-boolean value #:optional (context (jsc-make-context)))
  ((foreign-fn "jsc_value_new_boolean" (list '* unsigned-int) '*)
   context (if value 1 0)))
(define (jsc-boolean? jsc)
  (positive? ((foreign-fn "jsc_value_is_boolean" '(*) unsigned-int) jsc)))
(define (jsc->boolean jsc)
  (positive? ((foreign-fn "jsc_value_to_boolean" '(*) unsigned-int) jsc)))

(define* (jsc-make-string str #:optional (context (jsc-make-context)))
  ((foreign-fn "jsc_value_new_string" (list '* '*) '*)
   context (string->pointer str)))
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
        (jsc-property-set! arr (string->pointer (number->string idx))
                           (list-ref contents idx))))
    arr))
(define (jsc-array? jsc)
  (positive? ((foreign-fn "jsc_value_is_array" '(*) unsigned-int) jsc)))

;; All Scheme types: boolean?, pair?, symbol?, number?, char?, string?, vector?, port?, procedure?
;; Guile ones: hash-table? and objects (any predicate for those?)

(define* (json->jsc json #:optional (context (jsc-make-context)))
  ((foreign-fn "jsc_value_new_from_json" '(* *) '*)
   context (if (pointer? json)
               json
               (string->pointer json))))

(define (jsc->json jsc-value)
  (pointer->string
   ((foreign-fn "jsc_value_to_json" (list '* int) '*)
    jsc-value 0)))

(define* (g-signal-connect instance signal handler #:optional (data #f))
  ((foreign-fn "g_signal_connect_data" (list '* '* '* '* '* int) '*)
   instance
   (if (pointer? signal)
       signal
       (string->pointer signal))
   handler (or data %null-pointer) %null-pointer 0))

(define* (g-print format-string . args)
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

(define (page-id page)
  ((foreign-fn "webkit_web_page_get_id" '(*) uint64) page))

(define *page* #f)

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
