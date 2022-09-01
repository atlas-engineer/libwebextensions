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

(define (jsc-make-null context)
  ((foreign-library-function lib "jsc_value_new_null"
                             #:return-type '*
                             #:arg-types '(*))
   context))

(define (jsc-null? arg)
  (= 1 ((foreign-fn "jsc_value_is_null" '(*) unsigned-int) arg)))

(define* (json->jsc json #:optional (context (jsc-make-context)))
  ((foreign-fn "jsc_value_new_from_json" '(* *) '*)
   context (if (pointer? json)
               json
               (string->pointer json))))

(define (jsc->json jsc-value)
  (pointer->string
   ((foreign-fn"jsc_value_to_json" (list '* int) '*)
    jsc-value 0)))

(define (jsc->string jsc-value)
  (pointer->string
   ((foreign-fn "jsc_value_to_string" (list '*) '*) jsc-value)))

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
