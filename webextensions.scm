(define-module (webkit-webextensions)
  #:use-module (system foreign)
  #:use-module (system foreign-library)
  #:export (entry-webextensions))

;; Implies that the .so this is called from is linked against WebKit.
(define lib (load-foreign-library #f #:search-system-paths? #t))

(define jsc-make-context
  (foreign-library-function lib "jsc_context_new" #:return-type '*))

(define (jsc-make-null context)
  ((foreign-library-function lib "jsc_value_new_null"
                             #:return-type '*
                             #:arg-types '(*))
   context))

(define (jsc-null? arg)
  (= 1 ((foreign-library-function lib "jsc_value_is_null"
                                  #:return-type unsigned-int
                                  #:arg-types '(*))
        arg)))

(define* (json->jsc json #:optional (context (jsc-make-context)))
  ((foreign-library-function lib "jsc_value_new_from_json"
                             #:return-type '*
                             #:arg-types '(* *))
   context (if (pointer? json)
               json
               (string->pointer json))))

(define (jsc->json jsc-value)
  (pointer->string
   ((foreign-library-function lib "jsc_value_to_json"
                              #:return-type '*
                              #:arg-types (list '* int))
    jsc-value 0)))

(define (jsc->string jsc-value)
  (pointer->string
   ((foreign-library-function lib "jsc_value_to_string"
                              #:return-type '*
                              #:arg-types (list '*)) jsc-value)))

(define* (g-signal-connect instance signal handler #:optional (data #f))
  ((foreign-library-function lib "g_signal_connect_data"
                             #:arg-types (list '* '* '* '* '* int)
                             #:return-type '*)
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
                              a)) args)))

(define (page-id page)
  ((foreign-library-function lib "webkit_web_page_get_id"
                             #:return-type uint64
                             #:arg-types '(*))
   page))

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
