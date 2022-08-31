(define-module (webkit-webextensions)
  #:use-module (system foreign)
  #:use-module (system foreign-library)
  #:export (entry-webextensions))

;; Implies that the .so this is called from is linked against WebKit.
(define lib (load-foreign-library #f #:search-system-paths? #t))

;; (define (g-signal-connect instance signal handler)
;;   ((foreign-library-function lib "g_signal_connect_data"
;;                              #:arg-types (list '* '* '* '* '* int)
;;                              #:return-type '*)
;;    instance
;;    (if (pointer? signal)
;;        signal
;;        (string->pointer signal))
;;    handler
;;    %null-pointer %null-pointer 0))

(define jsc-make-context
  (foreign-library-function lib "jsc_context_new" #:return-type '*))

(define (jsc-make-null context)
  ((foreign-library-function lib "jsc_value_new_null"
                             #:return-type '*
                             #:arg-types (list '*))
   context))

;; (define (jsc-null? arg)
;;   (= 1 ((foreign-library-function lib "jsc_value_is_null"
;;                                   #:return-type unsigned-int
;;                                   #:arg-types (list '*))
;;         arg)))

;; (define (json->jsc json #:optional (context (make-jsc-context)))
;;   ((foreign-library-function lib "jsc_value_new_from_json"
;;                              #:return-type '*
;;                              #:arg-types (list '* '*))
;;    context (if (pointer? json)
;;                json
;;                (string->pointer json))))

(define (jsc->json jsc-value)
  (pointer->string
   ((foreign-library-function lib "jsc_value_to_json"
                              #:return-type '*
                              #:arg-types (list '* int))
    jsc-value 0)))

;; (define (jsc->string jsc-value)
;;   (pointer->string
;;    ((foreign-library-function lib "jsc_value_to_string"
;;                               #:return-type '*
;;                               #:arg-types (list '*)) jsc-value)))

(define (entry-webextensions)
  (display (jsc->json (jsc-make-null (jsc-make-context))))
  (display "\n"))
