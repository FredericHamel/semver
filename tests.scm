(import (semver))

(define success 0)
(define failed 0)

;; Unhegyenic macro to generate test.

(define-macro (macro-expect=? name expr val)
  `((lambda (expr)
      (if (eq? expr ,val)
        (set! success (+ success 1))
        (begin
          (println "Failed test " ,(object->string name) " expected " ,val " got " expr)
          (set! failed (+ failed 1)))))
    ,expr))

(define-macro (macro-expect-true? expr)
  `(macro-expect=? ',expr ,expr #t))

(define-macro (macro-expect-false? expr)
  `(macro-expect=? ',expr ,expr #f))

(define-macro (macro-version=? expr vmaj vmin vpatch vrev)
  `((lambda (ver)
      (macro-expect=? ,expr (version-major ver) ,vmaj)
      (macro-expect=? ,expr (version-minor ver) ,vmin)
      (macro-expect=? ,expr (version-patch ver) ,vpatch)
      (macro-expect=? ,expr (version-revision ver) ,vrev))
    ,expr))

(define (print-result)
  (display "Test ")
  (display success)
  (display "/")
  (display (+ success failed))
  (newline))


(macro-expect-false? (parse-version "0"))
(macro-expect-false? (parse-version "0.0"))

(macro-expect-false? (parse-version "0."))
(macro-expect-false? (parse-version "0.0."))
(macro-expect-false? (parse-version "0.0.0."))

(macro-expect-false? (parse-version "00.0.0"))
(macro-expect-false? (parse-version "0.00.0"))
(macro-expect-false? (parse-version "0.0.00"))

(macro-expect-false? (parse-version "01.0.0"))
(macro-expect-false? (parse-version "0.01.0"))
(macro-expect-false? (parse-version "0.0.01"))

(macro-expect-false? (parse-version "A.0.0"))
(macro-expect-false? (parse-version "0.B.0"))
(macro-expect-false? (parse-version "0.0.C"))

(macro-version=? (parse-version "0.0.0") 0 0 0 '())

(macro-version=? (parse-version "1.0.0") 1 0 0 '())
(macro-version=? (parse-version "0.1.0") 0 1 0 '())
(macro-version=? (parse-version "0.0.1") 0 0 1 '())

(macro-version=? (parse-version "v0.0.0") 0 0 0 '())
(macro-version=? (parse-version "v1.0.0") 1 0 0 '())
(macro-version=? (parse-version "v0.1.0") 0 1 0 '())
(macro-version=? (parse-version "v0.0.1") 0 0 1 '())

(define v0-0-0 (parse-version "0.0.0"))
(define v0-0-1 (parse-version "0.0.1"))
(define v0-0-2 (parse-version "0.0.2"))

(define v0-1-0 (parse-version "0.1.0"))
(define v0-1-1 (parse-version "0.1.1"))
(define v0-1-2 (parse-version "0.1.2"))

(define v0-2-0 (parse-version "0.2.0"))
(define v0-2-1 (parse-version "0.2.1"))
(define v0-2-2 (parse-version "0.2.2"))

(define v1-0-0 (parse-version "1.0.0"))
(define v1-0-1 (parse-version "1.0.1"))
(define v1-0-2 (parse-version "1.0.2"))

(define v1-1-0 (parse-version "1.1.0"))
(define v1-1-1 (parse-version "1.1.1"))
(define v1-1-2 (parse-version "1.1.2"))

(define v1-2-0 (parse-version "1.2.0"))
(define v1-2-1 (parse-version "1.2.1"))
(define v1-2-2 (parse-version "1.2.2"))

(define v2-0-0 (parse-version "2.0.0"))

;; Test version=?
(macro-expect-true? (version=? v0-0-0 v0-0-0))
(macro-expect-true? (version=? v0-0-1 v0-0-1))
(macro-expect-true? (version=? v0-0-2 v0-0-2))

(macro-expect-true? (version=? v0-1-0 v0-1-0))
(macro-expect-true? (version=? v0-1-1 v0-1-1))
(macro-expect-true? (version=? v0-1-2 v0-1-2))

(macro-expect-true? (version=? v0-2-0 v0-2-0))
(macro-expect-true? (version=? v0-2-1 v0-2-1))
(macro-expect-true? (version=? v0-2-2 v0-2-2))

(macro-expect-true? (version=? v1-0-0 v1-0-0))
(macro-expect-true? (version=? v1-0-1 v1-0-1))
(macro-expect-true? (version=? v1-0-2 v1-0-2))

(macro-expect-true? (version=? v1-1-0 v1-1-0))
(macro-expect-true? (version=? v1-1-1 v1-1-1))
(macro-expect-true? (version=? v1-1-2 v1-1-2))

(macro-expect-true? (version=? v1-2-0 v1-2-0))
(macro-expect-true? (version=? v1-2-1 v1-2-1))
(macro-expect-true? (version=? v1-2-2 v1-2-2))

(macro-expect-true? (version=? v2-0-0 v2-0-0))

;; Test version<?
(macro-expect-true? (version<? v0-0-0 v0-0-1))
(macro-expect-true? (version<? v0-0-1 v0-0-2))
(macro-expect-true? (version<? v0-0-2 v0-1-0))

(macro-expect-true? (version<? v0-1-0 v0-1-1))
(macro-expect-true? (version<? v0-1-1 v0-1-2))
(macro-expect-true? (version<? v0-1-2 v0-2-0))

(macro-expect-true? (version<? v0-2-0 v0-2-1))
(macro-expect-true? (version<? v0-2-1 v0-2-2))
(macro-expect-true? (version<? v0-2-2 v1-0-0))

(macro-expect-true? (version<? v1-0-0 v1-0-1))
(macro-expect-true? (version<? v1-0-1 v1-0-2))
(macro-expect-true? (version<? v1-0-2 v1-1-0))

(macro-expect-true? (version<? v1-1-0 v1-1-1))
(macro-expect-true? (version<? v1-1-1 v1-1-2))
(macro-expect-true? (version<? v1-1-2 v1-2-0))

(macro-expect-true? (version<? v1-2-0 v1-2-1))
(macro-expect-true? (version<? v1-2-1 v1-2-2))
(macro-expect-true? (version<? v1-2-2 v2-0-0))

(macro-expect-true? (version<? v0-0-0 v2-0-0))

(macro-expect-false? (version>? v0-0-0 v0-0-1))
(macro-expect-false? (version>? v0-0-1 v0-0-2))
(macro-expect-false? (version>? v0-0-2 v0-1-0))

(macro-expect-false? (version>? v0-1-0 v0-1-1))
(macro-expect-false? (version>? v0-1-1 v0-1-2))
(macro-expect-false? (version>? v0-1-2 v0-2-0))

(macro-expect-false? (version>? v0-2-0 v0-2-1))
(macro-expect-false? (version>? v0-2-1 v0-2-2))
(macro-expect-false? (version>? v0-2-2 v1-0-0))

(macro-expect-false? (version>? v1-0-0 v1-0-1))
(macro-expect-false? (version>? v1-0-1 v1-0-2))
(macro-expect-false? (version>? v1-0-2 v1-1-0))

(macro-expect-false? (version>? v1-1-0 v1-1-1))
(macro-expect-false? (version>? v1-1-1 v1-1-2))
(macro-expect-false? (version>? v1-1-2 v1-2-0))

(macro-expect-false? (version>? v1-2-0 v1-2-1))
(macro-expect-false? (version>? v1-2-1 v1-2-2))
(macro-expect-false? (version>? v1-2-2 v2-0-0))

(macro-expect-false? (version>? v0-0-0 v2-0-0))
(print-result)

