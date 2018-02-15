
(define-library (version)
  (export (rename make-version make)
          (rename version-major major)
          (rename version-minor minor)
          (rename version-patch patch)
          (rename version-revision revision))
  
  (import (gambit))

  (begin
    (define-type (version-type)
      constructor: make-version
      major minor patch revision)

    ;; Iterator
    (define (parse-version version-str)
      (call-with-input-string
        version-str
        (lambda (p)
          (define (major)
            (let ((v-maj (read-line p #\.)))
              (and
                (> (string-length v-maj) 1)
                (char=? (string-ref v-maj 0) #\v)
                (minor v-maj))))

          (define (minor v-maj)
            (let ((v-min (read-line p #\.)))
              (if (eof-object? v-min)
                (make-version v-maj 0 0 '())
                (and
                  (number? v-min)
                  (patch v-maj (##string->number v-min))))))

          (define (patch v-maj v-min)
            (let ((v-patch (read-line p #\-)))
              (if (eof-object? v-patch)
                (make-version v-maj v-min #f '())
                (and
                  (number? v-patch)
                  (revision v-maj v-min (string->number v-patch))))))

          (define (revision v-maj v-min v-patch)
            (let ((v-rev (read-all
                           p
                           (lambda (r)
                             (read-line r #\.)))))
              (make-version v-maj v-min v-patch v-rev)))
          
          (major))))
  
    (define (version-string>? str1 str2)
      (let ((len-str1 (length str1))
            (len-str2 (length str2)))
        (cond
          ((= len-str1 len-str2)
           (string-ci>? str1 str2))
          ((> len-str1 len-str2)
           (let loop ((i 0))
             (and
               (< i len-str1)
               (char>? (string-ref str1 i)
                       (string-ref str2 i))
               (loop (+ i 1)))))
          (if 

    (define (version>? ver1 ver2)



