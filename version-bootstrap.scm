
(define-type version
  major
  minor
  patch
  pre-release)

(define (parse-version version-str)
  (call-with-input-string
    version-str
    (lambda (p)
      (define (major)
        (minor (read-line p #\.)))

      (define (minor v-maj)
        (let ((v-min (read-line p #\.)))
          (if (eof-object? v-min)
            (make-version v-maj "" "" '())
            (patch v-maj v-min))))

      (define (patch v-maj v-min)
        (let ((v-patch (read-line p #\-)))
          (if (eof-object? v-patch)
            (make-version v-maj v-min "" '())
            (revision v-maj v-min v-patch))))

      (define (revision v-maj v-min v-patch)
        (let ((v-rev (read-all
                       p
                       (lambda (r)
                         (read-line r #\.)))))
          (make-version v-maj v-min v-patch v-rev)))
      
      (major))))
