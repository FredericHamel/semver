
(define-library (version)
  (export (rename parse-version parse)
          (rename make-version make)
          (rename version-major major)
          (rename version-minor minor)
          (rename version-patch patch)
          (rename version-revision revision)

          ; Predicate
          version>?)

  (import (gambit))

  (begin
    (define-type version-type
      constructor: make-version
      (major version-major)
      (minor version-minor)
      (patch version-patch)
      (revision version-revision))


    (define minimum-version-length 5)

    (define (is-between c c1 c2)
      (and (char>=? c c1)
           (char<=? c c2)))

    (define (is-digit c)
      (is-between c #\0 #\9))

    (define (is-positive-digit c)
      (is-between c #\1 #\9))

    ;; Iterator
    (define (parse-version version-str)
      (let ((version-str-len (string-length version-str)))
        (define (major start)
          (if (and (char=? (string-ref version-str start) #\0)
                   (or (char=? (string-ref version-str (+ start 1)) #\.)
                       (error "Invalid version syntax near char" (+ start 1))))
            (minor (+ start 2) 0)
            (if (is-positive-digit (string-ref version-str start))
              (let loop ((end (+ start 1)))
                (if (< end version-str-len)
                  (let ((c (string-ref version-str end)))
                    (cond
                      ((char=? c #\.)
                       (minor
                         (+ end 1)
                         (##string->number
                          (##substring version-str start end))))
                      ((is-digit c)
                       (loop (+ end 1)))
                      (else
                        (error "Major version must be a number"))))
                  (error "Invalid version format (pos " (string->number end) ")")))
              (error "Not a positive integer"))))

        (define (minor start v-maj)
          (if (< (+ start 2) version-str-len)
            (if (and (char=? (string-ref version-str start) #\0)
                     (or (char=? (string-ref version-str (+ start 1)) #\.)
                         (error "Invalid version syntax near char" (+ start 1))))
              (patch (+ start 2) v-maj 0)
              (let loop ((end start))
                (if (< end version-str-len)
                  (let ((c (string-ref version-str end)))
                    (cond
                      ((char=? c #\.)
                       (if (> end start)
                         (patch (+ end 1)
                                v-maj
                                (##string->number
                                 (##substring version-str start end)))
                         (error "Empty minor version")))
                      ((is-digit c)
                       (loop (+ end 1)))
                      (else
                        (error "Minor version must be a number"))))
                  (error "Invalid version format (pos " (number->string end) ")"))))
            (error "Reaching EOS")))

        (define (patch start v-maj v-min)
          (if (char=? (string-ref version-str start) #\0)
            (cond
              ((= 1 (- version-str-len start))
               (make-version v-maj v-min 0 '()))
              ((char=? (string-ref version-str (+ start 1)) #\-)
               (revision (+ start 2) v-maj v-min 0))
              (else
                (error "Invalid version syntax near char" (+ start 1))))
            (let loop ((end start))
              (if (>= end version-str-len)
                (if (> end start)
                  (make-version v-maj v-min
                                (##string->number
                                 (##substring version-str start end)) '())
                  (error "Invalid version format (pos " (number->string end)))
                (let ((c (string-ref version-str end)))
                  (cond
                    ((char=? c #\-)
                     (if (> end start)
                       (revision (+ end 1) v-maj v-min
                                 (##string->number
                                  (##substring version-str start end)))
                       (error "Empty patch version")))
                    ((is-digit c)
                     (loop (+ end 1)))
                    (else
                      (error "Patch version must be a number"))))))))

        (define (revision start v-maj v-min v-patch)
          (if (< start version-str-len)
            (let loop ((beg start)
                       (pos start)
                       (v-rev '()))
              ; Reach end of string.
              (if (>= pos version-str-len)
                (make-version v-maj v-min v-patch
                              ; Revision part of semantic versioning
                              (if (> pos beg)
                                (reverse
                                  (##cons (##substring version-str beg pos)
                                   v-rev))
                                v-rev))
                (let ((c (string-ref version-str pos)))
                  (cond
                    ((char=? c #\.)
                     (let ((newbeg (+ pos 1)))
                       (loop newbeg
                             newbeg
                             (##cons
                              (if (> pos beg)
                                (##substring
                                 version-str beg pos)
                                (error "Empty revision section"))
                              v-rev))))
                    (else
                      (loop beg (+ pos 1) v-rev)))))))
          (error "Empty revision"))

        (if (>= version-str-len minimum-version-length)
          ;; Allow both vX.y.z and X.y.z
          (major (if (char=? (string-ref version-str 0) #\v) 1 0))
          (error "Invalid version minimum length"))))

    (define (version>? ver1 ver2)
      (and (=
             (version-major ver1)
             (version-major ver2))
           (or
             (> (version-minor ver1)
                (version-minor ver2))
             (and
               (= (version-minor ver1)
                  (version-minor ver2))
               (> (version-patch ver1)
                  (version-patch ver2))))))))



