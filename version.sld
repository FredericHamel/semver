
(define-library (version)
  (export parse-version
          make-version
          version-major
          version-minor
          version-patch
          version-revision

          ;; Conversion
          version->string

          ; Predicate
          version=?
          version>?)

  (import (gambit))

  (begin
    (define-type version
      constructor: macro-make-version
      implementer: implement-type-version
      macros:
      prefix: macro-
      opaque:
      ;unprintable:

      major
      minor
      patch
      revision)

    (implement-type-version)

    ;; Wrapper function
    (define (make-version major minor patch revision)
      (macro-make-version major minor patch revision))

    (define (version-major ver)
      (macro-version-major ver))

    (define (version-minor ver)
      (macro-version-minor ver))

    (define (version-patch ver)
      (macro-version-patch ver))

    (define (version-revision ver)
      (macro-version-revision ver))

    ;; Parsing
    (define minimum-version-length 5)

    (define (is-between c c1 c2)
      (and (char>=? c c1)
           (char<=? c c2)))

    (define (is-digit c)
      (is-between c #\0 #\9))

    (define (is-positive-digit c)
      (is-between c #\1 #\9))

    ;; Iterator
    (define (parse-version str)
      (let ((str-len (string-length str)))
        (define (major start)
          (if (and (char=? (string-ref str start) #\0)
                   (or (char=? (string-ref str (+ start 1)) #\.)
                       (error "Invalid version syntax near char" (+ start 1))))
            (minor (+ start 2) 0)
            (if (is-positive-digit (string-ref str start))
              (let loop ((end (+ start 1)))
                (if (< end str-len)
                  (let ((c (string-ref str end)))
                    (cond
                      ((char=? c #\.)
                       (minor
                         (+ end 1)
                         (##string->number
                          (##substring str start end))))
                      ((is-digit c)
                       (loop (+ end 1)))
                      (else
                        (error "Major version must be a number"))))
                  (error "Invalid version format (pos " (string->number end) ")")))
              (error "Not a positive integer"))))

        (define (minor start v-maj)
          (if (< (+ start 2) str-len)
            (if (and (char=? (string-ref str start) #\0)
                     (or (char=? (string-ref str (+ start 1)) #\.)
                         (error "Invalid version syntax near char" (+ start 1))))
              (patch (+ start 2) v-maj 0)
              (let loop ((end start))
                (if (< end str-len)
                  (let ((c (string-ref str end)))
                    (cond
                      ((char=? c #\.)
                       (if (> end start)
                         (patch (+ end 1)
                                v-maj
                                (##string->number
                                 (##substring str start end)))
                         (error "Empty minor version")))
                      ((is-digit c)
                       (loop (+ end 1)))
                      (else
                        (error "Minor version must be a number"))))
                  (error "Invalid version format (pos " (number->string end) ")"))))
            (error "Reaching EOS")))

        (define (patch start v-maj v-min)
          (if (char=? (string-ref str start) #\0)
            (cond
              ((= 1 (- str-len start))
               (macro-make-version v-maj v-min 0 '()))
              ((char=? (string-ref str (+ start 1)) #\-)
               (revision (+ start 2) v-maj v-min 0))
              (else
                (error "Invalid version syntax near char" (+ start 1))))
            (let loop ((end start))
              (if (>= end str-len)
                (if (> end start)
                  (macro-make-version v-maj v-min
                                (##string->number
                                 (##substring str start end)) '())
                  (error "Invalid version format (pos " (number->string end)))
                (let ((c (string-ref str end)))
                  (cond
                    ((char=? c #\-)
                     (if (> end start)
                       (revision (+ end 1) v-maj v-min
                                 (##string->number
                                  (##substring str start end)))
                       (error "Empty patch version")))
                    ((is-digit c)
                     (loop (+ end 1)))
                    (else
                      (error "Patch version must be a number"))))))))

        (define (revision start v-maj v-min v-patch)
          (if (< start str-len)
            (let loop ((beg start)
                       (pos start)
                       (v-rev '()))
              ; Reach end of string.
              (if (>= pos str-len)
                (make-version v-maj v-min v-patch
                              ; Revision part of semantic versioning
                              (if (> pos beg)
                                (reverse
                                  (##cons (##substring str beg pos)
                                   v-rev))
                                v-rev))
                (let ((c (string-ref str pos)))
                  (cond
                    ((char=? c #\.)
                     (let ((newbeg (+ pos 1)))
                       (loop newbeg
                             newbeg
                             (##cons
                              (if (> pos beg)
                                (##substring
                                 str beg pos)
                                (error "Empty revision section"))
                              v-rev))))
                    (else
                      (loop beg (+ pos 1) v-rev)))))))
          (error "Empty revision"))

        (if (>= str-len minimum-version-length)
          ;; Allow both vX.y.z and X.y.z
          (major (if (char=? (string-ref str 0) #\v) 1 0))
          (error "Invalid version minimum length"))))

    ;;; TODO: include the revision in the following function.
    (define (version->string ver)
      (string-append
        (number->string (macro-version-major ver))
        "."
        (number->string (macro-version-minor ver))
        "."
        (number->string (macro-version-patch ver))))

    (define (version=? ver1 ver2)
      (and
        (=
          (macro-version-major ver1)
          (macro-version-major ver2))
        (=
          (macro-version-minor ver1)
          (macro-version-minor ver2))
        (=
          (macro-version-patch ver1)
          (macro-version-patch ver2))))

    (define (version>? ver1 ver2)
      (or (>
            (macro-version-major ver1)
            (macro-version-major ver2))
          (and (=
                 (macro-version-major ver1)
                 (macro-version-major ver2))
               (or
                 (> (macro-version-minor ver1)
                    (macro-version-minor ver2))
                 (and
                   (= (macro-version-minor ver1)
                      (macro-version-minor ver2))
                   (> (macro-version-patch ver1)
                      (macro-version-patch ver2)))))))))



