;;;clution.lib - project development tools for CL
;;;Written in 2018 by Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>
;;;
;;;To the extent possible under law, the author(s) have dedicated all copyright
;;;and related and neighboring rights to this software to the public domain
;;;worldwide. This software is distributed without any warranty.
;;;You should have received a copy of the CC0 Public Domain Dedication along
;;;with this software. If not, see
;;;<http://creativecommons.org/publicdomain/zero/1.0/>.

(in-package #:%clution.lib)

(defun %whitespacep (char)
  "Determine if `char' is a whitespace character"
  (check-type char character)
  ;;Abuse behavior of peek-char so we can use the
  ;;implementation's notion of what whitespace is
  (with-input-from-string (s (string char))
    (not (peek-char t s nil nil))))

(defun %terminating-p (char)
  "Determine if `char' is a 'terminating' character (delimits a token).
That is, if it is either a macro character that is terminatting, or if it is whitespace."
  (check-type char character)
  (multiple-value-bind (fn non-terminating-p)
      (get-macro-character char)
    (if fn
        (not non-terminating-p)
        (%whitespacep char))))

(defun %cell-before (item list &key (test #'eql))
  (cond
    ((funcall test item (car list))
     nil)
    (t
     (loop
       :for cell := list :then (cdr cell)
       :while (cdr cell)
       :when (funcall test item (cadr cell))
         :return cell))))

(defun %cell-after (item list &key (test #'eql))
  (cdr (member item list :test test)))

(defun %item-before (item list &key default (test #'eql))
  (if-let ((cell (%cell-before item list :test test)))
    (car cell)
    default))

(defun %item-after (item list &key default (test #'eql))
  (if-let ((cell (%cell-after item list :test test)))
    (car cell)
    default))

(defvar *%eol-sequences*
  (list
   (cons :windows '(#\Return #\Newline))
   (cons :unix '(#\Newline))
   (cons :old-mac '(#\Return))))

(defvar *%eol-sequence* '(#\Newline))

(defun %guess-eol-style (file-path &optional (default :unix))
  (let ((scores
          (list
           (cons :windows 0)
           (cons :unix 0)
           (cons :old-mac 0))))
    (with-input-from-file (stream file-path :external-format :utf-8)
      (loop
        :for c := (read-char stream nil nil)
        :while c
        :do
           (switch (c)
             (#\Return
              (switch ((read-char stream nil nil))
                (#\Newline
                 (incf (cdr (assoc :windows scores))))
                (t
                 (incf (cdr (assoc :old-mac scores))))))
             (#\Newline
              (incf (cdr (assoc :unix scores)))))))

    (setf scores (stable-sort scores #'> :key #'cdr))
    (cond
      ((every (lambda (cell) (zerop (cdr cell))) scores)
       ;;All zero
       default)
      (t ;;Possibly mixed, but return most popular
       (caar scores)))))

(defun %read-from-file (filename &rest args
                        &key
                          (eof-error-p nil)
                          (eof-value nil)
                          (if-does-not-exist nil if-does-not-exist-p)
                          (direction nil direction-p)
                        &allow-other-keys)
  (declare (ignore if-does-not-exist direction))
  (when if-does-not-exist-p
    (error "Can't specify :IF-DOES-NOT-EXIST for READ-FROM-FILE."))
  (when direction-p
    (error "Can't specify :DIRECTION for READ-FROM-FILE."))

  (let ((file-stream (apply #'open filename :direction :input :if-does-not-exist nil args)))
    (cond
      (file-stream
       (unwind-protect
            (read file-stream eof-error-p eof-value)
         (close file-stream)))
      (t
       (if eof-error-p
           (error 'end-of-file :stream nil)
           eof-value)))))

(defun %resolve-directives (pathname
                            &aux
                              (device (pathname-device pathname))
                              (host (pathname-host pathname)))
  "Attempts to resolve special directives in the directory component of `pathname'.
:UP and :BACK will be interpreted as 'eliminating' the next directory up.
:HOME will resolved to `user-homedir-pathname', using the same `host' component as `pathname'

eg:
/foo/bar/baz/../ => /foo/bar/

Note that the parent directory of root is itself. Therefore
/../../../../ => /

When `pathname' is relative, any such directives that cannot be resolved will be left intact
foo/../../bar/ => ../bar/"
  (labels ((recurse (components)
             (cond
               ((null components) nil)
               ((member (car components) '(:up :back))
                (let ((res (recurse (cdr components))))
                  (cond
                    ((null res)
                     (list (car components) :relative))
                    ((member (car res) '(:up :back :relative))
                     ;;keep it and ourselves
                     (cons (car components) res))
                    ((eq (car res) :absolute)
                     (list :absolute))
                    (t
                     ;;skip it
                     (cdr res)))))
               ((eq (car components) :home)
                (let ((homedir (user-homedir-pathname host)))
                  (setf device (pathname-device homedir)
                        host (pathname-host homedir))
                  (reverse (uiop:normalize-pathname-directory-component
                            (pathname-directory homedir)))))
               (t
                (cons (car components) (recurse (cdr components)))))))
    (make-pathname
     :directory
     (uiop:normalize-pathname-directory-component
      (reverse
       (recurse
        (reverse
         (uiop:normalize-pathname-directory-component
          (pathname-directory pathname))))))
     :device device
     :host host
     :defaults pathname)))

(defun %expand-pathname (pathname &optional (base *default-pathname-defaults*))
  (%resolve-directives
   (uiop:merge-pathnames* pathname (uiop:merge-pathnames* base))))

(defun %relative-pathname (pathname &optional (base *default-pathname-defaults*))
  "This function tries to return a relative name that is equivalent to filename, assuming the result will be interpreted relative to directory (an absolute directory name or directory file name). If directory is omitted or nil, it defaults to the current buffer's default directory.

On some operating systems, an absolute file name begins with a device name. On such systems, filename has no relative equivalent based on directory if they start with two different device names. In this case, file-relative-name returns filename in absolute form."
  (setf pathname (%expand-pathname pathname)
        base (uiop:pathname-directory-pathname (%expand-pathname base)))
  (cond
    ((not (equalp (pathname-device pathname) (pathname-device base)))
     pathname)
    ((uiop:pathname-equal pathname base)
     (if (uiop:directory-pathname-p pathname)
         #P"./"
         #P"."))
    ((uiop:subpathp pathname base)
     (uiop:enough-pathname pathname base))
    (t
     (let ((result-dir (list :relative)))
       (labels ((climb (base)
                  (cond
                    ((uiop:pathname-equal pathname base)
                     (make-pathname
                      :device nil
                      :host nil
                      :directory (uiop:denormalize-pathname-directory-component (nreverse result-dir))
                      :defaults base))
                    ((uiop:subpathp pathname base)
                     (let* ((subpath (uiop:enough-pathname pathname base)))
                       (make-pathname
                        :device nil
                        :host nil
                        :directory
                        (uiop:denormalize-pathname-directory-component
                         (append
                          (nreverse result-dir)
                          (rest
                           (uiop:normalize-pathname-directory-component
                            (pathname-directory subpath)))))
                        :defaults subpath)))
                    (t
                     (push :up result-dir)
                     (climb
                      (make-pathname
                       :directory
                       (uiop:denormalize-pathname-directory-component
                        (butlast
                         (uiop:normalize-pathname-directory-component
                          (pathname-directory base))))
                       :defaults base))))))
         (climb base))))))

(defun %ensure-relative-pathname (pathname &optional (base *default-pathname-defaults*))
  (let ((ret (%relative-pathname pathname base)))
    (when (uiop:absolute-pathname-p pathname)
      (error "could not make pathname relative: ~A" pathname))
    ret))