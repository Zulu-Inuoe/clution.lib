;;;clution.lib - project development tools for CL
;;;Written in 2018 by Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>
;;;
;;;To the extent possible under law, the author(s) have dedicated all copyright
;;;and related and neighboring rights to this software to the public domain
;;;worldwide. This software is distributed without any warranty.
;;;You should have received a copy of the CC0 Public Domain Dedication along
;;;with this software. If not, see
;;;<http://creativecommons.org/publicdomain/zero/1.0/>.

(in-package #:clution.lib.clu)

(defun %coerce-dir-path (path)
  (etypecase path
    (null nil)
    (string-designator (list (string path)))
    (enumerable (select path #'string))))

(defun %dir-node-p (node)
  (and (%list-node-p node)
       (%symbol-node-p (sexp-list-nth node 0))
       (sexp-symbol-match (sexp-list-nth node 0) "DIR" :keyword)))

(defun dir-node-name (node)
  (sexp-list-getf node "NAME"))

(defun dir-node-items (node)
  (sexp-list-getf node "ITEMS"))

(defun dir-ensure-items (node)
  (unless (dir-node-items node)
    (%append-to-list-node node (make-instance 'sexp-symbol-node :name "ITEMS" :package :keyword))
    (%append-to-list-node node (make-instance 'sexp-list-node))))

(defun dir-by-path (dir dir-path)
  (setf dir-path (%coerce-dir-path dir-path))
  (labels ((recurse (node path)
             (cond
               ((move-next path)
                (when-let* ((items (dir-node-items node))
                            (child (->  (vchildren items)
                                        (where #'%dir-node-p)
                                        (where (lambda (child)
                                                 (string= (string-node-string (dir-node-name child))
                                                          (current path))))
                                        (efirst))))
                  (recurse child path)))
               (t
                node))))
    (recurse dir (get-enumerator dir-path))))

(defun dir-node-plist (node path)
  (let ((plist (list)))
    (push :dir plist)
    (flet ((add-prop (prop-name prop-value)
             (push prop-name plist)
             (push prop-value plist)))
      (add-prop :name (string-node-string (dir-node-name node)))
      (add-prop :items
                (when-let ((items (dir-node-items node)))
                  (-> (vchildren items)
                      (select (lambda (i) (clu-item-plist i path)))
                      (to-list)))))
    (nreverse plist)))

(defun %system-node-p (node)
  (and (%list-node-p node)
       (%symbol-node-p (sexp-list-nth node 0))
       (sexp-symbol-match (sexp-list-nth node 0) "SYSTEM" :keyword)))

(defun system-node-path (node)
  (sexp-list-getf node "PATH"))

(defun system-node-type (node)
  (sexp-list-getf node "TYPE"))

(defun system-node-plist (node path)
  (let ((plist (list)))
    (push :system plist)
    (flet ((add-prop (prop-name prop-value)
             (push prop-name plist)
             (push prop-value plist)))
      (add-prop :path
                (namestring
                 (%expand-pathname
                  (string-node-string (system-node-path node))
                  (uiop:pathname-directory-pathname path))))
      (add-prop :type (make-keyword (symbol-node-name (system-node-type node)))))
    (nreverse plist)))

(defun clu-clu-dir (clu)
  (sexp-list-getf clu "CLU-DIR"))

(defun clu-item-plist (item path)
  (cond
    ((%system-node-p item)
     (system-node-plist item path))
    ((%dir-node-p item)
     (dir-node-plist item path))
    (t
     (error "unrecognized item type: ~A" item))))

(defun clu-items (clu)
  (dir-node-items clu))

(defun clu-ensure-items (clu)
  (dir-ensure-items clu))

(defun %clu-node-p (node)
  (and (%list-node-p node)
       (%symbol-node-p (sexp-list-nth node 0))
       (sexp-symbol-match  (sexp-list-nth node 0) "CLUTION")))

(defun clu-add-dir (clu dir-path name)
  (let ((dir (dir-by-path clu dir-path)))
    (unless dir
      (error "dir does not exist at path: '~A'" dir-path))
    (dir-ensure-items dir)
    (let ((items (dir-node-items dir)))
      (when (efirst* (vchildren items) (lambda (c) (and (%dir-node-p c) (string= (string-node-string (dir-node-name c)) name))))
        (error "dir already contains dir with name '~A'" name))
      (%append-to-list-node
       items
       (make-instance
        'sexp-list-node
        :children
        (list
         (make-instance 'sexp-symbol-node :name "DIR" :package :keyword)
         (%make-indent-node (+ (%node-indention-level items) 2) nil)
         (make-instance 'sexp-symbol-node :name "NAME" :package :keyword)
         (make-instance 'sexp-whitespace-node :text " ")
         (make-instance 'sexp-string-node :string name)
         (%make-indent-node (+ (%node-indention-level items) 2) nil)
         (make-instance 'sexp-symbol-node :name "ITEMS" :package :keyword)
         (%make-indent-node (+ (%node-indention-level items) 2) nil)
         (make-instance 'sexp-list-node)))))))

(defun clu-remove-dir (clu dir-path)
  (let ((dir (dir-by-path clu dir-path)))
    (unless dir
      (error "dir does not exist at path: '~A'" dir-path))
    (%delete-from-list-node (sexp-node-parent dir) dir)))

(defun clu-add-system (clu dir-path system-path system-type)
  (let ((dir (dir-by-path clu dir-path)))
    (unless dir
      (error "dir does not exist at path: '~A'" dir-path))
    (dir-ensure-items dir)
    (let ((items (dir-node-items dir)))
      (when (efirst* (vchildren items)
                     (lambda (sys-node)
                       (%pathname-equal (string-node-string (system-node-path sys-node))
                                        system-path)))
        (error "clu already contains system '~A'" system-path))
      (%append-to-list-node
       items
       (make-instance
        'sexp-list-node
        :children
        (list
         (make-instance 'sexp-symbol-node :name "SYSTEM" :package :keyword)
         (%make-indent-node (+ (%node-indention-level items) 2) nil)
         (make-instance 'sexp-symbol-node :name "PATH" :package :keyword)
         (make-instance 'sexp-whitespace-node :text " ")
         (make-instance 'sexp-string-node :string (namestring system-path))
         (%make-indent-node (+ (%node-indention-level items) 2) nil)
         (make-instance 'sexp-symbol-node :name "TYPE" :package :keyword)
         (make-instance 'sexp-whitespace-node :text " ")
         (make-instance 'sexp-symbol-node :name (symbol-name system-type) :package :keyword)))))))

(defun clu-remove-system (clu dir-path system-path)
  (let ((dir (dir-by-path clu dir-path)))
    (unless dir
      (error "dir does not exist at path: '~A'" dir-path))
    (let ((items (dir-node-items dir)))
      (unless items
        (error "no such system '~A' at path '~A'" system-path dir-path))
      (let ((system (efirst* (vchildren items)
                             (lambda (sys-node)
                               (%pathname-equal (string-node-string (system-node-path sys-node))
                                                system-path)))))
        (unless system
          (error "no such system '~A' at path '~A'" system-path dir-path))
        (%delete-from-list-node items system)))))

(defun clu-plist (clu path)
  (let ((plist (list))
        (pathname (namestring path)))
    (push :clution plist)
    (flet ((add-prop (prop-name prop-value)
             (push prop-name plist)
             (push prop-value plist)))
      (add-prop :path pathname)
      (add-prop :items
                (when-let ((items (clu-items clu)))
                  (-> (vchildren items)
                      (select (lambda (i) (clu-item-plist i path)))
                      (to-list))))
      (add-prop :clu-dir
                (namestring
                 (%expand-pathname
                  (if-let ((clu-dir (clu-clu-dir clu)))
                    (string-node-string clu-dir)
                    ".clu/")
                  (uiop:pathname-directory-pathname path)))))
    (nreverse plist)))

(defclass clu-file ()
  ((path
    :type pathname
    :initarg :path
    :initform (error "clu-file: must supply path")
    :reader clu-file-path)
   (eol-style
    :type (member :windows :unix :old-mac)
    :reader clu-file-eol-style)
   (nodes
    :type list
    :accessor clu-file-nodes)))

(defmethod initialize-instance :after ((obj clu-file) &key)
  (setf (slot-value obj 'nodes) (to-list (%parse-sexp-file (clu-file-path obj)))
        (slot-value obj 'eol-style) (%guess-eol-style (clu-file-path obj) :unix)))

(defun read-clu-file (path)
  (make-instance 'clu-file :path (pathname path)))

(defun write-clu-file (clu-file stream)
  (dolist (node (clu-file-nodes clu-file))
    (%write-node node stream)))

(defun clu-file-clu (clu-file)
  (efirst (clu-file-nodes clu-file) #'%clu-node-p))

(defun clu-file-ensure-clu (clu-file
                            &aux
                              (*%eol-sequence* (cdr (assoc (clu-file-eol-style clu-file) *%eol-sequences*))))
  (unless (clu-file-clu clu-file)
    (when (and (any (clu-file-nodes clu-file))
               (not (%opaque-node-ends-with-newline (elast (clu-file-nodes clu-file)))))
      (setf (clu-file-nodes clu-file)
            (nconc (clu-file-nodes clu-file)
                   (list
                    (%make-indent-node 0 nil)))))
    (setf (clu-file-nodes clu-file)
          (nconc (clu-file-nodes clu-file)
                 (list
                  (make-instance
                   'sexp-list-node
                   :children
                   (list (make-instance 'sexp-symbol-node :name "CLUTION" :package :keyword)))
                  (%make-indent-node 0 nil))))))

(defun clu-file-add-dir (clu-file dir-path name)
  (clu-file-ensure-clu clu-file)
  (let* ((*%eol-sequence* (cdr (assoc (clu-file-eol-style clu-file) *%eol-sequences*)))
         (clu (clu-file-clu clu-file)))
    (clu-add-dir clu dir-path name)))

(defun clu-file-remove-dir (clu-file dir-path)
  (let ((*%eol-sequence* (cdr (assoc (clu-file-eol-style clu-file) *%eol-sequences*)))
        (clu (clu-file-clu clu-file)))
    (clu-remove-dir clu dir-path)))

(defun clu-file-add-system (clu-file dir-path system-path type)
  (clu-file-ensure-clu clu-file)
  (let ((*%eol-sequence* (cdr (assoc (clu-file-eol-style clu-file) *%eol-sequences*)))
        (clu (clu-file-clu clu-file))
        (rel-path (%relative-pathname system-path (clu-file-path clu-file))))
    (clu-add-system clu dir-path rel-path type)))

(defun clu-file-remove-system (clu-file dir-path system-path)
  (let ((*%eol-sequence* (cdr (assoc (clu-file-eol-style clu-file) *%eol-sequences*)))
        (clu (clu-file-clu clu-file))
        (rel-path (%relative-pathname system-path (clu-file-path clu-file))))
    (clu-remove-system clu dir-path rel-path)))

(defun clu-file-plist (clu-file)
  (when-let ((clu (clu-file-clu clu-file)))
    (clu-plist clu (clu-file-path clu-file))))
