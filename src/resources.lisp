(in-package #:nova)
(cl-annot:enable-annot-syntax)

(defvar *content-path*)
(defparameter *local-project?* nil)

(defun resolve-path (path)
  (let ((dir (pathname-directory path)))
    (if (and dir (eq :absolute (car dir)))
        path
        (merge-pathnames path *content-path*))))

@export
(defun set-content-path (path)
  (setf *content-path* (make-pathname :defaults path)))

@export
(defun system-source-directory (system-designator)
  (if *local-project?*
      "./"
      (asdf:system-source-directory system-designator)))

@export
(defun save-application (filename toplevel-function)
  (setf *local-project?* t)
  (ccl:save-application filename
                        :toplevel-function toplevel-function
                        :application-type :gui
                        :prepend-kernel t))
