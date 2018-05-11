(in-package #:nova)
(cl-annot:enable-annot-syntax)

(defvar *content-path*)

(defun resolve-path (path)
  (let ((dir (pathname-directory path)))
    (if (and dir (eq :absolute (car dir)))
	path
	(merge-pathnames path *content-path*))))

@export
(defun set-content-path (path)
  (setf *content-path* (make-pathname :defaults path)))
