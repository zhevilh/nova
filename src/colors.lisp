(in-package #:nova)
(cl-annot:enable-annot-syntax)

(defclassi color () r g b a)

@export
(defun color (r g b &optional (a))
  (make-instance 'color :r r :g g :b b :a a))

@export
(defun alpha (new-alpha color)
  (with-new (a) color
    (setf a new-alpha)))

(defmacro define-colors (&body definitions)
  `(progn
     ,.(mapcar (build-lambda
		 (destructuring-bind (name &rest color) %
		   (let ((name (intern (concatenate 'string "+" (string name) "+"))))
		     `(progn
			(defparameter ,name (color ,@color))
			(export ',name)))))
	       definitions)))

(define-colors
  (black 0 0 0)
  (white 255 255 255)
  (red 255 0 0)
  (green 0 255 0)
  (blue 0 0 255)
  (yellow 255 255 0)
  (deep-pink 255 20 147)
  (hot-pink 255 105 180))
