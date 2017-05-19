(in-package :nova)
(annot:enable-annot-syntax)

@export-class
(defclassi color ()
  (r 0) (g 0) (b 0) (a 0))

(defvar *colors* (make-hash-table))

(defun color (r g b a)
  (make-instance 'color :r r :g g :b b :a a))

@export
(defun resolve-color (color-or-keyword)
  (if (keywordp color-or-keyword)
      (gethash color-or-keyword *colors*)
      color-or-keyword))

@export
(defmacro defcolor (keyword r g b)
  `(setf (gethash ,keyword *colors*) (color ,r ,g ,b 0)))

@export
(defmacro new-color (color &key r g b a)
  `(make-new (resolve-color ,color)
	     ,(if r `(r ,r))
	     ,(if g `(g ,g))
	     ,(if b `(b ,b))
	     ,(if a `(a ,a))))

@export
(defun color->v3 (color)
  (with-slots (r g b a) (resolve-color color)
    (rtg-math:v3! r g b)))

(defcolor :white 1.0 1.0 1.0)
(defcolor :black 0 0 0)
(defcolor :red 1.0 0 0)
(defcolor :green 0 1.0 0)
(defcolor :blue 0 0 1.0)
(defcolor :yellow 1.0 1.0 0)
(defcolor :fuschia 1.0 0 1.0)
