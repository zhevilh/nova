(in-package #:nova)
(cl-annot:enable-annot-syntax)

@export-class
(defclassi point () (x 0) (y 0))
@export-class
(defclassi size () (w 0) (h 0))
@export-class
(defclassi area (point size))

(defmacro define-map-function (function-name
			       &rest accessors)
  `(defun ,function-name (f &rest instances)
     (with-new ,accessors (car instances)
       ,.(mapcar (lambda (a)
		   `(setf ,a (apply (curry #'funcall f)
				    (mapcar #',a instances))))
		 accessors))))

@export
(defun point (x y)
  (make-instance 'point :x x :y y))

@export
(define-map-function map-point x y)

@export
(defun size (w h)
  (make-instance 'size :w w :h h))

@export
(define-map-function map-size w h)

@export
(defun area (x y w h)
  (make-instance 'area :x x :y y :w w :h h))

@export
(define-map-function map-area x y w h)

@export
(defun size->area (size)
  (area 0 0 (w size) (h size)))

@export
(defun make-area (point size)
  (with-slots (x y) point
    (with-slots (w h) size
      (area x y w h))))

@export
(defun rectangle (left top right bottom)
  (area left top (- right left) (- bottom top)))

@export
(defun left (area) (x area))
@export
(defun right (area) (with-slots (x w) area
		      (+ x w)))
@export
(defun top (area) (y area))
@export
(defun bottom (area) (with-slots (y h) area
		       (+ y h)))

@export
(defun in-area? (point area)
  (with-slots (x y) point
    (and (>= x (left area))
	 (<= x (right area))
	 (>= y (top area))
	 (<= y (bottom area)))))

@export
(defun area-intersect? (area1 area2)
  (or (< (left area2) (right area1))
      (< (top area2) (bottom area1))
      (< (left area1) (right area2))
      (< (top area1) (bottom area2))))

@export
(defun area-intersection (area1 area2)
  (when (area-intersect? area1 area2)
    (let ((top (max (top area1) (top area2)))
	  (bottom (min (bottom area1) (bottom area2)))
	  (left (max (left area1) (left area2)))
	  (right (min (right area1) (right area2))))
      (area left top (- right left) (- bottom top)))))

@export
(defun scale-size (scale-ratio size)
  (map-size (lambda (x) (* scale-ratio x)) size))

@export
(defun get-fit-scale (source-size target-size)
  (with-slots ((sh h) (sw w)) source-size
    (with-slots ((th h) (tw w)) target-size
      (min (/ th sh)
	   (/ tw sw)))))

@export
(defun align-in (source-size target-area
		 &key
		   (h-align :center) (v-align :center)
		   (h-pad 0) (v-pad 0))
  (with-slots ((tx x) (ty y) (tw w) (th h)) target-area
    (with-new (x y w h) (if (typep source-size 'area)
			    source-size
			    (change-class source-size 'area))
      (setf x tx)
      (setf y ty)
      (ecase h-align
	(:left (incf x h-pad))
	(:right (incf x (- tw w h-pad)))
	(:center (incf x (+ h-pad (/ (- tw w) 2)))))
      (ecase v-align
	(:top (incf y v-pad))
	(:bottom (incf y (- th h v-pad)))
	(:center (incf y (+ v-pad (/ (- th h) 2))))))))

@export
(defun fit-size (source-size target-size &key shrink-only?)
  (with-slots ((tw w) (th h)) target-size
    (with-slots (w h) source-size
      (scale-size (-> (min (/ tw w) (/ th h))
		      (if shrink-only? (min 1 %) %))
		  source-size))))
