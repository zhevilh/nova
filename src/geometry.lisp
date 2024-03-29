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

(defmacro define-list-function (function-name
                                &rest accessors)
  `(defun ,function-name (instance)
     (with-accessors ,(loop for a in accessors
                            collect (list a a))
         instance
       (list ,@accessors))))

@export
(defun point (x y)
  (make-instance 'point :x x :y y))

(defmethod print-object ((p point) stream)
  (format stream "#<POINT (~a ~a)>"
          (x p) (y p)))

@export
(define-map-function map-point x y)
@export
(define-equal-function point= #'= x y)
@export
(define-list-function list-point x y)

@export
(defun point-move (point x y)
  (with-new ((px x) (py y)) point
    (incf px x)
    (incf py y)))

@export
(defun size (w h)
  (make-instance 'size :w w :h h))

(defmethod print-object ((s size) stream)
  (format stream "#<SIZE (~a ~a)>"
          (w s) (h s)))

@export
(define-map-function map-size w h)
@export
(define-equal-function size= #'= w h)
@export
(define-list-function list-size w h)

@export
(defun area (x y w h)
  (make-instance 'area :x x
                       :y y
                       :w w
                       :h h))

(defmethod print-object ((a area) stream)
  (format stream "#<AREA (~a ~a ~a ~a)>"
          (x a) (y a) (w a) (h a)))

@export
(define-map-function map-area x y w h)
@export
(define-equal-function area= #'= x y w h)
@export
(define-list-function list-area x y w h)

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
(defun area-left (area) (x area))
@export
(defun area-right (area) (with-slots (x w) area
		      (+ x w)))
@export
(defun area-top (area) (y area))
@export
(defun area-bottom (area) (with-slots (y h) area
		       (+ y h)))

@export
(defun in-area? (point area)
  (with-slots (x y) point
    (and (>= x (area-left area))
	 (<= x (area-right area))
	 (>= y (area-top area))
	 (<= y (area-bottom area)))))

@export
(defun area-intersect? (area1 area2)
  (or (< (area-left area2) (area-right area1))
      (< (area-top area2) (area-bottom area1))
      (< (area-left area1) (area-right area2))
      (< (area-top area1) (area-bottom area2))))

@export
(defun area-intersection (area1 area2)
  (when (area-intersect? area1 area2)
    (let ((top (max (area-top area1) (area-top area2)))
	  (bottom (min (area-bottom area1) (area-bottom area2)))
	  (left (max (area-left area1) (area-left area2)))
	  (right (min (area-right area1) (area-right area2))))
      (area left top (- right left) (- bottom top)))))

@export
(defun scale-area (scale-ratio area)
  (map-area (lambda (x) (* scale-ratio x)) area))

@export
(defun grow-area (value area)
  (with-access (x y w h) area
    (area (- x value) (- y value) (+ w (* 2 value)) (+ h (* 2 value)))))

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

@export
(defun split-size (source-size target-size)
  (with-slots ((tw w) (th h)) target-size
    (with-slots (w h) source-size
      (loop with count-w = (floor w tw)
            with count-h = (floor h th)
            for ih from 0 to (1- count-h)
            collect (loop for iw from 0 to (1- count-w)
                          collect (area (* tw iw) (* th ih) tw th))))))
