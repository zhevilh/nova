(in-package :nova)

(export '(point x y))
(defclass point ()
  ((x :initarg :x :initform 0 :reader x)
   (y :initarg :y :initform 0 :reader y)))

(defun point (x y)
  (make-instance 'point :x x :y y))

(export '(size w h))
(defclass size ()
  ((w :initarg :w :reader w)
   (h :initarg :h :reader h)))

(defun size (w h)
  (make-instance 'size :w w :h h))

(export 'fit-in)
(defun fit-in (size fit-w fit-h)
  "size a => a -> int -> int -> a"
  (with-new (h w) size
    (let ((ratio (min 1 (/ fit-h h) (/ fit-w w))))
      (setf h (floor (* ratio h)))
      (setf w (floor (* ratio w))))))

(export 'area)
(defclass area (point size) ())

(defun area (x y w h)
  (make-instance 'area :x x :y y :w w :h h))

(export '(sprite texture source-area target-size))
(defclass sprite (size)
  ((texture :initarg :texture :reader texture)
   (source-area :initarg :source-area :reader source-area)))

(defun sprite (texture &key source-area target-size)
  (make-instance 'sprite
		 :texture texture
		 :source-area (or source-area (area 0 0
						    (sdl2:texture-width texture)
						    (sdl2:texture-height texture)))
		 :w (if target-size (w target-size) (sdl2:texture-width texture))
		 :h (if target-size (h target-size) (sdl2:texture-height texture))))

(export 'split)
(defun split (split-w split-h sprite)
  "int -> int -> sprite -> sprite list"
  (let ((count-w (floor (/ (w sprite) split-w)))
	(count-h (floor (/ (h sprite) split-h)))
	sprites)
    (dotimes (ih count-h)
      (dotimes (iw count-w)
	(push (with-new (source-area (nw w) (nh h)) sprite
		(setf nw split-w)
		(setf nh split-h)
		(setf source-area (area (* split-w iw)
					(* split-h ih)
					split-w split-h)))
	      sprites)))
    (nreverse sprites)))

(export '(draw-sprite target-point))
(defclass draw-sprite (sprite point) ())

(export 'place-sprite)
(defun place-sprite (sprite x y)
  "sprite -> int -> int -> draw-sprite"
  (change-class (copy-instance sprite) 'draw-sprite
		:x x :y y))

(export 'center-sprite)
(defun center-sprite (sprite x y)
  "sprite -> int -> int -> draw-sprite"
  (with-slots ((sw w) (sh h)) sprite
    (place-sprite sprite
		  (- x (floor (/ sw 2)))
		  (- y (floor (/ sh 2))))))

(export 'center-sprite-in)
(defun center-sprite-in (sprite target-area)
  "sprite -> area -> draw-sprite"
  (with-slots (x y w h) target-area
    (with-slots ((sw w) (sh h)) sprite
      (place-sprite sprite
		    (+ x (floor (/ (- w sw) 2)))
		    (+ y (floor (/ (- h sh) 2)))))))
