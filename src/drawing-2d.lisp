(in-package :nova-2d)
(annot:enable-annot-syntax)

;; Geometry

@export-class
(defclassi point () (x 0) (y 0))
@export-class
(defclassi size () (w 0) (h 0))
@export-class
(defclassi area (point size))

@export
(defun point (x y)
  "number -> number -> point"
  (make-instance 'point :x x :y y))

@export
(defun size (w h)
  "integer -> integer -> size"
  (make-instance 'size :w w :h h))

@export
(defun scale-to-fit (s &key w h)
  "size s => s -> integer -> integer -> s"
  (when (or w h)
    (with-new ((sw w) (sh h)) s
      (let* ((w-ratio (if w (/ w sw) 1))
	     (h-ratio (if h (/ h sh) 1))
	     (min-ratio (min w-ratio h-ratio)))
	(setf sw (floor (* min-ratio sw))
	      sh (floor (* min-ratio sh)))))))

@export
(defun area (x y w h)
  "number -> number -> integer -> integer -> area"
  (make-instance 'area :x x :y y :w w :h h))

@export
(defun make-area (point size)
  "point -> size -> area"
  (with-slots (x y) point
    (with-slots (w h) size
      (area x y w h))))

@export
(defun make-centered-area (point size)
  "point -> size -> area"
  (with-slots (x y) point
    (with-slots (w h) size
      (area (- x (floor (* 0.5 w)))
	    (- y (floor (* 0.5 h)))
	    w h))))

@export
(defun center (area)
  "area -> point"
  (with-slots (x y w h) area
    (point (+ x (floor (* 0.5 w)))
	   (+ y (floor (* 0.5 h))))))

;; Textures and sprites

@export-class
(defclassi texture (size) gl-texture)
@export-class
(defclassi sprite (area) texture)

@export
(defun texture! (filename &key scale-to)
  "filename -> :scale-to size -> texture"
  (when scale-to
    (error "scale-to is not currently supported in texture loading."))
  (sdl-surface->texture (sdl2-image:load-image filename)))

@export
(defun sdl-surface->texture (surface)
  (let* ((surface-w (sdl2:surface-width surface))
	 (surface-h (sdl2:surface-height surface))
	 (mode (if (= 4 (plus-c:c-ref surface
				      sdl2-ffi:sdl-surface
				      :format :bytes-per-pixel))
		   :rgba
		   :rgb))
	 (gl-texture (make-texture-2d (sdl2:surface-pixels surface)
				      mode mode surface-w surface-h)))
    (ether:finalize
	(make-instance 'texture
		       :gl-texture gl-texture
		       :w surface-w :h surface-h)
      (when-gl-context
	(gl:delete-textures (list gl-texture))))))

(defun split-sprite (sprite split-size)
  (with-slots ((split-w w) (split-h h)) split-size
    (let ((count-w (floor (/ (w sprite) split-w)))
	  (count-h (floor (/ (h sprite) split-h)))
	  sprites)
      (dotimes (ih count-h)
	(dotimes (iw count-w)
	  (push (with-new (x y w h) sprite
		  (setf x (* split-w iw)
			y (* split-h ih)
			w split-w
			h split-h))
		sprites)))
      (nreverse sprites))))

@export
(defun sprite (texture &key source-area)
  "texture -> :source-area area -> sprite"
  (let ((source-area (or source-area
			 (area 0 0 (w texture) (h texture)))))
    (with-slots (x y w h) source-area
      (make-instance 'sprite
		     :x x :y y :w w :h h
		     :texture texture))))

@export
(defun sprites (texture &key source-area split-size)
  "texture -> :source-area area -> :split-size size -> sprite list"
  (if split-size
      (split-sprite (sprite texture :source-area source-area)
		    split-size)
      (list (sprite texture :source-area source-area))))

@export
(defun draw-sprite! (sprite &key place-at center-at fit-in
			      scale-to (rotate 0.0) (color :white))
  (when (not (xor place-at center-at fit-in))
    (error "Must specify one of place-at, center-at or fit-in."))
  (when (and fit-in scale-to)
    (error "Cannot use both fit-in and scale."))
  (with-slots (x y w h)
      (cond (place-at (make-area place-at (or scale-to sprite)))
	    (center-at (make-centered-area place-at (or scale-to sprite)))
	    (fit-in (with-slots ((fw w) (fh h)) fit-in
		      (make-centered-area (center fit-in)
					  (scale-to-fit sprite :w fw :h fh)))))
    (with-slots ((sx x) (sy y) (sh h) (sw w) texture) sprite
      (with-slots ((tw w) (th h) gl-texture) texture
	(with-slots (r g b) (resolve-color color)
	  (gl-draw-sprite! gl-texture x y w h
			   sx sy sw sh tw th
			   (rtg-math:radians rotate) r g b))))))


