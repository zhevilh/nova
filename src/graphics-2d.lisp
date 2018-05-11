(in-package #:nova)
(cl-annot:enable-annot-syntax)

(defvar *renderer*)

(defmacro with-renderer (window &body body)
  `(sdl2:with-renderer (*renderer* ,window)
     ,@body))

(defun area-to-rect (area)
  (when area
    (with-slots (x y w h) area
      (sdl2:make-rect (round x) (round y) (round w) (round h)))))

@export
(defun load-image (path)
  (sdl2-image:load-image (resolve-path path)))

@export
(defun image-size (image)
  (size (sdl2:surface-width image)
	(sdl2:surface-height image)))

@export
(defun sub-image (area image)
  (with-slots (x y w h) area
    (let ((target (sdl2:create-rgb-surface w h 16)))
      (sdl2:blit-surface image (sdl2:make-rect x y w h)
			 target (sdl2:make-rect 0 0 w h))
      
      target)))

@export
(defun load-texture (source
		     &key
		       renderer
		       (scale-filtering :nearest)
		       blend-mode)
  (sdl2-ffi.functions:sdl-set-hint sdl2-ffi:+sdl-hint-render-scale-quality+
                                   (ecase scale-filtering
                                     (:nearest "0")
                                     (:linear "1")
                                     (:anisotropic "2")))
  (let* ((surface (if (or (stringp source)
                          (pathnamep source))
                      (load-image source)
                      source))
         (texture (sdl2:create-texture-from-surface
                   (or renderer *renderer*)
                   surface)))
    (when blend-mode
      (sdl2:set-texture-blend-mode texture blend-mode))
    texture))

@export
(defun texture-size (texture)
  (size (sdl2:texture-width texture)
	(sdl2:texture-height texture)))

(defmacro with-drawing-color (color renderer &body body)
  `(if color
       (with-slots (r g b a) ,color
	 (unwind-protect
	      (progn (sdl2:set-render-draw-color ,renderer r g b a)
		     ,@body)
	   (sdl2:set-render-draw-color ,renderer 0 0 0 0)))
       (progn,@body)))

@export
(defun clear-screen (&key renderer color)
  (let ((renderer (or renderer *renderer*)))
    (with-drawing-color color renderer
      (sdl2:render-clear renderer))))

@export
(defun clear-area (area &key renderer color)
  (let ((renderer (or renderer *renderer*)))
    (with-drawing-color color renderer
      (sdl2:render-fill-rect renderer (area-to-rect area)))))

(defmacro with-texture-color (texture color &body body)
  `(if ,color
       (with-slots (r g b a) ,color
	 (unwind-protect
	      (progn (sdl2:set-texture-color-mod ,texture r g b)
		     (sdl2:set-texture-alpha-mod ,texture 255)
		     ,@body)
	   (sdl2:set-texture-color-mod ,texture 255 255 255)
	   (sdl2:set-texture-alpha-mod ,texture 255)))
       (progn ,@body)))

@export
(defun draw-texture (texture target-area-or-point
		     &key renderer source-area color)
  (let ((renderer (or renderer *renderer*))
	(target-area (if (typep target-area-or-point 'area)
			 target-area-or-point
			 (make-area target-area-or-point (texture-size texture)))))
    (with-texture-color texture color
      (sdl2:render-copy-ex renderer texture
			   :source-rect (area-to-rect source-area)
			   :dest-rect (area-to-rect target-area)))))

@export
(defun free-texture (texture)
  (sdl2:destroy-texture texture)
  (tg:gc))

(defclassi sprite () sprite-texture source-area)

@export
(defun sprite (texture source-area)
  (make-instance 'sprite :sprite-texture texture :source-area source-area))

@export
(defun split-sprite (size sprite)
  (with-slots ((sw w) (sh h)) size
    (with-slots (x y w h) (source-area sprite)
      (let ((count-w (floor (/ w sw)))
	    (count-h (floor (/ h sh))))
	(loop for ih from 0 to (1- count-h)
	   append (loop for iw from 0 to (1- count-w)
		     collect (with-new (source-area) sprite
			       (setf source-area
				     (area (+ (* sw iw) x)
					   (+ (* sh ih) y)
					   sw sh)))))))))
