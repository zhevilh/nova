(in-package #:nova)
(cl-annot:enable-annot-syntax)

(defvar *parent-scene*)
(defvar *renderer*)
(defvar *loaded-textures*)

(defmacro with-renderer (window &body body)
  `(sdl2:with-renderer (*renderer* ,window)
     ,@body))

(defmacro with-loaded-textures (&body body)
  `(let ((*loaded-textures* nil))
     (unwind-protect
          (progn ,@body)
       (free-loaded-textures))))

(defun add-loaded-texture (texture)
  (push (tg:make-weak-pointer texture) *loaded-textures*))

(defun free-loaded-textures ()
  (dolist (texture *loaded-textures*)
    (-> (tg:weak-pointer-value texture)
        (when % (free-texture %)))))

(defun area-to-rect (area)
  (when area
    (with-slots (x y w h) area
      (sdl2:make-rect (round x) (round y) (round w) (round h)))))

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
		       blend-mode)
  (multiple-value-bind (surface new-surface?)
      (if (or (stringp source)
              (pathnamep source))
          (values (sdl2-image:load-image (resolve-path source))
                  t)
          source)
    (let* ((texture (sdl2:create-texture-from-surface
                     (or renderer *renderer*)
                     surface)))
      (when new-surface?
        (sdl2:free-surface surface))
      (when blend-mode
        (sdl2:set-texture-blend-mode texture blend-mode))
      ;;(add-loaded-texture texture)
      texture)))

@export
(defun texture-size (texture)
  (size (sdl2:texture-width texture)
	(sdl2:texture-height texture)))

@export
(defun make-texture (width height &key renderer (pixel-format :rgba8888)
                                    blend-mode)
  (let ((texture (sdl2:create-texture (or renderer *renderer*)
                                      pixel-format
                                      :target
                                      (round width)
                                      (round height))))
    (when blend-mode
      (sdl2:set-texture-blend-mode texture blend-mode))
    texture))

@export
(defmacro with-render-texture (texture &body body)
  `(progn
     (sdl2:set-render-target *renderer* ,texture)
     ,@body
     (sdl2:set-render-target *renderer* nil)))

@export
(defmacro with-new-render-texture (size (&key renderer (pixel-format :rgba8888)
                                           blend-mode)
                                   &body body)
  (with-gensyms (texture)
    `(let ((,texture (make-texture (w ,size) (h ,size)
                                   :renderer ,renderer :pixel-format ,pixel-format
                                   :blend-mode ,blend-mode)))
       (with-render-texture ,texture
         ,@body)
       ,texture)))

(defmacro with-drawing-color (color renderer &body body)
  `(if ,color
       (with-slots (r g b a) ,color
	 (unwind-protect
	      (progn (sdl2:set-render-draw-color ,renderer r g b (or a 255))
		     ,@body)
	   (sdl2:set-render-draw-color ,renderer 0 0 0 0)))
       (progn,@body)))

@export
(defun clear-screen (&key renderer color)
  (when (not *parent-scene*)
    (let ((renderer (or renderer *renderer*)))
      (with-drawing-color color renderer
        (sdl2:render-clear renderer)))))

@export
(defun clear-area (area &key renderer color blend-mode)
  (let ((renderer (or renderer *renderer*)))
    (when blend-mode
      (sdl2:set-render-draw-blend-mode renderer blend-mode))
    (with-drawing-color color renderer
      (sdl2:render-fill-rect renderer (area-to-rect area)))
    (when blend-mode
      (sdl2:set-render-draw-blend-mode renderer :none))))

@export
(defun draw-point (point &key renderer color blend-mode)
  (let ((renderer (or renderer *renderer*)))
    (sdl2:set-render-draw-blend-mode renderer blend-mode)
    (with-drawing-color color renderer
      (sdl2:render-draw-point renderer (x point) (y point)))))

@export
(defun draw-rect (area &key renderer color blend-mode (thickness 1))
  (let ((renderer (or renderer *renderer*)))
    (when blend-mode
      (sdl2:set-render-draw-blend-mode renderer blend-mode))
    (with-drawing-color color renderer
      (loop for i from 0 below thickness
            for a = (grow-area (- i) area)
            do (sdl2:render-draw-rect renderer (area-to-rect a))))
    (when blend-mode
      (sdl2:set-render-draw-blend-mode renderer :none))))

@export
(defun draw-circle (point radius
                    &key renderer color (thickness 1)
                      (quadrants (list 0 1 2 3)))
  (let ((renderer (or renderer *renderer*))
        (quadrants (loop for q in (ensure-list quadrants)
                          sum (expt 2 q))))
    #+ () (when blend-mode
            (sdl2:set-render-draw-blend-mode renderer blend-mode))
    (with-drawing-color color renderer
      (dotimes (i thickness)
        (declare (optimize (speed 3)))
        (loop with radius = (+ radius i)
              with diameter = (* radius 2)
              with px = (round (x point))
              with py = (round (y point))
              with x = (- radius 1)
              with y = 0
              with tx = 1
              with ty = 1
              with error = (- tx diameter)
              while (>= x y) do
                (progn
                  (when (> (logand quadrants 1) 0)
                    (sdl2:render-draw-point renderer (- px x) (- py y))
                    (sdl2:render-draw-point renderer (- px y) (- py x)))
                  (when (> (logand quadrants 2) 0)
                    (sdl2:render-draw-point renderer (+ px x) (- py y))
                    (sdl2:render-draw-point renderer (+ px y) (- py x)))
                  (when (> (logand quadrants 4) 0)
                    (sdl2:render-draw-point renderer (- px y) (+ py x))
                    (sdl2:render-draw-point renderer (- px x) (+ py y)))
                  (when (> (logand quadrants 8) 0)
                    (sdl2:render-draw-point renderer (+ px y) (+ py x))
                    (sdl2:render-draw-point renderer (+ px x) (+ py y))))
                (if (> error 0)
                    (progn (decf x)
                           (incf tx 2)
                           (incf error (- tx diameter)))
                    (progn (incf y)
                           (incf error ty)
                           (incf ty 2))))))
    #+ () (when blend-mode
            (sdl2:set-render-draw-blend-mode renderer :none))))

@export
(defun draw-rounded-rect (area corner-radius
                          &key renderer color (thickness 1))
  (let ((renderer (or renderer *renderer*)))
    (with-access (x y w h) (grow-area (- thickness) area)
      (loop for point
              in (list (point (+ x corner-radius) (+ y corner-radius))
                       (point (- (+ x w) corner-radius) (+ y corner-radius))
                       (point (+ x corner-radius) (- (+ y h) corner-radius))
                       (point (- (+ x w) corner-radius) (- (+ y h) corner-radius)))
            for quadrant from 0 to 3
            do (draw-circle point corner-radius
                            :renderer renderer :color color ;; :blend-mode blend-mode
                            :thickness thickness :quadrants quadrant))
      (loop for area
              in (list (area (+ x corner-radius 1) (- y thickness -2)
                             (- w (* corner-radius 2) 1) thickness)
                       (area (+ x corner-radius 1) (+ y h -1)
                             (- w (* corner-radius 2) 1) thickness)
                       (area (- x thickness -2) (+ y corner-radius 1)
                             thickness (- h (* corner-radius 2) 1))
                       (area (+ x (- w 1)) (+ y corner-radius 1)
                             thickness (- h (* corner-radius 2) 1)))
            do (clear-area area
                           :renderer renderer
                           :color color
                           ;;:blend-mode blend-mode
                           )))))

(defmacro with-texture-color (texture color &body body)
  `(if ,color
       (with-slots (r g b a) ,color
	 (unwind-protect
	      (progn (sdl2:set-texture-color-mod ,texture r g b)
		     (sdl2:set-texture-alpha-mod ,texture (or a 255))
		     ,@body)
	   (sdl2:set-texture-color-mod ,texture 255 255 255)
	   (sdl2:set-texture-alpha-mod ,texture 255)))
       (progn ,@body)))

@export
(defun draw-texture (texture target-area-or-point
		     &key
                       renderer source-area color (angle 0)
                       blend-mode flip)
  (let ((renderer (or renderer *renderer*))
	(target-area (if (typep target-area-or-point 'area)
			 target-area-or-point
			 (make-area target-area-or-point
                                    (or source-area (texture-size texture))))))
    (when blend-mode
      (sdl2:set-texture-blend-mode texture blend-mode))
    (with-texture-color texture color
      (sdl2:render-copy-ex renderer texture
                           :source-rect (area-to-rect source-area)
                           :dest-rect (area-to-rect target-area)
                           :angle angle
                           :flip flip))))

@export
(defun free-texture (texture)
  (sdl2:destroy-texture texture))

(defclassi sprite () sprite-texture source-area)

@export
(defun sprite (texture &optional source-area)
  (make-instance 'sprite :sprite-texture texture
                         :source-area (or source-area
                                          (make-area (point 0 0)
                                                     (texture-size texture)))))

@export
(defun sprite-size (sprite)
  (.-> sprite source-area))

@export
(defun split-sprite (size sprite)
  (-> (split-size (sprite-size sprite) size)
      (maptree (lambda (area)
                 (with-new (source-area) sprite
                   (setf source-area (with-new (x y w h) source-area
                                       (incf x (.-> area x))
                                       (incf y (.-> area y))
                                       (setf w (.-> area w)
                                             h (.-> area h))))))
               %)))

@export
(defun draw-sprite (sprite target-area-or-point &key renderer color)
  (with-slots (sprite-texture source-area) sprite
    (draw-texture sprite-texture target-area-or-point
                  :renderer renderer :source-area source-area
                  :color color)))

@export
(defun render-sprite (sprite &key renderer blend-mode)
  (with-slots (sprite-texture source-area) sprite
    (with-slots (w h) source-area
      (let ((texture (make-texture w h
                                   :renderer (or renderer *renderer*)
                                   :pixel-format (sdl2:query-texture sprite-texture)
                                   :blend-mode blend-mode)))
        (with-render-texture texture
          (draw-sprite sprite (point 0 0) :renderer (or renderer *renderer*)))
        texture))))

@export
(defun render-thumbnail (source-texture thumbnail-size &key renderer)
  (with-slots (w h) thumbnail-size
    (let ((texture (make-texture w h
                                 :renderer (or renderer *renderer*)
                                 :pixel-format (sdl2:query-texture source-texture))))
      (with-render-texture texture
        (draw-texture source-texture
                      (align-in
                       (fit-size (texture-size source-texture) thumbnail-size)
                       (make-area (point 0 0) thumbnail-size))))
      texture)))

@export
(defun set-render-draw-blend-mode (blend-mode &key renderer)
  (let ((renderer (or renderer *renderer*)))
    (sdl2:set-render-draw-blend-mode renderer blend-mode)))
