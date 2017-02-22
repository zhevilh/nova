(in-package :nova)

(defclass scene ()
  ((load-fn :initarg :load-fn :reader load-fn)
   (draw-fn :initarg :draw-fn :reader draw-fn)
   (update-fn :initarg :update-fn :reader update-fn)
   (actors :initform (make-hash-table) :reader actors)
   (loaded? :initform nil :reader loaded?)))

(export '(scene-load
	  scene-event
	  scene-update
	  scene-draw))
(defgeneric scene-load (scene)
  (:documentation "unit -> IO unit"))
(defgeneric scene-event (scene event world)
  (:documentation "event -> world -> world"))
(defgeneric scene-update (scene world)
  (:documentation "world -> world"))
(defgeneric scene-draw (scene world)
  (:documentation "world -> draw-sprite list"))

(defmethod scene-load :around ((scene scene))
  (if (not (loaded? scene))
      (labels! ((set-actor (key actor) (setf (gethash key (actors scene)) actor)))
	(call-next-method))))

(defmethod scene-event ((scene scene) event world) (declare (ignore event)) world)

(defmethod scene-update ((scene scene) world) world)

(defmethod scene-draw :around ((scene scene) world)
  (declare (ignore world))
  (labels! ((draw-sprite (draw-sprite)
			  (using-render-context (renderer)
			    (with-slots (texture source-area x y w h) draw-sprite
			      (sdl2:render-copy-ex renderer texture
						   :source-rect (area->sdl-rect source-area)
						   :dest-rect (sdl2:make-rect x y w h))))))
    (call-next-method)))

(export 'set-actor)
(defun set-actor (key actor)
  "actor -> IO actor"
  (declare (ignore key actor))
  (error "This function can only be called within the scene-load method of a scene."))

(export 'change-scene)
(defun change-scene (next-scene)
  (declare (ignore next-scene))
  (error "This function can only be called within the scene-event or scene-update method of a scene."))

(export 'get-actor)
(defun get-actor (key)
  "keyword -> actor"
  (declare (ignore key))
  (error "This function can only be called within the scene-draw method of a scene."))

(export 'draw-sprite)
(defun draw-sprite (draw-sprite)
  "draw-sprite -> IO"
  (declare (ignore draw-sprite))
  (error "This function can only be called within the scene-draw method of a scene."))
