(in-package :nova)
(annot:enable-annot-syntax)

@export-class
(defclassi scene ())

@export
(defgeneric scene-event (scene event world)
  (:documentation "event -> world -> world"))
@export
(defgeneric scene-update (scene world)
  (:documentation "world -> world"))
@export
(defgeneric scene-draw (scene world)
  (:documentation "world -> draw-sprite list"))

(defmethod scene-event ((scene scene) event world) (declare (ignore event)) world)
(defmethod scene-update ((scene scene) world) world)
