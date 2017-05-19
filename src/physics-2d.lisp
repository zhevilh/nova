(in-package :nova-2d)
(annot:enable-annot-syntax)

;; Useful data types.

@export-class
(defclassi velocity ()
  dx dy)

@export
(defun make-velocity (dx dy)
  "float -> float -> velocity"
  (make-instance 'velocity :dx dx :dy dy))

@export
(defun velocity (degrees magnitude)
  "int -> float -> velocity"
  (let ((radians (rtg-math:radians (- degrees 90))))
    (make-velocity (* magnitude (cos radians))
		   (* magnitude (sin radians)))))

@export
(defun decompose-velocity (velocity)
  "velocity -> degrees, magnitude"
  (with-slots (dx dy) velocity
    (values (-> (rtg-math:degrees (atan dy dx))
		(+ % 90)
		(mod % 360))
	    (sqrt (+ (expt dx 2) (expt dy 2))))))

;; Collision detection

@export-class
(defclassi collision ()
  collision-info collision-object collision-hitbox)

@export
(defun collision-map (&optional boundary-size)
  "[boundary-size] -> collision-map"
  (let ((collision-map (fset:empty-map)))
    (when boundary-size
      (with-slots (w h) boundary-size
	(setf collision-map
	      (-> collision-map
		  (push-hitbox % :boundaries :top-boundary (area 0 -1 w 1))
		  (push-hitbox % :boundaries :bottom-boundary (area 0 h w 1))
		  (push-hitbox % :boundaries :left-boundary (area -1 0 1 h))
		  (push-hitbox % :boundaries :right-boundary (area w 0 1 h))))))
    collision-map))

@export
(defun push-hitbox (collision-map layer object hitbox)
  "collision-map -> keyword -> object -> hitbox -> collision-map"
  (push (make-instance 'collision
		       :collision-object object
		       :collision-hitbox hitbox)
	(fset:lookup collision-map layer))
  collision-map)

@export
(defmacro push-hitbox! (collision-map layer object hitbox)
  "Same as push-hitbox, but binds the result to the collision-map symbol."
  `(setf ,collision-map (push-hitbox ,collision-map ,layer ,object ,hitbox)))

@export
(defun remove-object (collision-map object)
  "collision-map -> object -> collision-map"
  (fset:image (lambda (layer tuples)
		(values layer
			(remove-if (lambda (collision)
				     (eq (collision-object collision) object))
				   tuples)))
	      collision-map))

@export
(defmacro remove-object! (collision-map object)
  "Same as remove-object, but binds the result to the collision-map symbol."
  `(setf ,collision-map (remove-object ,collision-map ,object)))

@export
(defun remove-layer (collision-map layer)
  "collision-map -> keyword -> collision-map"
  (fset:filter (lambda (it-layer tuples)
		 (declare (ignore tuples))
		 (not (eq it-layer layer)))
	       collision-map))

@export
(defmacro remove-layer! (collision-map layer)
  "Same as remove-layer, but binds the result to the collision-map symbol."
  `(setf ,collision-map (remove-layer ,collision-map ,layer)))

@export
(defun get-collisions (collision-map hitbox)
  "collision-map -> hitbox -> collision list"
  (fset:reduce (lambda (acc layer collisions)
		 (declare (ignore layer))
		 (union acc
			(loop for collision in collisions
			   for result = (collision? hitbox
						    (collision-hitbox collision))
			   when result
			   collect (with-new (collision-info) collision
				     (setf collision-info result)))))
	       collision-map
	       :initial-value '()))

@export
(defun get-layer (collision-map layer)
  "collision-map -> layer -> collision list"
  (fset:lookup collision-map layer))

@export
(defgeneric collision? (object1 object2)
  (:documentation "any -> any -> collision-info"))

(defmethod collision? ((area1 area) (area2 area))
  (with-slots ((x1 x) (y1 y) (w1 w) (h1 h)) area1
    (with-slots ((x2 x) (y2 y) (w2 w) (h2 h)) area2
      (and (>= (+ x1 w1) x2)
	   (>= (+ x2 w2) x1)
	   (>= (+ y1 h1) y2)
	   (>= (+ y2 h2) y1)))))

@export
(defmacro defcollision ((object1 class1) (object2 class2) &body body)
  "Defines a new collision detection function between two objects.
Will expand into two defmethod declarations for the collision? method."
  `(progn (defmethod collision? ((,object1 ,class1) (,object2 ,class2))
	    ,@body)
	  (defmethod collision? ((,object2 ,class2) (,object1 ,class1))
	    ,@body)))
