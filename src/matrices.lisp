(in-package :%nova)
(annot:enable-annot-syntax)

(defun orthographic-matrix (right left bottom top near far)
  (declare (type float right left bottom top near far))
  (m4:make (/ 2.0 (- right left)) 0.0 0.0 (- (/ (+ right left)
						(- right left)))
	   0.0 (/ 2.0 (- top bottom)) 0.0 (- (/ (+ top bottom)
						(- top bottom)))
	   0.0 0.0 (/ -2.0 (- far near)) (- (/ (+ far near)
					       (- far near)))
	   0.0 0.0 0.0 1.0))

@export
(defun projection-matrix-2d (view-width view-height)
  (declare (type fixnum view-width view-height))
  (orthographic-matrix view-width 0.0 view-height 0.0 1.0 -1.0))

@export
(defun projection-matrix-texture (width height)
  (declare (type float width height))
  (m4:make (/ 1.0 width) 0.0 0.0 0.0
	   0.0 (/ 1.0 height) 0.0 0.0
	   0.0 0.0 0.0 0.0
	   0.0 0.0 0.0 1.0))
