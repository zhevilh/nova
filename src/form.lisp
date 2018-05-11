(in-package :nova-form)
(cl-annot:enable-annot-syntax)

(define-class draw-info ()
  draw-function
  source-area
  target-area
  color)

(defun texture-function (texture)
  (lambda (renderer source-area target-area color)
    (draw-texture texture target-area
                  :renderer renderer
                  :source-area source-area
                  :color color)))

(defparameter clear-function
  (lambda (renderer source-area target-area color)
    (declare (ignore source-area))
    (clear-area target-area
                :renderer renderer
                :color color)))

(defun draw-with-info (info renderer)
  (with-slots (draw-function source-area target-area color) info
    (funcall draw-function renderer source-area target-area color)))

(defmacro map-new-info (slots draw-info-list &body body)
  `(map-with-new ,slots (remove-if-not #'identity (flatten ,draw-info-list))
     ,@body))

(defun map-draw-info (functions draw-info)
  (mapcar (lambda (f) (funcall f draw-info)) (flatten functions)))

(defun get-boundaries (draw-info-list)
  (when-> (remove-if-not #'identity draw-info-list)
          (mapcar #'draw-info-target-area %)
          (rectangle (apply #'min (mapcar #'left %))
                     (apply #'min (mapcar #'top %))
                     (apply #'max (mapcar #'right %))
                     (apply #'max (mapcar #'bottom %)))))

@export
(defun resolve (arg)
  (if (functionp arg) (funcall arg) arg))

(defmacro with-resolve ((&rest args) &body body)
  `(let (,.(mapcar (lambda (a) `(,a (resolve ,a))) args))
     ,@body))

(defvar *binding*)
(defvar *draw-area*)
(defvar *font*)
(defvar *fore-color* +white+)
(defvar *texture-color* +white+)
(defvar *back-color* +black+)

(defun resolve-slot (slot)
  (when *binding*
    (if (functionp slot)
        (funcall slot *binding*)
        (slot-value *binding* slot))))

@export
(defun bind-slot (slot)
  (lambda () (resolve-slot slot)))

@export
(defun bind-timing-slot (slot)
  (lambda () (run-timing (resolve-slot slot))))

@export
(defun bind-format (format &rest slot-args)
  (lambda () (apply #'format nil format (mapcar #'resolve-slot slot-args))))

@export
(defmacro bind ((&rest binding-slots) &body body)
  `(lambda ()
     (with-slots ,binding-slots *binding*
       ,@body)))

@export
(defun nova-text (text &key font fore-color)
  (let ((font (or font *font*))
        (fore-color (or fore-color *fore-color*)))
    (lambda (draw-info)
      (with-resolve (font fore-color text)
        (with-new (draw-function color target-area) draw-info
          (-> (render-text font text)
              (setf draw-function (texture-function %)
                    color fore-color
                    target-area (make-area (point 0 0) (texture-size %)))))))))

@export
(defun nova-texture (texture &key target-size source-area texture-color)
  (let ((texture-color (or texture-color *texture-color*)))
    (lambda (draw-info)
      (with-resolve (texture texture-color)
        (when texture
          (with-new (draw-function color (info-source-area source-area)
                     target-area)
              draw-info
            (setf draw-function (texture-function texture)
                  info-source-area source-area
                  target-area (make-area (point 0 0) (or target-size
                                                         (texture-size texture)))
                  color texture-color)))))))

@export
(defun nova-area (area &key back-color)
  (let ((back-color (or back-color *back-color*)))
    (lambda (draw-info)
      (with-resolve (area back-color)
        (when area
          (with-new (draw-function target-area color) draw-info
            (setf draw-function clear-function
                  target-area area
                  color back-color)))))))

@export
(defun align (h-align v-align &rest elements)
  (lambda (draw-info)
    (with-resolve (h-align v-align)
      (map-new-info (source-area target-area) (map-draw-info elements draw-info)
	(setf target-area (align-in target-area *draw-area*
				    :v-align v-align
				    :h-align h-align))))))

@export
(defun pad (direction padding &rest elements)
  (lambda (draw-info)
    (with-resolve (padding)
      (map-new-info (source-area target-area) (map-draw-info elements draw-info)
        (setf target-area
              (with-new (x y w h) target-area
                (when (or (eq direction :all) (eq direction :left))
                  (setf x (max (+ (left *draw-area*) padding) x)))
                (when (or (eq direction :all) (eq direction :top))
                  (setf y (max (+ (top *draw-area*) padding) y)))
                (when (or (eq direction :all) (eq direction :bottom))
                  (-> (- (+ y h) (- (bottom *draw-area*) padding))
                      (decf y (max 0 %))))
                (when (or (eq direction :all) (eq direction :right))
                  (-> (- (+ x w) (- (right *draw-area*) padding))
                      (decf x (max 0 %))))))))))

@export
(defun fit (shrink-only? &rest elements)
  (lambda (draw-info)
    (with-resolve (shrink-only?)
      (let ((draw-info-list (map-draw-info elements draw-info)))
        (when draw-info-list
          (when-> (get-boundaries draw-info-list)
                  (with-slots (x y w h) %
                    (-> (get-fit-scale (size (+ x w) (+ y h)) *draw-area*)
                        (if shrink-only? (min 1 %) %)
                        (map-new-info (target-area) draw-info-list
                          (setf target-area (scale-size % target-area)))))))))))

@export
(defun place (point &rest elements)
  (lambda (draw-info)
    (with-resolve (point)
      (map-new-info (target-area) (map-draw-info elements draw-info)
        (setf target-area (with-new (x y) target-area
                            (incf x (x point))
                            (incf y (y point))))))))

@export
(defun resize (size element)
  (lambda (draw-info)
    (with-resolve (size)
      (when-> (funcall element draw-info)
              (with-new (target-area) %
                (setf target-area (with-new (w h) target-area
                                    (setf w (w size)
                                          h (h size)))))))))

@export
(defun scale (ratio center? &rest elements)
  (lambda (draw-info)
    (with-resolve (ratio)
      (map-new-info (target-area) (map-draw-info elements draw-info)
        (setf target-area (-> (scale-size ratio target-area)
                              (if center?
                                  (align-in % target-area)
                                  %)))))))

@export
(defmacro style ((&key font fore-color back-color texture-color) &body body)
  `(let (,.(-> (list (if font `(*font* ,font))
		     (if fore-color `(*fore-color* ,fore-color))
                     (if back-color `(*back-color* ,back-color))
                     (if texture-color `(*texture-color* ,texture-color)))
             (remove-if-not #'identity %)))
     (list ,@body)))

@export
(defmacro bind-let ((&rest binding-slots) (&rest bindings) &body body)
  (let ((binding-symbols (mapcar #'car bindings)))
    `(let (,.(mapcar (lambda (b)
                       (destructuring-bind (binding value) b
                         `(,binding (bind ,binding-slots ,value))))
                     bindings))
       (list ,.(maptree (lambda (expr)
                          (if (find expr binding-symbols)
                              `(resolve ,expr)
                              expr))
                        body)))))

@export
(defun box (&rest elements)
  (lambda (draw-info)
    (map-draw-info elements draw-info)))

@export
(defmacro define-form (name &body elements)
  `(defparameter ,name
     (box ,@elements)))

@export
(defun draw-form (form draw-area &key renderer binding)
  (let ((*binding* binding)
	(*draw-area* draw-area))
    (dolist (element (flatten (funcall form (make-instance 'draw-info))))
      (draw-with-info element renderer))))
