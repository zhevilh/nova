(in-package :nova-presentation)
(cl-annot:enable-annot-syntax)

(define-class presentation-element ()
  draw-fn
  size
  h-align
  v-align)

(defvar *draw-area*)
(defparameter *default-keys*
  (list :fore-color +white+
        :h-align :left
        :v-align :top
        :pad-all 0
        :fit-mode :stretch))

(defmacro get-or-default (value key)
  `(or ,value (getf *default-keys* ,key)))

(defmacro define-element (spec args size &body fn-body)
  `(defun ,spec
       ,(append
         args
         (if (not (find '&key args)) '(&key))
         '(h-align v-align
           pad-left pad-right pad-top pad-bottom pad-all
           width height fit-mode))
     (let ((h-align (get-or-default h-align :h-align))
           (v-align (get-or-default v-align :v-align))
           (pad-left (get-or-default pad-left :pad-left))
           (pad-right (get-or-default pad-right :pad-right))
           (pad-top (get-or-default pad-top :pad-top))
           (pad-bottom (get-or-default pad-bottom :pad-bottom))
           (pad-all (get-or-default pad-all :pad-all))
           (fit-mode (get-or-default fit-mode :fit-mode)))
       (let ((left-padding pad-all)
             (right-padding pad-all)
             (top-padding pad-all)
             (bottom-padding pad-all))
         (if pad-left (setf left-padding pad-left))
         (if pad-right (setf right-padding pad-right))
         (if pad-top (setf top-padding pad-top))
         (if pad-bottom (setf bottom-padding pad-bottom))
         (make-instance 'presentation-element
                        :draw-fn (lambda ()
                                   (let ((*draw-area*
                                           (with-new (x y w h) *draw-area*
                                             (incf x left-padding)
                                             (decf w (+ left-padding
                                                        right-padding))
                                             (incf y top-padding)
                                             (decf h (+ top-padding
                                                        bottom-padding)))))
                                     ,@fn-body))
                        :size (-> (or ,size (size nil nil))
                                  (with-slots (w h) %
                                    (let ((target-size (size (or width w)
                                                             (or height h))))
                                      (if (and w h)
                                          (ecase fit-mode
                                            (:stretch target-size)
                                            (:scale (fit-size % target-size)))
                                          target-size)))
                                  (with-new (w h) %
                                    (if w (incf w (+ left-padding right-padding)))
                                    (if h (incf h (+ top-padding bottom-padding)))))
                        :h-align h-align
                        :v-align v-align)))))

(define-element box (element)
    nil
  (with-slots (draw-fn size h-align v-align
               left-padding right-padding
               top-padding bottom-padding)
      element
    (let ((*draw-area*
            (if size
                (-> (with-new (w h) size
                      (if (not w) (setf w (w *draw-area*)))
                      (if (not h) (setf h (h *draw-area*))))
                    (align-in % *draw-area* :h-align h-align :v-align v-align))
                *draw-area*)))
        (funcall draw-fn))))

@export
(define-element nova-fill (size &key fore-color)
    size
  (clear-area *draw-area*
              :color (get-or-default fore-color :fore-color)
              :renderer (get-or-default nil :renderer)
              :blend-mode (get-or-default nil :blend-mode)))

@export
(define-element nova-text (text &key font fore-color)
    (texture-size (render-active-text (get-or-default font :font) text))
  (draw-texture (render-active-text (get-or-default font :font) text)
                *draw-area*
                :color (get-or-default fore-color :fore-color)
                :renderer (get-or-default nil :renderer)))

@export
(define-element nova-texture (texture
                              &key source-area texture-color angle blend-mode)
    (texture-size texture)    
  (draw-texture texture *draw-area*
                :source-area (get-or-default source-area :source-area)
                :color (get-or-default texture-color :texture-color)
                :angle (get-or-default angle :angle)
                :renderer (get-or-default nil :renderer)
                :blend-mode blend-mode))

(defun make-style (style element)
  (with-new (draw-fn) element
    (setf draw-fn (lambda ()
                    (let ((*default-keys* (append style *default-keys*)))
                      (funcall (.-> element draw-fn)))))))

@export
(define-element nova-overlay (elements)
    (reduce (lambda (acc next)
              (size (max (w acc) (w next)) (max (h acc) (h next))))
            (mapcar #'presentation-element-size elements))
  (dolist (e elements)
    (funcall (.-> (box e) draw-fn))))

@export
(define-element nova-stack (elements &key (orientation :vertical))
    (-> (mapcar #'presentation-element-size elements)
        (ecase orientation
          (:vertical (size nil (apply #'+ (mapcar #'h %))))
          (:horizontal (size (apply #'+ (mapcar #'w %)) nil))))
  (loop for top = 0 then (if (eq orientation :vertical)
                             (+ top (h (.-> element size)))
                             top)
        for left = 0 then (if (eq orientation :horizontal)
                              (+ left (w (.-> element size)))
                              left)
        for element in elements
        do (let ((*draw-area* (make-area (point-move *draw-area* left top)
                                         (size (- (w *draw-area*) left)
                                               (- (h *draw-area*) top)))))
             (funcall (.-> (box element) draw-fn)))))

@export
(defmacro nova-style ((&rest style-plist) element)
  `(let ((*default-keys* (append (list ,@style-plist) *default-keys*)))
     (make-style (list ,@style-plist) ,element)))

@export
(defun present (screen-area element)
  (let ((*draw-area* screen-area))
    (funcall (.-> (box element) draw-fn))))
