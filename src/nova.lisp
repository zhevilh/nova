(in-package :nova)
(annot:enable-annot-syntax)

(define-condition quit-game () ())

(defvar *window*)
(defvar *end-scene?*)
(defvar *error-handler*)
(defvar *fps*)

@export
(defun quit-game ()
  (error 'quit-game))

@export
(defun end-scene ()
  (setf *end-scene?* t))

@export
(defun run-scene (scene)
  (let (*end-scene?*
        (last-frame-internal-time 0)
        (internal-time-per-frame
          (/ internal-time-units-per-second *fps*)))
    (unwind-protect
         (with-loaded-textures
           (handler-bind
               ((error (lambda (c)
                         (when *error-handler*
                           (funcall *error-handler* c)))))
             (load-scene scene)
             (sdl2:with-sdl-event (event)
               (loop
                 until *end-scene?*
                 do (progn
                      (tick-active-texts)
                      (draw-scene scene)
                      (sdl2:render-present *renderer*)
                      (setf last-frame-internal-time
                            (get-internal-real-time))
                      (loop as rc = (sdl2:next-event event)
                            until (= rc 0)
                            do (handle-event
                                scene
                                (sdl2:get-event-type event)
                                event))
                      (idle-scene scene)
                      (sleep
                       (/
                        (max 0 (- internal-time-per-frame
                                  (- (get-internal-real-time)
                                     last-frame-internal-time)))
                        internal-time-units-per-second)))))
             (return-scene scene)))
      (unload-scene scene))))

@export
(defun run-game (title width height controller
		 &key
		   (fps 24)
                   (scale-filtering :anisotropic)
		   fullscreen-mode
		   open-joysticks
		   (music-volume 1)
                   error-bind)
  (unwind-protect
       (sdl2:with-init (:everything)
         (sdl2-ffi.functions:sdl-set-hint sdl2-ffi:+sdl-hint-render-scale-quality+
                                          (ecase scale-filtering
                                            (:nearest "0")
                                            (:linear "1")
                                            (:anisotropic "2")))
         (reset-time)
	 (sdl2-ttf:init)
	 (with-joysticks
	   (when open-joysticks
	     (open-all-joysticks))
	   (with-audio (:mp3 :ogg)
	     (set-music-volume music-volume)
	     (with-active-texts
	       (sdl2:with-window (*window* :title title :w width :h height)
		 ;; Windows emacs fix.
		 (sdl2:hide-window *window*)
		 (sdl2:show-window *window*)
		 (sdl2:set-window-fullscreen *window* fullscreen-mode)
		 (with-renderer *window*
                   (let ((*fps* fps)
                         (*error-handler* error-bind))
                     (randomize)
                     (handler-case 
                         (if (functionp controller)
                             (funcall controller)
                             (run-scene controller))
                       (quit-game ())))))))))
    (tg:gc)))

@export
(defun run-music-player (controller-fn &key
                                         (music-volume 1)
                                         music)
  (unwind-protect
       (sdl2:with-init (:everything)
         (with-audio (:mp3 :ogg)
           (set-music-volume music-volume)
           (when music
             (setf music (load-music music))
             (play-music music))
           (handler-case
               (funcall controller-fn)
             (quit-game ()))
           (pause-music)))
    (tg:gc)))

@export
(defgeneric load-scene (scene))
(defmethod load-scene (scene)
  (declare (ignore scene)))

@export
(defgeneric unload-scene (scene))
(defmethod unload-scene (scene)
  (declare (ignore scene)))

@export
(defgeneric draw-scene (scene))
(defmethod draw-scene (scene)
  (declare (ignore scene)))

@export
(defgeneric idle-scene (scene))
(defmethod idle-scene (scene)
  (declare (ignore scene)))

@export
(defgeneric return-scene (scene))
(defmethod return-scene (scene)
  (declare (ignore scene)))

@export
(defun set-fullscreen (mode)
  (sdl2:set-window-fullscreen *window* mode))

@export
(defun toggle-cursor (show)
  (if show
      (sdl2:show-cursor)
      (sdl2:hide-cursor)))

@export
(defun relative-cursor-mode (enabled)
  (sdl2:set-relative-mouse-mode enabled))

@export
(defun raise-window ()
  (sdl2:raise-window *window*))

@export
(defun cursor-position ()
  (multiple-value-bind (x y) (sdl2:mouse-state)
    (point x y)))
