(in-package :nova)
(annot:enable-annot-syntax)

(define-condition quit-game () ())

(defvar *window*)
(defvar *fullscreen-mode* nil)
(defvar *current-scene* nil)
(defvar *end-scene?*)
(defvar *scene-stack* nil)
(defvar *parent-scene*)
(defvar *error-handler*)
(defvar *fps*)

@export
(defun quit-game ()
  (error 'quit-game))

@export
(defun end-scene ()
  (setf *end-scene?* t))

@export
(defun run-scene (scene &key child-scene?)
  (let* (*end-scene?*
         (parent-scene (when child-scene? *current-scene*))
         (*parent-scene* parent-scene)
         (*current-scene* scene)
         (last-frame-internal-time 0)
         (internal-time-per-frame
           (/ internal-time-units-per-second *fps*)))
    (unwind-protect
         (with-loaded-textures
           (handler-bind
               ((error (lambda (c)
                         (when *error-handler*
                           (funcall *error-handler* c)))))
             (push scene *scene-stack*)
             (initialize-scene scene)
             (load-scene scene)
             (sdl2:with-sdl-event (event)
               (loop
                 until *end-scene?*
                 do (progn
                      (tick-active-texts)
                      (when *parent-scene*
                        (let ((*parent-scene* nil))
                          (draw-scene parent-scene)))
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
                      (when *parent-scene*
                        (let ((*parent-scene* nil))
                          (idle-scene parent-scene)))
                      (idle-scene scene)
                      (sleep
                       (/
                        (max 0 (- internal-time-per-frame
                                  (- (get-internal-real-time)
                                     last-frame-internal-time)))
                        internal-time-units-per-second)))))
             (return-scene scene)))
      (unload-scene scene)
      (pop *scene-stack*)
      (initialize-scene (car *scene-stack*)))))

@export
(defun run-game (title width height controller
		 &key
		   (fps 24)
                   (scale-filtering :anisotropic)
		   fullscreen-mode
                   resizable?
		   open-joysticks
		   (music-volume 1)
                   error-bind)
  (unwind-protect
       (sdl2:with-init (:everything)
         (set-scale-filtering scale-filtering)
         (reset-time)
	 (sdl2-ttf:init)
	 (with-joysticks
	   (when open-joysticks
	     (open-all-joysticks))
	   (with-audio (:mp3 :ogg)
	     (set-music-volume music-volume)
	     (with-active-texts
	       (sdl2:with-window (*window*
                                  :title title
                                  :w (floor width)
                                  :h (floor height)
                                  :flags (list-not-nil
                                          (when resizable? :resizable)))
		 ;; Windows emacs fix.
		 (sdl2:hide-window *window*)
		 (sdl2:show-window *window*)
		 (set-fullscreen fullscreen-mode)
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
(defgeneric initialize-scene (scene)
  (:method (scene)
    (declare (ignore scene))))

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
  (when (eq mode t)
    (setf mode :fullscreen))
  (setf *fullscreen-mode* mode)
  (sdl2:set-window-fullscreen *window* mode))

@export
(defun toggle-fullscreen (&optional (fullscreen-mode t))
  (set-fullscreen (and (not *fullscreen-mode*) fullscreen-mode)))

@export
(defun set-scale-filtering (scale-filtering)
  (sdl2-ffi.functions:sdl-set-hint sdl2-ffi:+sdl-hint-render-scale-quality+
                                   (ecase scale-filtering
                                     (:nearest "0")
                                     (:linear "1")
                                     (:anisotropic "2"))))

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

@export
(defun window-size ()
  (multiple-value-bind (w h) (sdl2:get-window-size *window*)
    (size w h)))
