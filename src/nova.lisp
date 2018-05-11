(in-package :nova)
(annot:enable-annot-syntax)

;; Stupid fix because sdl2-ttf won't load correctly for some reason.
(load (merge-pathnames "src/autowrap.lisp" (asdf:system-source-directory :sdl2-ttf)))

(defvar *window*)
(defvar *next-scene* nil)
(defvar *quit?* nil)

@export
(defun change-scene (next-scene)
  (load-scene next-scene)
  (setf *next-scene* next-scene))

@export
(defun quit-game ()
  (setf *quit?* t))

@export
(defun run-game (title width height initial-scene
		 &key
		   (fps 24)
		   fullscreen-mode
		   open-joysticks
		   (music-volume 1))
  (unwind-protect
       (sdl2:with-init (:everything)
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
		   (let ((last-frame-internal-time 0)
			 (internal-time-per-frame
                           (/ internal-time-units-per-second fps))
			 current-scene
			 *quit?*)
		     (change-scene initial-scene)
		     (sdl2:with-sdl-event (event)
		       (loop
                         until *quit?*
                         do (progn
                              (setf last-frame-internal-time
                                    (get-internal-real-time))
                              (loop as rc = (sdl2:next-event event)
                                    until (or *quit?* (= rc 0))
                                    do (handle-event current-scene
                                                     (sdl2:get-event-type event)
                                                     event))
                              (if *next-scene*
                                  (setf current-scene *next-scene*
                                        *next-scene* nil)
                                  (progn
                                    (idle-scene current-scene)
                                    (tick-active-texts)
                                    (draw-scene current-scene)
                                    (sdl2:render-present *renderer*)
                                    (sleep
                                     (/ (max 0 (- internal-time-per-frame
                                                  (- (get-internal-real-time)
                                                     last-frame-internal-time)))
                                        internal-time-units-per-second))))))))))))))
    (tg:gc)))

@export
(defgeneric load-scene (scene))
(defmethod load-scene (scene)
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
