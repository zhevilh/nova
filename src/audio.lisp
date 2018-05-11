(in-package :nova)
(annot:enable-annot-syntax)

(defvar *loaded-music* nil)
(defvar *loaded-samples* nil)

(defmacro with-audio (flags &body body)
  `(let ((*loaded-music* (make-hash-table))
	 (*loaded-samples* (make-hash-table)))
     (unwind-protect
	  (progn
	    (sdl2-mixer:init ,@flags)
	    (sdl2-mixer:open-audio 44100 :s16sys 2 1024)
	    ,@body)
       (loop for m being the hash-values in *loaded-music*
	  do (sdl2-mixer:free-music m))
       (loop for s being the hash-values in *loaded-samples*
	  do (sdl2-mixer:free-chunk s))
       (pause-music)
       (pause-sample)
       (sdl2-mixer:close-audio)
       (sdl2-mixer:quit))))

@export
(defun load-music (path)
  (let* ((path (resolve-path path))
	 (music (gethash path *loaded-music*)))
    (or music (setf (gethash path *loaded-music*)
		    (sdl2-mixer:load-music path)))))

@export
(defun load-sample (path)
  (let ((path (resolve-path path))
	(sample (gethash path *loaded-samples*)))
    (or sample (setf (gethash path *loaded-samples*) (sdl2-mixer:load-wav path)))))

@export
(defun play-music (music &key (loops 1) (start-position 0))
  ;; No idea why, but some music won't play properly unless I play them twice.
  (sdl2-mixer:play-music music 0)
  (sdl2-mixer:play-music music loops)
  (seek-music start-position))

@export
(defun seek-music (position)
  (sdl2-mixer::mix-set-music-position (coerce position 'double-float)))

@export
(defun pause-music ()
  (sdl2-mixer:halt-music))

@export
(defun set-music-volume (volume)
  (sdl2-mixer:volume-music (round (* volume 128))))

@export
(defun play-sample (sample &optional (channel -1))
  (sdl2-mixer:play-channel channel sample 0))

@export
(defun pause-sample (&optional (channel -1))
  (sdl2-mixer:halt-channel channel))

@export
(defun set-channel-volume (volume &optional (channel -1))
  (sdl2-mixer:volume channel volume))
