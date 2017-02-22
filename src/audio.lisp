(in-package :nova)

(defvar *loaded-music* nil)
(defvar *loaded-samples* nil)

(defmacro with-audio (flags &body body)
  `(let ((*loaded-music* (make-hash-table))
	 (*loaded-samples* (make-hash-table)))
     (unwind-protect
	  (progn
	    (format t "Initializing SDL2 mixer...")
	    (sdl2-mixer:init ,@flags)
	    (format t "OK.~%")

	    (format t "Opening audio channels...")
	    (sdl2-mixer:open-audio 44100 :s16sys 2 1024)
	    (format t "OK.~%")
	    
	    ,@body)
       (loop for m being the hash-values in *loaded-music*
	  do (sdl2-mixer:free-music m))
       (loop for s being the hash-values in *loaded-samples*
	  do (sdl2-mixer:free-chunk s))
       (sdl2-mixer:halt-channel -1)
       (sdl2-mixer:close-audio)
       (sdl2-mixer:quit))))

(export 'load-music)
(defun load-music (path)
  "string -> IO music"
  (setf (gethash path *loaded-music*) (sdl2-mixer:load-music path)))

(export 'load-sample)
(defun load-sample (path)
  "string -> IO sample"
  (setf (gethash path *loaded-samples*) (sdl2-mixer:load-wav path)))

(export 'play-music)
(defun play-music (music &optional (loops 0))
  "music -> IO"
  (sdl2-mixer:play-music music loops))

(export 'play-sample)
(defun play-sample (sample &optional (channel -1))
  "sample -> IO"
  (sdl2-mixer:play-channel channel sample 0))
