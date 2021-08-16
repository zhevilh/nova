(in-package :nova)
(annot:enable-annot-syntax)

(defvar *loaded-music* nil)
(defvar *loaded-samples* nil)
(defvar *music-volume* nil)

(defclass audio-data ()
  ((foreign-pointer :initarg :pointer :reader pointer)
   (source-path :initarg :source :reader source)))

(defmacro with-audio (flags &body body)
  `(let ((*loaded-music* (make-hash-table))
	 (*loaded-samples* (make-hash-table))
         (*music-volume* 1))
     (unwind-protect
	  (progn
	    (sdl2-mixer:init ,@flags)
	    (sdl2-mixer:open-audio 44100 :s16sys 2 1024)
	    ,@body)
       (loop for m being the hash-values in *loaded-music*
             do (sdl2-mixer:free-music (pointer m)))
       (loop for s being the hash-values in *loaded-samples*
             do (sdl2-mixer:free-chunk (pointer s)))
       (pause-music)
       (pause-sample)
       (sdl2-mixer:close-audio)
       (sdl2-mixer:quit))))

@export
(defun load-music (path)
  (let* ((path (resolve-path path))
	 (music (gethash path *loaded-music*)))
    (or music (setf (gethash path *loaded-music*)
		    (make-instance 'audio-data
                                   :pointer (sdl2-mixer:load-music path)
                                   :source path)))))

@export
(defun load-sample (path)
  (let ((path (resolve-path path))
	(sample (gethash path *loaded-samples*)))
    (or sample (setf (gethash path *loaded-samples*)
                     (make-instance 'audio-data
                                    :pointer (sdl2-mixer:load-wav path)
                                    :source path)))))

@export
(defun play-music (music &key (loops 1) (start-position 0))
  ;; No idea why, but some music won't play properly unless I play them twice.
  (sdl2-mixer:play-music (pointer music) 0)
  (sdl2-mixer:play-music (pointer music) loops)
  (seek-music start-position))

@export
(defun seek-music (position)
  (sdl2-mixer::mix-rewind-music)
  (-> (float position)
      (sdl2-mixer::mix-set-music-position (coerce % 'double-float))))

@export
(defun pause-music ()
  (sdl2-mixer::mix-pause-music))

@export
(defun resume-music ()
  (sdl2-mixer::mix-resume-music)
  (sdl2-mixer::mix-resume-music))

@export
(defun set-music-volume (volume)
  (sdl2-mixer:volume-music (max 0 (round (* volume 127))))
  (setf *music-volume* volume))

@export
(defun get-music-volume ()
  *music-volume*)

@export
(defun play-sample (sample &key (channel -1))
  (sdl2-mixer:play-channel channel (pointer sample) 0))

@export
(defun pause-sample (&optional (channel -1))
  (sdl2-mixer:halt-channel channel))

@export
(defun set-channel-volume (volume &optional (channel -1))
  (sdl2-mixer:volume channel (max 0 (round (* volume 127)))))

@export
(defun music-playing? ()
  (= 1 (sdl2-mixer::mix-playing-music)))

@export
(defun sample-playing? (&optional (channel -1))
  (= 1 (sdl2-mixer::mix-playing channel)))

@export
(defun audio-source (music-or-sample)
  (source music-or-sample)
  )
