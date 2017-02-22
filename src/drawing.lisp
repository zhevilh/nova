(in-package :nova)

(export '(color r g b a))
(defclass color ()
  ((r :initform 0 :initarg :r :reader r)
   (g :initform 0 :initarg :g :reader g)
   (b :initform 0 :initarg :b :reader b)
   (a :initform 0 :initarg :a :reader a)))

(defvar *colors* (make-hash-table))

(defun color (r g b a)
  (make-instance 'color :r r :g g :b b :a a))

(defun resolve-color (color-or-keyword)
  (if (keywordp color-or-keyword)
      (gethash color-or-keyword *colors*)
      color-or-keyword))

(export 'defcolor)
(defmacro defcolor (keyword r g b)
  `(setf (gethash ,keyword *colors*) (color ,r ,g ,b 0)))

(export 'new-color)
(defmacro new-color (color &key r g b a)
  `(with-new (r g b a) (resolve-color ,color)
     (if ,r (setf r ,r))
     (if ,g (setf g ,g))
     (if ,b (setf b ,b))
     (if ,a (setf a ,a))))

(defcolor :white 255 255 255)
(defcolor :black 0 0 0)
(defcolor :red 255 0 0)
(defcolor :green 0 255 0)
(defcolor :blue 0 0 255)
(defcolor :yellow 255 255 0)
(defcolor :fuschia 255 0 255)

(defvar *loaded-fonts* nil)

(defmacro with-ttf (&body body)
  `(let ((*loaded-fonts* (make-hash-table :test #'equal)))
     (unwind-protect
	  (progn
	    (format t "Initializing SDL2 TTF...")
	    (sdl2-ttf:init)
	    (format t "OK.~%")
	    
	    ,@body)
       (loop for f being the hash-values in *loaded-fonts*
	  do (sdl2-ttf:close-font f))
       (sdl2-ttf:quit))))

(export 'load-font)
(defun load-font (path point)
  "string -> int -> IO font"
  (setf (gethash '(path . point) *loaded-fonts*) (sdl2-ttf:open-font path point)))
