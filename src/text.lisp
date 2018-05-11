(in-package :nova)
(cl-annot:enable-annot-syntax)

(defvar *open-fonts*)
(defvar *last-active-texts*)
(defvar *active-texts*)

@export
(defmacro with-active-texts (&body body)
  `(let ((*open-fonts* (make-hash-table))
	 *last-active-texts*
	 *active-texts*)
     ,@body))

@export
(defun open-font (assoc-keyword file size)
  (let* ((path (resolve-path file)))
    (setf (gethash assoc-keyword *open-fonts*)
	  (sdl2-ttf:open-font path size))))

@export
(defun render-text (font-key text &key renderer (scale-filtering :nearest))
  (let ((font (gethash font-key *open-fonts*)))
    (when (not font)
      (error "Could not find open font."))
    (with-slots (r g b a) +white+
      (let* ((surface (sdl2-ttf:render-utf8-solid font text r g b a))
	     (texture (load-texture surface
				    :renderer renderer
				    :scale-filtering scale-filtering)))
	(sdl2:free-surface surface)
	(values texture (texture-size texture))))))

(define-class active-text ()
  font-key string texture)

@export
(defun tick-active-texts ()
  (setf *last-active-texts* *active-texts*)
  (setf *active-texts* nil))

(defun find-active-text (font-key string where create-new?)
  (or (find-if (lambda (at)
		 (with-slots ((at-font-key font-key) (at-string string)) at
		   (and (eq font-key at-font-key)
			(string= string at-string))))
	       where)
      (when create-new?
	(make-instance 'active-text
		       :font-key font-key
		       :string string
		       :texture (render-text font-key string)))))

@export
(defun render-active-text (font-key string)
  (let ((at (or (find-active-text font-key string *active-texts* nil)
		(find-active-text font-key string *last-active-texts* t))))
    (pushnew at *active-texts*)
    (with-slots (texture) at
      (values texture (texture-size texture)))))
