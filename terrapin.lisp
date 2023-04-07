;;; the terp object ;;;

(in-package #:terrapin)

(defclass terp ()
  ((x
    :initarg :x
    :initform 0
    :accessor x)
   (y
    :initarg :y
    :initform 0
    :accessor y)
   (angle
    :initarg :angle
    :initform 0
    :accessor angle)
   (pen
    :initarg :pen
    :initform t
    :accessor pen)))


(defun init-func (turtle)
  "intialize functions with the turtle object"
  (defun to-radians (angle)
    "basic trig shit"
    (* angle (/ pi 180)))

  (defun goto (x y &optional (angle 0))
    "moves to point without drawing a line, I think"
    (setf (x turtle) x)
    (setf (y turtle) y)
    (setf (angle turtle) angle))

  (defun toggle-pen ()
    "toggles pen up and down"
    (if (pen turtle)
      (setf (pen turtle) nil)
      (setf (pen turtle) t)))

  (defun pen-up ()
    (setf (pen turtle) nil))

  (defun pen-down ()
    (setf (pen turtle) t))

  (defun right (degrees)
    "add degrees to turtle angle"
    (setf (angle turtle) (+ (angle turtle) degrees)))

  (defun left (degrees)
    "left is one backwards right"
    (right (- degrees)))

  (defun forward (steps)
    "move forward at angle. if pen-down is t, draw a line"
    (let ((x (x turtle))
          (y (y turtle))
          (angle (angle turtle)))
      (let ((dest-x x)
            (dest-y y))
        (setf dest-x (+ dest-x (* steps (cos (to-radians angle)))))
        (setf dest-y (- dest-y (* steps (sin (to-radians angle)))))
        (if (pen turtle)
          (progn
            (move-to (x turtle) (y turtle))
            (line-to dest-x dest-y)))
        (setf (x turtle) dest-x)
        (setf (y turtle) dest-y)
        (stroke))))

  (defun back (steps)
    "backin up"
    (forward (- steps))))

(defmacro init-terp (name x y)
  `(defvar ,name (make-instance 'terp :x ,x :y ,y)))


(defun init (w h &optional (title "terapin.png"))
  "initialize some globals"
  (defparameter *width* w)
  (defparameter *height* h)
  (defparameter *file* title)
  (init-terp turtle (/ *width* 2) (/ *height* 2)))

(defun reset-terp (turtle)
  (setf (x turtle) (/ *width* 2))
  (setf (y turtle) (/ *height* 2)))

(defmacro terp-go (&rest body)
  "the basic terrapin macro. Send terp wherever you want, with whatever code."
  (init-func turtle)
  (reset-terp turtle)
  `(with-canvas (:width *width* :height *height*)
     (rectangle 0 0 *width* *height*)
     (set-rgb-fill 1 1 1)
     (fill-path)
     ,@body
     (save-png *file*)))


