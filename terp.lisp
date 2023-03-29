(ql:quickload :vecto)
(defpackage :terp
  (:use :cl :vecto))

(in-package :terp)

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
   (pen-down
    :initarg :pen-down
    :initform t
    :accessor pen-down)))

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
    (if (pen-down turtle)
      (setf (pen-down turtle) nil)
      (setf (pen-down turtle) t)))

  (defun left (degrees)
    "add degrees to turtle angle"
    (let ((a (angle turtle)))
      (setf (angle turtle) (+ a degrees))))

  (defun right (degrees)
    "right is one backwards left"
    (left (- degrees)))

  (defun forward (steps)
    "move forward at angle. if pen-down is t, draw a line"
    (let ((x (x turtle))
          (y (y turtle))
          (angle (angle turtle)))
      (let ((dest-x x)
            (dest-y y))
        (setf dest-x (+ dest-x (* steps (cos (to-radians angle)))))
        (setf dest-y (- dest-y (* steps (sin (to-radians angle)))))
        (if (pen-down turtle)
          (progn
            (move-to (x turtle) (y turtle))
            (line-to dest-x dest-y)))
        (setf (x turtle) dest-x)
        (setf (y turtle) dest-y)
        (stroke))))

  (defun back (steps)
    "backin up"
    (forward (- steps))))

(defun init (w h &optional (title "terp.png"))
  "initialize some globals"
  (defparameter *width* w)
  (defparameter *height* h)
  (defparameter *file* title))

(defmacro terp-go (&rest body)
  "the basic terrapin macro. Send terp wherever you want, with whatever code."
  `(let ((turtle (make-instance 'terp :x (/ *width* 2) :y (/ *height* 2))))
     (init-func turtle)
     (with-canvas (:width *width* :height *height*)
       (rectangle 0 0 *width* *height*)
       (set-rgb-fill 1 1 1)
       (fill-path)
       ,@body
       (save-png *file*))))
