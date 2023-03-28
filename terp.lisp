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
    :initform nil
    :accessor pen-down)))

(defun init-func (turtle)
  (defun to-radians (angle)
    (* angle (/ pi 180)))

  (defun goto (x y &optional (angle 0))
    (setf (x turtle) x)
    (setf (y turtle) y)
    (setf (angle turtle) angle))

  (defun left (degrees)
    (let ((a (angle turtle)))
      (setf a (+ degrees))
      (if (< a 0) (setf a (+ a 360)))
      (if (> a 360) (setf a (- a 360)))))

  (defun right (degrees)
    (left (- degrees)))

  (defun forward (steps)
    (let ((x (x turtle))
          (y (y turtle))
          (angle (angle turtle)))
      (let ((dest-x x)
            (dest-y y))
        (setf dest-x (+ dest-x (* steps (cos (to-radians angle)))))
        (setf dest-y (- dest-y (* steps (sin (to-radians angle)))))
        (move-to x y)
        (line-to dest-x dest-y)
        (stroke))))

  (defun back (steps)
    (forward (- steps))))

(defun init (w h &optional (title "trtle.png"))
  (defparameter *width* w)
  (defparameter *height* h)
  (defparameter *file* title))

(defmacro terp-go (&rest body)
  `(let ((turtle (make-instance 'trtl :x (/ *width* 2) :y (/ *height* 2))))
     (init-func turtle)
     (with-canvas (:width *width* :height *height*)
       (rectangle 0 0 *width* *height*)
       (set-rgb-fill 1 1 1)
       (fill-path)
       ,@body
       
       (save-png *file*))))
