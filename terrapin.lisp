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


(defun to-radians (angle)
  "basic trig shit"
  (* angle (/ pi 180)))

(defmacro goto (x y &optional (turtle 'turtle))
  "moves to point without drawing a line"
  `(setf (x ,turtle) ,x)
  `(setf (y ,turtle) ,y))


(defmacro toggle-pen (&optional (turtle 'turtle))
  "toggles pen up and down"
  `(if (pen ,turtle)
     (setf (pen ,turtle) nil)
     (setf (pen ,turtle) t)))

(defmacro pen-up (&optional (turtle 'turtle))
  `(setf (pen ,turtle) nil))

(defmacro pen-down (&optional (turtle 'turtle))
  `(setf (pen ,turtle) t))

(defmacro right (degrees &optional (turtle 'turtle))
  "add degrees to turtle angle"
  `(setf (angle ,turtle) (+ (angle ,turtle) ,degrees)))

(defmacro left (degrees &optional (turtle 'turtle))
  "left is one backwards right"
  `(right (- ,degrees) ,turtle))

(defmacro forward (steps &optional (turtle 'turtle))
  "move forward at angle. if pen-down is t, draw a line"
  `(let ((x (x ,turtle))
         (y (y ,turtle))
         (angle (angle ,turtle)))
     (let ((dest-x x)
           (dest-y y))
       (setf dest-x (+ dest-x (* ,steps (cos (to-radians angle)))))
       (setf dest-y (- dest-y (* ,steps (sin (to-radians angle)))))
       (if (pen ,turtle)
         (progn
           (v:move-to (x ,turtle) (y ,turtle))
           (v:line-to dest-x dest-y)))
       (setf (x ,turtle) dest-x)
       (setf (y ,turtle) dest-y)
       (v:stroke))))

(defmacro back (steps &optional (turtle 'turtle))
  "backin up"
  `(forward (- ,steps) ,turtle))

(defmacro init-terp (name x y)
  "creates a turtle object"
  `(defvar ,name (make-instance 'terp :x ,x :y ,y)))

(defun init (w h &optional (title "terapin.png"))
  "initialize some globals"
  (defparameter *width* w)
  (defparameter *height* h)
  (defparameter *file* title)
  (init-terp turtle (/ *width* 2) (/ *height* 2)))

(defun reset-terp (turtle)
  (setf (x turtle) (/ *width* 2))
  (setf (y turtle) (/ *height* 2))
  (setf (angle turtle) 0))

(defmacro terp-go (&rest body)
  "the basic terrapin macro. Send terp wherever you want, with whatever code."
  (reset-terp turtle)
  `(v:with-canvas (:width *width* :height *height*)
     (v:rectangle 0 0 *width* *height*)
     (v:set-rgb-fill 1 1 1)
     (v:fill-path)
     ,@body
     (v:save-png *file*)))


