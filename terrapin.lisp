(in-package #:terrapin)

;; this is the tertle object
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

;; basic functionality 
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

(defmacro terp (&rest body)
  "the basic terrapin macro. Send terp wherever you want, with whatever code."
  (reset-terp turtle)
  `(v:with-canvas (:width *width* :height *height*)
     (v:rectangle 0 0 *width* *height*)
     (v:set-rgb-fill 1 1 1)
     (v:fill-path)
     ,@body
     (v:save-png *file*)))

;; some shapes n stuff
(defun squarepiece (size)
  "draws a segment of <size> and then turns right 90 degrees. Basically just used for making boxes I guess"
  (forward size)
  (right 90))

(defun square (size)
  "makes a square where each side is of <size>."
  (dotimes (i 4)
    (squarepiece size)))

(defun quad (side1 side2)
  "draws a quadrilateral with segment lengths <side1> and <side2>"
  (dotimes (i 2)
    (squarepiece side1)
    (squarepiece side2)))

(defun triangle (size)
  "draws a triangle with segment length of <size>."
  (dotimes (i 3)
    (forward size)
    (right 120)))

(defun arc-r (radius angle)
  "draws a right-moving arc of <radius> and <angle>."
  (let ((conv (/ 360(* 2 pi))))
    (dotimes (i angle)
      (forward (/ radius conv))
      (right 1))))

(defun arc-l (radius angle)
  "draws a left-moving arc of <radius> and <angle>."
  (let ((conv (/ 360 (* 2 pi))))
    (dotimes (i angle)
      (forward (/ radius conv))
      (left 1))))

(defun circle (radius)
  "draws a circle of <radius>."
  (let ((limit-r (/ 360 (* 2 pi))))
    (dotimes (i 360)
      (forward (/ radius limit-r))
      (right 1))))

(defun poly (side angle)
  "draws a polygon where each segment is <side> length and has <angle> between segments. Drawing ends when the polygon has closed."
  (do ((turn 0 (+ turn angle)))
      ((and (not (eq 0 turn)) (eq 0 (mod turn 360))))
    (forward side)
    (right angle)))
