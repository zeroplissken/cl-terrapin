(in-package :terrapin)

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
