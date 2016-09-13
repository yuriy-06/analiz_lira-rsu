(in-package #:analiz)
(defun grad (a)
  (values (/ (* 180 a) pi)))
(defun rad (a)
  (values (/ (* pi a) 180)))
(defun circle-x-y (&key R step-alfa (start-alfa 0) (end-alfa 360) (dx 0) (dy 0))
;выводит координаты дуги окружности, со смещением от начала координат
  (let (m x y )
    (loop
       (setf 
	x (ocrugl-1.00 (+ dx (* (cos (rad start-alfa )) R)))
	y (ocrugl-1.00 (+ dy (* (sin (rad start-alfa )) R)))
	start-alfa (+ start-alfa step-alfa))
       (if (> start-alfa end-alfa) (return (reverse m))(push (list x y) m)))))
	 
(defun format-list (m) ;(format-list (circle-x-y :r 3.315 :step-alfa 22.5 :dx 5.23 :dy 6.77))
  ;печатает список в столбик
  (dolist (x m) (format t "~a~%" x)))
