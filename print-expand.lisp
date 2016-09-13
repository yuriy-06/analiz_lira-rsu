(in-package :analiz)

(let (x)
  (defmacro two-values (x)
    `(values (list ',x ,x)))
  )

  
(defun eval-list (l)  ;;   (eval-list '(w c))
  (let (m) 
    (dolist (x l) (push (eval x) m))
    (values (reverse m)))
  )

(defun print** (l) ;; (print** '(w c 1.4))
  (let (m)
    (dolist (x l) (setf m (append m (list x "*"))))
    (values (nbutlast m))))
	
  
(defun print* (message list-param units) ;; (print* "hi!="  '(w c 1.4) "тс")
  (let (out-val)
    (setf out-val (apply #'* (eval-list list-param)))
    (format t "~a~%" message)
    (format t "~{~a~}" (append (print** list-param) (list "=")))
	(format t "~a" (print** (eval-list list-param)))
    (format t "~a~a~a~%" "=" (ocrugl-1.000 out-val) units)
    ))