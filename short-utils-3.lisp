(in-package #:analiz)

;макрос присваивает каждой переменной в списке значение value
(defmacro setn-list (list* value)   ;(setn-list '(n1 n2 n3) 0) ;(setn-list '(n1 n2 n3) nil)
  `(dolist (x ,list*) (set x ,value)))

(defmacro set-list* (list* value-list)   ;  (set-list* (list 0 1 2 3) '(n1 n2 n3 n4))
  `(let ((l* (list-length ,value-list)))    ;негигееничный макрос l* совпадает
     (dotimes (i l*) (set (nth i ,value-list) (nth i ,list*)))))

(defmacro setf-list (list* value-list)   ;  (setf-list (list 0 1 2 3) (list n1 n2 n3 n4))
  `(let ((l* (list-length ,value-list)))
     (dotimes (i (- l* 1)) (setf (nth i ,value-list) (nth i ,list*)))))

(defun list-srez***  (list* n1 n2)   ;  (list-srez*** (list 0 1 2 3 4) 1 3)  =>  (1 2 3)   самый быстрый
	(declare (fixnum n1 n2 i))
  (let (m (i 0))
	(if (> n1 n2) (return-from list-srez*** "n1>n2"))
    (dolist (x list*) (progn (if (<= n1 i n2) (setf  m  (append m (list x)))) (incf i))) 
    (values m)))
	

(defun usil-min-max-list (list);возвращает минимум и максимум из списка
  (let ((n-max (first list)) (n-min (first list))) 
    (dolist (x list)(progn (if (> x n-max) (setq n-max x))
                      (if (< x n-min)(setq n-min x))))
    (values (list n-min n-max))))

(defun max-mod (n1 n2)
  (if (> (abs n1)(abs n2)) (abs n1) (abs n2)))

(defun filter-nil (list*) ;; 
  (let (m)
    (dolist (x list*) (if (equal x nil) () (push x m) ))
    (values m)))

(defun min-max-ex (list* &optional t-list) ;; (min-max-ex (list 0 2 4 -9 89))  (min-max-ex (list 0 2 4 -9 89) t) => (-9 89)
  (declare (optimize (speed 3)))
  (let (min max)
    (if (typep (first list*) 'string) (setq list* (mapcar #'parse-float list*))) ;; если список из строк, превращаем его в список чисел
    (setf min (first list*) max (first list*))
    (dolist (x list*) 
      (cond ((> min x) (setq min x))
            ((< max x) (setq max x))))
    (if (equal t-list nil) (values min max) (values (list min max)))))

(defmacro ocrugl-float (name-func number-float)
  `(defun ,name-func (x)
     (if (equal x nil) (values nil) (values (float (/ (round (* x ,number-float)) ,number-float))))))
(ocrugl-float ocrugl-1.00 100) ;создает функции округления (имя функции и еденицы после дроби)
(ocrugl-float ocrugl-1.000 1000)
(ocrugl-float ocrugl-1.00000 100000)

(defun ocrugl-100 (x)
  (values  (* (round (/ x 100)) 100)))

(defun ocrugl-list-1.00 (list-x);(ocrugl-list-1.00 (list 21.35689 -22.69998 -5.163 6.123))
  (let (m-ocr)
    (dolist (x list-x)
      (setq m-ocr (append m-ocr (list  (ocrugl-1.00 x)))))
    (values m-ocr)))

(defmacro list-razbor-6 (list-r n1 n2 n3 n4 n5 n6)
  `(setf ,n1 (first ,list-r) ,n2 (second ,list-r) ,n3 (third ,list-r) ,n4 (fourth ,list-r) ,n5 (fifth ,list-r) ,n6 (sixth ,list-r)))

(defun list-clear (list*) ; очищает список от nil и ""   пример вызова ----  (list-clear (list "" nil "5.23"))
  (let ((m (list )))
    (dolist (x list*) (if (or (equal x nil) (equal x "")) ()(setf m (append m (list x)))))
    (values m)))
	
(defmacro collector (n1 n2)
	`(setf ,n1 (append ,n1 ,n2)))

(defmacro collector-l (n1 n2)  ; (collector-l n1 n2)
	`(setf ,n1 (append ,n1 (list ,n2))))

(defmacro collector-destroy (n1 n2 n3)
	`(setf ,n1 (append ,n1 ,n2) ,n3 nil))

(defmacro sum-list (val list*)
  `(progn (setf ,val 0)
    (dolist (x ,list*) (setf ,val (+ x ,val)))))

(defun base-t (val list*)  ;; проверяет, есть ли это значение в базе
  (dolist (x list*) (if (equal x val)(return t))))
  
(let (message list*)
  (defmacro print-expand (message list*)   ;; пример вызова (print-expand "расчет ветра W=" '(* w c 1.4))
    `(let (m f-m m-out (i 0))
       (format t "~a" message) ;;выводим сообщение "расчет ветра W="
       (setf m (rest list*)  ;; отбираем аргументы функции
	     f-m (first list*))  ;; отбираем наименование операции
       (dolist (x m) (progn (push f-m m-out)(push x m-out))) ;; собираем массив вида (* 1,4 * с * w)
       (setf m-out (reverse m-out)  ;; берем rest ,  (rest (* 1,4 * с * w)) => (1,4 * с * w)
	     m-out (rest m-out)) ;;  (reverse (1,4 * с * w)) => (w * c * 1.4)
       (format t "~{~a~}" m-out)  ;; печатаем все
       (format t "~a" "=")
       (dolist (x m-out) 
	 (progn (incf i)
		(if (evenp i) (format t "~a" x) (format t "~a" (eval x)))))
       (format t "~a~a" "=" ,list*)    
       (format t "~a" #\Newline))))
	   
(defun set-python (list*)
  (let (m)
    (dolist (x list*)
      (if (base-t x m) ()(setf m (append m (list x))))) ;; если нет элемента в базе, добавить его
    (values m)))
