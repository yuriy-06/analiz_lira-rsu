(in-package #:analiz)

(defvar *i* 0)
;(defvar m2* nil)

(defun pl (list)
;возвращает t если все члены списка равны друг другу 
;(требуется для автоматического определения координатной плоскости узлов)
  (let ((x1 (first list)))
    (dolist (x list)(if (equal x1 x)()(return-from pl nil)))
    (values T)))

(defmacro if-cond (p1 p2)
  `(equal ,p1 ,p2))
(defmacro geom-cond (p1 p2 p3)
  `(cond 
    (,p1(return-from geom "yz"))
    (,p2(return-from geom "xz"))
    (,p3(return-from geom "xy"))))

(defun geom (nodes geom-p)
;определяет плоскость расположения узлов или признак изометричного расположения (на вход массив строк nodes)
;geom-p необходим для принудительного задания плоскости
  (let (x-list y-list z-list s x z)
    (geom-cond (if-cond geom-p "yz")
               (if-cond geom-p "xz")
               (if-cond geom-p "xy"))
    (dolist (v nodes) 
      (progn 
        (setf x (second v)
              y (third v)
              z (fourth v)
              x-list (append x-list (list x))
              y-list (append y-list (list y))
              z-list (append z-list (list z)))))
    (geom-cond (if-cond (pl x-list) T)  
               (if-cond (pl y-list) T) 
               (if-cond (pl z-list) T))
    (values "izometr")))

	
(defun coordinats-split (list-nodes)
  (let (m)
    (dolist (x list-nodes) (setf m (append m (list (split ";" x)))))
    (values m)))

(defun coordinats* (list-nodes v-node);возвращает координаты конкретного узла
  (let (s node v)
    (dolist (x list-nodes)
      (progn
        (setf node (first x))
        (if (equal node v-node) 
            (progn (setf v (rest x))
              (return-from coordinats* v)))))
    (format t "~a~a~%" "функци¤ coordinats: нет такого узла " v-node)
    (values nil)));если такого узла нет


(defun nodes-print (priznak nodes);возвращает координаты точки конкретного изображения
    ;(if (equal nodes nil) (return-from nodes-print  (values 0 0)));здесь обработка ошибки
  (let ((y 0) i1 i2 xx yy)
    (cond ((equal priznak "xy") (setf i1 0 i2 1))
          ((equal priznak "xz") (setf i1 0 i2 2))
          ((equal priznak "yz") (setf i1 1 i2 2))
          ((equal priznak "izometr") (setf i1 0 i2 2 y 0.7)))
    (setf xx (+ (parse-float(nth i1 nodes)) (* y (parse-float (nth 1 nodes))))
          yy (+ (parse-float(nth i2 nodes)) (* y (parse-float (nth 1 nodes)))))
    (values xx yy)))

(defun geom-test ()                ;(geom-test)
  (labeled-time (coordinats (nodes "c:/lisp/nodes.csv") "1853"))
  ;(labeled-time (coordinats-parallel (nodes "c:/lisp/nodes.csv") "1853"))
  (+ 1 1))

(defun grad-to-rad (grad)  ; (grad-to-rad 90) => 1.5707963267948966D0
; (ocrugl-1.00 (cos (grad-to-rad 45))) = > 0.71
  (values (* grad (/ pi 180))))
(defun povorot-xy (x y alfa)
  (setf  alfa (grad-to-rad alfa)
	 x1 (+ (* x (ocrugl-1.00000 (cos alfa)))(* y (ocrugl-1.00000 (sin alfa))))
	 y1 (- (* y (ocrugl-1.00000 (cos alfa))) (* x (ocrugl-1.00000 (sin alfa)))))
  (values x1 y1))

(defun coordinats-povorot (list-nodes v-node1 alfa-z);  можно оптимизировать и ввести проверки
  (let* ((x (coordinats* list-nodes v-node1))
         (v1 (first x)) (v2 (second x)) (v3 (third x))
         (v1* (parse-float v1)) (v2* (parse-float v2)) (v1**) (v2**))
    (multiple-value-setq (v1** v2**) (povorot-xy v1* v2* alfa-z))
    (values (list (write-to-string v1**)(write-to-string v2**) v3))))

(defun sort-ke (m2*)
   (let* (i (index 0) m3 (l (list-length m2*)) (n (round (/ l 4))))
     (dotimes (index n) 
       (progn
         (setf i (pop m2*))
         (dolist (j m2*)
           (if (equal-ke i j)               ;если два КЕ совпадают по выбранным координатам отображения
               (setf i (merge-ke-f i j)        ;сливаю их в список
		     m2* (remove j m2*))))
         (setf  m2* (append   m2* (list i))))))
   (values m2*))

(defun ke-filter (list-ke list-nodes geom-p)
  (let (nodes-base nodes-base* v v-node v-node1 v-node2 node1 node2 node11 node12 node21 node22 m m2 base-ke m2*)
	(format t "~%~a~%" "начался процесс фильтрации списка КЕ ")
    (setf m2* nil)
    (dolist (x list-nodes) 
      (progn (setf 
              v-node (first x))
	     (push v-node nodes-base)  ;забиваем номера узлов  в базу nodes-base
	     (push x nodes-base*)))  ;забиваем порезанные узлы  в базу nodes-base* 
    
    (dolist (x list-ke) 
      (progn
        (setf v (split ";" x)
              v-node1 (second v)
              v-node2 (third v))
        (if (and (base-t v-node1 nodes-base) (base-t v-node2 nodes-base))
	    (progn  (collector-l m v)))))       ;если оба узла этого КЭ лежат в нужной плоскости - тогда оставляем его
    ;;таким образом сокращаем количество итераций по выбору РСУ
    (if (equal *ferma* t)  ;если параметр *ferma* говорит нам считать ферму
        (progn  ;выполняем код
          (cond 
	    ((equal "xy" geom-p) (progn
                                  (defun first* (x) (first x))
                                  (defun second* (x) (second x))))
           ((equal "xz" geom-p) (progn
                                  (defun first* (x) (first x))
                                  (defun second* (x) (third x))))
           ((equal "yz" geom-p) (progn
                                  (defun first* (x) (second x))
                                  (defun second* (x) (third x))))) ;в зависимости от выбранного параметра назначаем функции выбора узлов
          (dolist (x m)
	    (progn
	      (setf
	       ke-v (first x)
	       v-node1 (second x)
	       v-node2 (third x)
	       node1 (coordinats*  nodes-base*  v-node1)
	       node2 (coordinats*   nodes-base* v-node2)
	       node11 (first* node1)             ;получаем 
	       node12 (second* node1)
	       node21 (first* node2)
	       node22 (second* node2))
	      (push (list x ke-v node11 node12 node21 node22) m2*))))
	(return-from ke-filter m))  ;если *ferma* равна nil --> выходим и выводим отсюда m
   ; (setf m2* (merge-del ))
    (setf m (sort-ke m2*))
    (setf m2* nil)
    (dolist (x m) (collector-l m2 (first x)))
    (format t "~a~%" "закончился процесс фильтрации КЕ")
    (values m2)))

(defun equal-ke (i j)
  (if (equal (list-srez*** i 2 5)(list-srez*** j 2 5)) (values t)))

(defun merge-ke-f (i j)
  (let (m m1 i1 j1 i1* j1* ke1 ke2 hvost node1 node2)
    (setf i1 (first i)        ;первый список КЕ --> ("123" "4" "16")
          j1 (first j)                      ;второй список КЕ
          i1* (first i1)
          j1* (first j1)
          node1 (second i1)
          node2 (third i1)
          ke1 (second i)                        ;
          ke2 (second j)
          hvost (list-srez*** i 2 5)
          h1 (first hvost)
          h2 (second hvost)
          h3 (third hvost)
          h4 (fourth hvost))
   ; (incf *i*)
    (if (not (typep i1* 'list)) (setf i1* (list i1*)))
    (if (not (typep j1* 'list)) (setf j1* (list j1*)))
    (if (not (typep  ke1 'list)) (setf ke1 (list ke1)))
    (if (not (typep  ke2 'list)) (setf ke2 (list ke2)))

    (values (list (list (append i1* j1*) node1 node2) (append ke1 ke2) h1 h2 h3 h4))))

(defun out-ke-f (list*)
  (let (m)
    (dolist (x list*) 
      (if (not (typep (first x) 'list))()))))

(defun test ()
  (setf a (list 0 1 2 3 4))
  (dolist (x a)
    (progn 
      (princ x)
      (setf a (remove x a))))
  (princ a))

(defun test2 ()
  (setf a (list 0 1 2 3 4))
  (dolist (x a)
    (progn 
      (princ a)
      (setf a (remove 3 a)))))
