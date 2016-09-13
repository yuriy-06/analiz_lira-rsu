(in-package #:analiz)

(defun yc-trans (y yc)
  (let ((dy (- yc y)))
    (values (+ yc dy))))
(defmacro text-m (n number text-usil);макрос сокращает выражение по выводу текста в svg
  `(text scene (:x (+ (* x3 mn-i)  dx) :y (+ (* y3 mn-i)  dy (* ,number font-size)))
     (tspan (:fill "red" :font-weight "bold" :font-size font-size) (concatenate 'string ,text-usil (write-to-string  (first ,n)) ";" (write-to-string  (second ,n))))))

(defun nodes-filter (list-nodes x n) ;пример вызова (nodes-filter list-nodes x n)
  (let (m v i)
    (cond
     ((equal x "x")(setq i 1))
     ((equal x "y")(setq i 2))
     ((equal x "z")(setq i 3)))
    (dolist (var list-nodes)
      (progn 
        (setq v (split ";" (filter var)))
;сравнивает 0 и 0.000 (строка ниже) и  они почему-то не равны --- вылечил приведением типов (parse-float n)
        (if (equal (parse-float (nth i v)) (float n)) 
            (setf m (append m (list var ))))))
    (values m)))
  
(defmacro ploskost (x list-x font-size)
  `(if (equal ,list-x nil) ()
     (dolist (n ,list-x) 
       (progn 
         (setq list-nodes (nodes-filter list-nodes-n ,x n))
         (print (concatenate 'string "координата   " (write-to-string n)))
         (analiz-ris :font-size ,font-size :list-nodes-n list-nodes :list-ke list-ke 
                     :out-svg (concatenate 'string out-svg "_" ,x "_" (write-to-string n) ".svg") 
                     :number-10 i-ke :value-list-usil value-list-usil :alfa-z 0)))))

(defun main-ris (&key list-x list-y list-z value-list-usil)
;пример вызова (main-ris :list-x (list 0 7.7 15.4) :list-y nil :list-z nil :value-list-usil '(n qz)); списки могут быть (nil)

;(main-ris :list-x (list 31.78) :list-y nil :list-z nil :value-list-usil '(n))
(let* (
       (*file* (open "C:/lisp/analiz.ini" :if-does-not-exist nil))
       (f (read *file* nil))
       (font-size (nth 0 f))
       (file-node-csv (nth 1 f))
       (file-ke-csv (nth 2 f))
       (file-rsu-csv (nth 3 f))
       (out-svg (nth 4 f))
       list-ke n-ke i-ke  list-nodes-n
       nx ny text list-nodes
       ) 
  (multiple-value-setq (list-ke n-ke) (ke file-ke-csv)); 
  (setf list-ke (filter-str-list list-ke))
  (setq i-ke (floor (/ n-ke 10))) ;вычислили десятую часть числа КЕ (в целом числе)
  (print "считали КЕ")    
  (setq *list-rsu-csv* (rsu-split (file-m file-rsu-csv)))
  (print "считали RSU")
  (setq list-nodes-n (filter-str-list (nodes file-node-csv)))
  (print "считали NODES")  
  ;после этой строчки мы загоняем список узлов через фильтр, который оставит только узлы соотвсетствующих плоскостей
  (print "начали работу по плоскостям")
  (print "плоскость Х")
  (ploskost "x" list-x font-size)
  (print "прогнали плоскость Х")
  (print "плоскость Y")
  (ploskost "y" list-y font-size)
  (print "прогнали плоскость Y")
  (print "плоскость Z")
  (ploskost "z" list-z font-size)
  (print "прогнали плоскость Z")
  (close *file*)))

;выводит в картинку элементы с усилиями выписанными на них (макс мин)
(defun analiz-ris (&key font-size list-nodes-n list-ke out-svg number-10 geom-p value-list-usil alfa-z)
  (let (m2 nx ny m-analiz  list-nodes ii pr priznak v-ke v-node1 v-node2 v-rsu x1 x2 y1 y2 x-max x-min y-max y-min
           xc yc  py v mn-x mn-y dx dy px px2 py2 x3 y3 n1 n2 n3 n4 n5 n6 mn mn-i text)
    (setf list-nodes list-nodes-n   ;чертовы глобальные переменные - используется другой массив
			list-nodes (coordinats-split list-nodes)
          ii 0 ; счетчик циклов
          pr (list "10%" "20%" "30%" "40%" "50%" "60%" "70%" "80%" "90%" "100%") ; 
          priznak (geom list-nodes geom-p))
; здесь надо отсортировать ке по признаку присутствия в плоскости !!!!!!!!!!!!!!!!!
	(format t "~%~a~%" "начался обсчет массива РСУ и узлов")
	;(break "some text")
    (dolist (x (ke-filter list-ke list-nodes geom-p))
      (progn 
        (incf ii)
        (if (= ii number-10)
            (progn
              (print (pop pr))
              (setq ii 1)))
        (setf
         v-ke (first x)
         *v-ke* v-ke
                              ; *list-v-ke* (list v-ke)
;здесь v-node1 становиться списком из 3-х координат
 ;а если узел равен nil, то делаем прерывание этой итерации цикла  
         v-node1 (second x) 
         v-node2 (third x) 
         v-node1 (coordinats-povorot list-nodes v-node1 alfa-z)  ; можно оптимизировать и ввести проверки
         v-node2 (coordinats-povorot list-nodes v-node2 alfa-z))
        (if (not (typep v-ke 'list)) (setf *list-v-ke* (list v-ke))(setf  *list-v-ke* v-ke))
                       
        (if (or (equal v-node1 nil)(equal v-node2 nil)) ()
	    (progn
                             (m-list-rsu*)
                             (setf v-rsu (min-max-rsu-for-ke);здесь возвращаются мин-макс РСУ для данного КЕ
                                   x1  (nth-value 0 (nodes-print priznak v-node1))
                                   y1 (nth-value 1 (nodes-print priznak v-node1))
                                   x2  (nth-value 0 (nodes-print priznak v-node2))
                                   y2  (nth-value 1 (nodes-print priznak v-node2))
                                   text  v-ke ;
                                   v (list x1 y1 x2 y2 v-rsu v-ke)
                                   nx (append nx (list x1 x2));здесь выводим все узлы для нахождения макс мин из всех
                                   ny (append ny (list y1 y2))
                                   m-analiz (append m-analiz (list v)))))))
	(format t "~%~a~%" "закончился обсчет массива РСУ и узлов")
	
	(format t "~%~a~%" "начался обсчет изображения")
  ;(setq m (super-kb m "c:/lisp/kb.txt"))
    (setf nx (usil-min-max-list nx) ; можно оптимизировать
          ny (usil-min-max-list ny));габариты изображения
    (setf x-max (nth 1 nx) 
          x-min (nth 0 nx) 
          y-max (nth 1 ny) 
          y-min (nth 0 ny))
    (setf px (- x-max x-min)
          py (- y-max y-min); без запасов по краям
          yc (/ (+ y-max y-min) 2 );центр тяжести по Y вычисляется  !!!здесь была ошибка + на минус
          xc (/ (+ x-max x-min) 2 ))
	(cond ((= px 0)(setf px 20))
	  ((= py 0)(setf py 20)))
;затем пересчитываются координаты, приводятся к координатам изображения
    (dolist (x m-analiz)
      (progn
        (setf x1 (first x) y1 (second x) x2 (third x) y2 (fourth x)
              x1 (-  x1 x-min );начало координат изображения приводится к левому нижнему углу этого изображения
              x2 (-  x2 x-min );
              y1 (-  (yc-trans y1 yc) y-min );а игреки еще и зеркалятся (игрек svg направлен вниз)
              y2 (-  (yc-trans y2 yc) y-min );
              v-rsu (fifth x)
              v (list x1 y1 x2 y2 v-rsu)
              m2 (append m2 (list v)))))
  ;непосредственно вывод изображения
    (if (> px py)
        (setf px2 1024 py2
              768 mn-x (/ 1024 px)
              mn-y (/ 768 py))
      (setf px2 768 
            py2 1024 
            mn-x (/ 768 px) 
            mn-y (/ 1024 py)))
; mn-x mn-y - множитили приведения координат к размеру изображения
    (if (> mn-x mn-y)(setq mn mn-y)(setq mn mn-x));затем выбираем единый множитель для габаритов изображения (тот который меньше)
    (setf mn-i (/ mn 1.1);уменьшаем единый множитель, чтоб были "поля"
          dx (/ (- px2 (* px mn-i)) 2) 
          dy (/ (- py2 (* py mn-i)) 2))
		  
	(format t "~%~a~%" "начался вывод изображения в файл")

    (let ((scene (make-svg-toplevel 'svg-1.1-toplevel :height (+ (* 10 font-size) py2) :width  (+ (* 10 font-size) px2))))
      (title scene "ANALIZ")
      (draw scene (:rect :x 0 :y 0 :height (+ (* 10 font-size) py2) :width (+ (* 10 font-size) px2))
            :stroke "blue" :stroke-width 1 :fill "rgb(255, 255, 255)")
      (dolist (x m2) 
        (progn
          (setf x1 (first x) y1 (second x) x2 (third x) y2 (fourth x);извлекаем координаты из списка
                x3 (/ (+ x1 x2) 2);а вот это координаты для текста
                y3 (/ (+ y1 y2) 2)
                v-rsu (fifth x))
          (list-razbor-6 v-rsu n1 n2 n3 n4 n5 n6) ; здесь усилия разбираются по переменным с соотв-им номером
         
          (let ((i -1))
            (dolist (x value-list-usil) (cond
                                         ;здесь сравнивается имя символа с конкретным видом усилия
                                         ;(text-m n1..n6 -- вывод текста в svg , (incf i) - это номер строки
                                         ((equal (symbol-name x) "N")(text-m n1 (incf i) "N="))
                                         ((equal (symbol-name x) "MK")(text-m n2 (incf i) "Mk="))
                                         ((equal (symbol-name x) "MY")(text-m n3 (incf i) "My="))
                                         ((equal (symbol-name x) "QZ")(text-m n4 (incf i) "Qz="))
                                         ((equal (symbol-name x) "MZ")(text-m n5 (incf i) "Mz="))
                                         ((equal (symbol-name x) "QY")(text-m n6 (incf i) "Qy=")))))
          (draw  scene 
                 (:line :x1 (+ (* x1 mn-i)  dx) :y1 (+ (* y1 mn-i) dy) :x2 (+ (* x2 mn-i)  dx) 
                  :y2 (+ (* y2 mn-i) dy) :stroke "blue" :stroke-width 2)))); + s  это отступ от начала координат
      (with-open-file (s (pathname out-svg) :direction :output :if-exists :supersede)
        (stream-out s scene)))))

(defun analiz-one (geom-p  value-list-usil   &optional (alfa-z 0) (ferma nil))
; (analiz-one "xy" '(n) 0) или (analiz-one nil '(n) 0) что эквивалентно (analiz-one "izometr" '(n) 0) или (analiz-one "foo123" '(n) 0)
; (analiz-one "xy" '(n mk my qz mz qy) 0)   (analiz-one "xz" '(n) 0 t)
  (if (equal ferma t)(setf *ferma* t) (setf *ferma* nil))  ;выставляем или сбрасываем глобальную переменную
  (let* ((file (open "C:/lisp/analiz.ini" :if-does-not-exist nil))
        (f (read file nil))
        (font-size (nth 0 f))
        (file-node-csv (nth 1 f))
        (file-ke-csv (nth 2 f))
        (file-rsu-csv  (nth 3 f))
        (out-svg (nth 4 f))
		;данная строчка устанавливает переменную, которая будет глобальной по отношению к нижележащим функциям
		;отсюда планировщик потоков будет брать свои данные  !!!!!!!!!!!!!!!!!!!!!!!!!
        ;(*list-rsu-csv* (rsu-split (rsu-csv file-rsu-csv)))  
		;rsu-split-parallel  перемешивает строки и тогда неправильно работает выбор мин макс рсу
		(*list-rsu-csv* (rsu-split (rsu-csv file-rsu-csv)))  
	 (list-nodes-n (filter-str-list (nodes file-node-csv)))
		(i-ke) (list-ke) n-ke )
    (close file)
    (multiple-value-setq (list-ke n-ke) (ke file-ke-csv));
    (setf list-ke (filter-str-list list-ke))
    (setf i-ke (floor (/ n-ke 10)));вычислили десятую часть числа КЕ (в целом числе)
    (analiz-ris :font-size font-size :list-nodes-n list-nodes-n :list-ke list-ke 
                :out-svg out-svg :number-10 i-ke :geom-p geom-p :value-list-usil value-list-usil :alfa-z alfa-z)))
