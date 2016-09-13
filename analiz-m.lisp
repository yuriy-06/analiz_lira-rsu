(in-package #:analiz)
(defun analiz-m (&key node-f ke-f rsu-f);возвращает массив выбранных координат и РСУ для каждого элемента *** 
;вызов вида (analiz-m  :node-f "c:/lisp/node.csv" :ke-f "c:/lisp/ke.csv" :rsu-f "c:/lisp/rsu.csv")
  (let ((n 0) (mk 0) (my 0) (qz 0) (mz 0) (qy 0))
    (setf list-nodes (nodes node-f))
    (setf list-ke (ke ke-f))
    (setf list-rsu-csv (rsu-split-parallel (file-m rsu-f)))
    (setf m nil)
    (dolist (x list-ke)      
      (setf s (split ";" (filter x))
	    v-ke (first s)
	    v-node1 (second s)
	    v-node2 (third s)
	    v-node1 (coordinats list-nodes v-node1);здесь v-node1 становиться списком из 3-х координат
	    v-node2 (coordinats list-nodes v-node2)
	    v-rsu (min-max-rsu-for-ke list-rsu-csv v-ke);возвращает мин-макс РСУт для данного КЕ
	    v (list v-ke v-node1 v-node2 v-rsu)
	    m (append m v)))
    (values m)))

(defun analiz-m-test ()                ;(analiz-m-test)
  (labeled-time (setf v1 (analiz-m  :node-f "c:/lisp/nodes.csv" :ke-f "c:/lisp/ke.csv" :rsu-f "c:/lisp/rsu.csv")))
;  (labeled-time (setf v2 (analiz-m-parallel  :node-f "c:/lisp/nodes.csv" :ke-f "c:/lisp/ke.csv" :rsu-f "c:/lisp/rsu.csv")))  
  (+ 1 1))
