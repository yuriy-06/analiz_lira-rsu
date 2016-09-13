(in-package #:analiz)

(defvar n '())
(defvar mk nil)
(defvar my nil)
(defvar qz nil)
(defvar mz nil)
(defvar qy nil)
(defvar n-v nil)
(defvar mk-v nil)
(defvar my-v nil)
(defvar qz-v nil)
(defvar mz-v nil)
(defvar qy-v nil)
(defvar ke nil)

(defmacro mult* (n1 n2 n)
  `(multiple-value-setq (,n1 ,n2) (min-max-ex ,n)))

(defmacro mult-min-max* ()
  `(progn
     (mult* n-min n-max n)
     (mult* mk-min mk-max mk)
     (mult* my-min my-max my)
     (mult* qz-min qz-max qz)
     (mult* mz-min mz-max mz)
     (mult* qy-min qy-max qy)))

(defmacro usil* ()  ; (usil*)
  `(progn
     (collector-l n n-v)
     (collector-l mk  mk-v)
     (collector-l my  my-v)
     (collector-l qz  qz-v)
     (collector-l mz  mz-v)
     (collector-l qy  qy-v)))

(defmacro f* (n n1 n2)
  ` (format t "~a~a~a~a~%" ,n (ocrugl-1.00 ,n1)  " " (ocrugl-1.00 ,n2)))
(defmacro format-str* ()
  `(progn
     (format str "~a~a~a~a~a~a~a~a~a~a~a~a~a~%" 
             ke-v ";" n-min ";" mk-min ";" my-min ";" qz-min ";" mz-min ";" qy-min ";")
     (format str "~a~a~a~a~a~a~a~a~a~a~a~a~a~%" 
             ke-v ";" n-max ";" mk-max ";" my-max ";" qz-max ";" mz-max ";" qy-max ";")))

(defun analiz ();Вычисляет мин макс из ВСЕХ РСУ, для всех КЕ ||||| пример вызова (analiz (rsu-list**))
  (declare (optimize (debug 3)))
  (let (n-min n-max mk-min mk-max my-min my-max qz-min qz-max mz-min mz-max qy-min qy-max 
        n mk my qz mz qy rsu-list)
    (setf rsu-list (pop *test*))
    (dolist (x rsu-list) 
      (setf 
       n (append n (list (second x)))
       mk (append mk (list (third x)))
       my (append my (list (fourth x)))
       qz (append qz (list (fifth x)))
       mz (append mz (list (sixth x)))
       qy (append qy (list (seventh x)))))
    (mult-min-max*)
    (push (list nil n-min mk-min my-min qz-min  mz-min  qy-min) *test2*)
    (push (list nil n-max mk-max my-max qz-max  mz-max  qy-max) *test2*)))

(defun rsu-list () ;
  ;;(declare (optimize (debug 3)))
  (let (m m2 list-usil ke ke-v v l v1 a)
    (setf v1 (pop *rsu-list*))
	(if (equal v1 nil) (return-from rsu-list nil))
    (setf m (list-clear (mapcar #'filter-rsu-txt v1)))
    (dolist (x m) (setf m2 (append m2  (list (regex-replace-all "C??\\s??[A-D]+[1-2]+" x "A ")))))
    (setf m nil)
    (dolist (x m2) (setf m (append m (list (regex-replace-all "C" x " ")))))
    (dolist (x m) 
      (progn
	(setf v  (nth-value 1 (scan-to-strings  "\\d*\\s+[A-D]+\\s*[A-D]*[1-2]*?(\\d*.+\\d*\\s+)" x)))
	(setf ke-v  (nth-value 1 (scan-to-strings  "^[|]+\\s*(\\d+)\\s+\\d+\\s+\\d+\\s+\\d+\\s+[A-D]+" x)))
	(if (equal ke-v nil)()(setf ke (aref ke-v 0)))
	(if (equal v nil) () 
            (progn
	      (setf l (mapcar #'parse-float (list-srez*** (split "\\s+" (aref v 0)) 0 6)))
	      (setq a  (list 'n 'mk 'my 'qz 'mz 'qy ))         
	      (dotimes (i 6) (set (nth i a) (nth i l)))
	      (setf      list-usil (append list-usil (list (list ke n mk my qz mz qy))))))))
	;;(break "one")
    (push list-usil *test*)))
	  
(defun rsu-list-1 () ;для ке с номерами сечений
    (let (m m2 list-usil ke ke-v v l v1 a section section-number)
      (setf v1 (pop *rsu-list*))
      (setf m (list-clear (mapcar #'filter-rsu-txt v1)))
      (dolist (x m) (setf m2 (append m2  (list (regex-replace-all "C??\\s??[A-D]+[1-2]+" x "A ")))))
      (setf m nil)
      (dolist (x m2) (setf m (append m (list (regex-replace-all "C" x " ")))))
      (dolist (x m) 
        (progn
          (setf v  (nth-value 1 (scan-to-strings  "\\d*\\s+[A-D]+\\s*[A-D]*[1-2]*?(\\d*.+\\d*\\s+)" x)))
          (setf ke-v  (nth-value 1 (scan-to-strings  "^[|]+\\s*(\\d+)\\s+\\d+\\s+\\d+\\s+\\d+\\s+[A-D]+" x))
		section (nth-value 1 (scan-to-strings  "^[|]+\\s*\\d+\\s*(\\d+)\\s+\\d+\\s+\\d+\\s+[A-D]+" x)))
          (if (equal ke-v nil)()(setf ke (aref ke-v 0)))
		  (if (equal section nil)()(setf section-number (aref section 0)))
          (if (equal v nil) () 
            (progn
	      (setf l (mapcar #'parse-float (list-srez*** (split "\\s+" (aref v 0)) 0 6)))
             (setq a  (list 'n 'mk 'my 'qz 'mz 'qy ))         
            (dotimes (i 6) (set (nth i a) (nth i l)))
            (setf      list-usil (append list-usil (list (list ke section-number n mk my qz mz qy))))))))
      (push list-usil *test*)))

(defun rsu-to-csv (out-file)  ;пример вызова (rsu-to-csv  "c:/lisp/rsu.csv")
  ;(declare (optimize (speed 3) (safety 0)))
  ;(declare (string ke n mk my qz mz qy))
  (let (ke n mk my qz mz qy l (i* 0) count pr)    
    (m-list*)
    (with-open-file (str out-file :direction :output :if-exists :supersede :external-format :latin-1)
	(setf l (* (list-length (first *test*)) *number-of-process*)
		count (round (/ l 10))
		pr (list "10%" "20%" "30%" "40%" "50%" "60%" "70%" "80%" "90%" "100%"))
      (dolist (i *test*) 
        (dolist (x i)
         (progn 
          (setf ke (first x) n (second x) mk (third x) my (fourth x) qz (fifth x) mz (sixth x) qy (seventh x))
          (format str "~{~a~}~%" (list ke ";" n ";" mk ";" my ";" qz ";" mz ";" qy ";"))
		  (if (= i* count)
			(progn (print (pop pr))
				(setf i* 1))
				(incf i*))))))))

(defun rsu-to-csv-1 (out-file)  ;пример вызова (rsu-to-csv-1  "c:/lisp/rsu.csv")
  ;;для КЕ с номерами сечений
  ;(declare (optimize (speed 3) (safety 0)))
  ;(declare (string ke n mk my qz mz qy))
  (let (ke n mk my qz mz qy l (i* 0) count pr section-number)    
    (m-list**)  ;payload другой
    (with-open-file (str out-file :direction :output :if-exists :supersede :external-format :latin-1)
	(setf l (* (list-length (first *test*)) *number-of-process*)
		count (round (/ l 10))
		pr (list "10%" "20%" "30%" "40%" "50%" "60%" "70%" "80%" "90%" "100%"))
      (dolist (i *test*) 
        (dolist (x i)
         (progn 
          (setf ke (first x) section-number  (second x) n (third x) mk (fourth x) my (fifth x) qz (sixth x) mz (seventh x) qy (eighth x))
		  ;можно вставить если section-number = то, то это иначе не выводить format
          (format str "~{~a~}~%" (list ke ";" section-number ";" n ";" mk ";" my ";" qz ";" mz ";" qy ";"))
		  (if (= i* count)
			(progn (print (pop pr))
				(setf i* 1))
			(incf i*))))))))

(defun rsu-to-csv-1* (out-file number)  ;пример вызова (rsu-to-csv-1*  "c:/lisp/rsu.csv" "2")
  ;; (analiz:rsu-to-csv-1*  "c:/lisp/rsu.csv" "2")
  ;;выводит только указанные номера сечений
  ;(declare (optimize (speed 3) (safety 0)))
  ;(declare (string ke n mk my qz mz qy))
  (let (ke n mk my qz mz qy l (i* 0) count pr section-number)    
    (m-list**)  ;payload другой
    (with-open-file (str out-file :direction :output :if-exists :supersede :external-format :latin-1)
      (setf l (* (list-length (first *test*)) *number-of-process*)
	    count (round (/ l 10))
	    pr (list "10%" "20%" "30%" "40%" "50%" "60%" "70%" "80%" "90%" "100%"))
      (dolist (i *test*) 
        (dolist (x i)
         (progn 
	   (setf ke (first x) section-number  (second x) n (third x) mk (fourth x) my (fifth x) qz (sixth x) mz (seventh x) qy (eighth x))
					;можно вставить если section-number = то, то это иначе не выводить format
	   (if (equal section-number number)
	       (format str "~{~a~}~%" (list ke ";"  n ";" mk ";" my ";" qz ";" mz ";" qy ";")))
	   (if (= i* count)
	       (progn (print (pop pr))
		      (setf i* 1))
	       (incf i*))))))))

(defun rsu-to-csv-min-max (out-file)  ;пример вызова (rsu-to-csv-min-max  "c:/lisp/rsu.csv")
	(declare (optimize (speed 3) (safety 0)))
  (let (n1 n2 mk1 mk2 my1 my2 qz1 qz2 mz1 mz2 qy1 qy2
	   n mk my qz mz qy ke ke-v l count pr (i* 0))  ; для rsu-to-csv-min-max-1 добавляем section-number
    ;(declare (string ke ke-v) (short-ﬂoat n1 n2 mk1 mk2 my1 my2 qz1 qz2 mz1 mz2 qy1 qy2))
	(print "начал работу планировщик заданий (m-list*)")
    (m-list*) ; для rsu-to-csv-min-max-1 меняем планировщик на (m-list**)
	(print "закончил работу планировщик заданий")
	(setn-list '( n mk my qz mz qy) nil)
    (with-open-file (str out-file :direction :output :if-exists :supersede)
      (setf l (* (list-length (first *test*)) *number-of-process*)
		count (round (/ l 10))
		pr (list "10%" "20%" "30%" "40%" "50%" "60%" "70%" "80%" "90%" "100%"))
      (dolist (i  *test*)
         (dolist (x i)
           (progn 
              (setf ke (first x))
              (if (equal ke-v ke) ;а section-number равно нашему
                  (progn 
                     (setf
                           n (push  (second x) n)
                           mk (push (third x) mk)
                           my (push (fourth x) my) 
                           qz (push (fifth x) qz)
                           mz (push (sixth x) mz)
                           qy (push  (seventh x) qy)
                  ))
                (progn 
                  (multiple-value-setq (n1 n2) (min-max-ex n))
                  (multiple-value-setq (mk1 mk2) (min-max-ex mk))
                   (multiple-value-setq (my1 my2) (min-max-ex my))
                   (multiple-value-setq (qz1 qz2) (min-max-ex qz))
                   (multiple-value-setq (mz1 mz2) (min-max-ex mz))
                    (multiple-value-setq (qy1 qy2) (min-max-ex qy))
                    (format str "~{~a~}~%" (list ke-v ";" n1 ";" mk1 ";" my1 ";" qz1 ";" mz1 ";" qy1 ";")) ;здесь  для rsu-to-csv-min-max-1 добавляем section-number соответственно
                    (format str "~{~a~}~%" (list ke-v ";" n2 ";" mk2 ";" my2 ";" qz2 ";" mz2 ";" qy2 ";"))
                    (setf ke-v ke)
                    (setn-list '( n mk my qz mz qy) nil))) ;и здесь добавляем section-number соответственно
              (if (= i* count)
			(progn (print (pop pr))
				(setf i* 1))
				(incf i*))
                ))))))
;отличающиеся части можно вынести в отдельные макросы и собрать функции через более общие макросы , оттестить соответственно

(defun rsu-to-csv-min-max-1 (out-file number)  ;пример вызова (rsu-to-csv-min-max-1  "c:/lisp/rsu.csv" "1")
  ;;не работает, должно работать в однопоточной версии
  ;;(например в планировщике заданий указать (setf *number-of-prosess* 0.5) (модифицировать его в другой версии)
	(declare (optimize (speed 3) (safety 0)))
  (let (n1 n2 mk1 mk2 my1 my2 qz1 qz2 mz1 mz2 qy1 qy2
	   n mk my qz mz qy ke ke-v l count pr (i* 0) section-number)  ; для rsu-to-csv-min-max-1 добавляем section-number
    ;(declare (string ke ke-v) (short-ﬂoat n1 n2 mk1 mk2 my1 my2 qz1 qz2 mz1 mz2 qy1 qy2))
	(print "начал работу планировщик заданий (m-list**)")
    (m-list**) ; для rsu-to-csv-min-max-1 меняем планировщик на (m-list**)
	(print "закончил работу планировщик заданий")
	(setn-list '( n mk my qz mz qy section-number) nil)
    (with-open-file (str out-file :direction :output :if-exists :supersede)
      (setf l (* (list-length (first *test*)) *number-of-process*)
		count (round (/ l 10))
		pr (list "10%" "20%" "30%" "40%" "50%" "60%" "70%" "80%" "90%" "100%"))
      (dolist (i  *test*)
         (dolist (x i)
           (progn 
              (setf ke (first x) section-number (second x))
              (if (and (equal ke-v ke)(equal section-number number)) ;а section-number равно нашему
                  (progn 
		    (setf
		     n (push  (third x) n)
		     mk (push (fourth x)  mk)
		     my (push (fifth x) my) 
		     qz (push (sixth x) qz)
		     mz (push (seventh x) mz)
		     qy (push  (eighth x) qy)
		     )) 
		  (progn 
		    (multiple-value-setq (n1 n2) (min-max-ex n))
		    (multiple-value-setq (mk1 mk2) (min-max-ex mk))
		    (multiple-value-setq (my1 my2) (min-max-ex my))
		    (multiple-value-setq (qz1 qz2) (min-max-ex qz))
		    (multiple-value-setq (mz1 mz2) (min-max-ex mz))
                    (multiple-value-setq (qy1 qy2) (min-max-ex qy))
                    (format str "~{~a~}~%" (list ke-v ";" n1 ";" mk1 ";" my1 ";" qz1 ";" mz1 ";" qy1 ";")) ;здесь  для rsu-to-csv-min-max-1 добавляем section-number соответственно
                    (format str "~{~a~}~%" (list ke-v ";" n2 ";" mk2 ";" my2 ";" qz2 ";" mz2 ";" qy2 ";"))
                    (setf ke-v ke)
                    (setn-list '( n mk my qz mz qy section-number) nil))) ;и здесь добавляем section-number соответственно
              (if (= i* count)
			(progn (print (pop pr))
				(setf i* 1))
				(incf i*))))))))
