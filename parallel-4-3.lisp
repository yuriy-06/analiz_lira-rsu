;parallel-------------------------------------------------------------------------------------------------
(in-package #:analiz)
(defvar *number-of-process* 7)
;(if (win-nt)  (defvar *rsu-file* "c:/lisp/rsu.txt"))
(defvar *rsu-file* "c:/lisp/rsu.txt")
;(if (unix)   (defvar *rsu-file* "/home/yura/lisp/rsu.txt"))
(defmacro rsu-list-txt () ;  (rsu-list-txt)
  `(nth-value 0 (rsu-txt *rsu-file*)))
;(defvar *rsu-list* (list (rsu-list-txt)))
(defvar *rsu-list* nil)
(defvar *test* (list ))
(defvar *test2* nil)
(defvar *m-list* nil)
(defvar *thread-stek-list* nil)
;------------------------------


(defun join-thread-list (list)
  (dolist (x list) (join-thread x)))

(defmacro payload (pay)
  `(progn
     (push m* *rsu-list*)
     (push  (make-thread ,pay) *thread-stek-list*)))

(defmacro condition1 ()
  `(and (> i qvant) 
	(not (equal (nth-value 1 
			       (scan-to-strings  "^[|]{1}\\s*(\\d+)\\s+\\d+\\s+\\d+\\s+\\d+\\s+[A-D]+" x)) nil)))
  ;;условие является общим
  ;;для разделения как по номерам конечных элементов, так и по номерам сечений
  )
(defmacro condition2 ()  `(> i qvant))

(defmacro dolist* (massive condition pload)
  `(dolist (x ,massive) 
        (progn 
          (if ,condition
              (progn (payload  ,pload)
                (setf i 1 ; обнуляем счетчик
                      m* nil)))
          (collector-l m* x)
          (incf i))))

(defun m-list* ()  ;   (m-list*)
  (declare (optimize (debug 3)))
  (let* ((m (rsu-list-txt)) (row-number (list-length m))   ;m - список, который надо делить на потоки
          (qvant (round (/ row-number *number-of-process*))))  ;округленное число строк для загрузки потока
    (let ((i 1) m*)
      (setf *test* nil *rsu-list* nil) ;обнуляем переменные результатов
      (dolist* m (condition1) #'rsu-list)
      (payload #'rsu-list))      ;  здесь получается на один процесс больше
    (join-thread-list *thread-stek-list*)))

(defun m-list** ()  ;   (m-list**) ;payload другой, учитываются номера сечений стержней
  (let* ((m (rsu-list-txt)) (row-number (list-length m))   ;m - список, который надо делить на потоки
          (qvant (round (/ row-number *number-of-process*))))  ;округленное число строк для загрузки потока
    (let ((i 1) m*)
      (setf *test* nil *rsu-list* nil) ;обнуляем переменные результатов
      (dolist* m (condition1) #'rsu-list-1)
      (payload #'rsu-list-1))      ;  здесь получается на один процесс больше
    (join-thread-list *thread-stek-list*)))

(defun m-list-rsu* ()  ;   (m-list-rsu*)
  (let* ((row-number (list-length *list-rsu-csv*))   ;вместо списка m глобадьная переменная *list-rsu-csv*
  ;она вычисляется всего один раз , чтобы не гонять по циклу analiz-ini
        (qvant (round (/ row-number *number-of-process*))))  ;округленное число строк для загрузки потока
    (let ((i 1) m*)
      (setf *test* nil *rsu-list* nil) ;обнуляем переменные результатов
      (dolist* *list-rsu-csv* (condition2) #'min-max-rsu-for-ke*)
      (payload #'min-max-rsu-for-ke*))      ;  здесь получается на один процесс больше
    (join-thread-list *thread-stek-list*)))


(defun analiz**()   ; (analiz**)
  (declare (optimize (debug 3)))
  (let (m l1 l2
        n-min n-max mk-min mk-max my-min my-max qz-min qz-max mz-min mz-max qy-min qy-max 
        n mk my qz mz qy xx)
    (m-list* )
    (setf *test2* nil)
    (dolist (x *test*)(push  (make-thread #'analiz) *thread-stek-list*))   ; анализ обрабатывает не все части работы
    (join-thread-list *thread-stek-list*)
	(setf *test* nil)
    (push *test2* *test*)
    (join-thread (make-thread #'analiz))
    (setf m *test2*
          l1 (second m)  ; push меняет порядок
          l2 (first m)
          n-min (second l1) mk-min (third l1) my-min (fourth l1) qz-min (fifth l1) mz-min (sixth l1) qy-min (seventh l1)
          n-max (second l2) mk-max (third l2) my-max (fourth l2) qz-max (fifth l2) mz-max (sixth l2) qy-max (seventh l2))
    (f* "Nmin Nmax     = " n-min n-max)
    (f* "Mk-min Mk-max = " mk-min mk-max)
    (f* "My-min My-max = " my-min my-max)
    (f* "Qz-min Qz-max = " qz-min qz-max)
    (f* "Mz-min Mz-max = " mz-min mz-max)
    (f* "Qy-min Qy-max = " qy-min  qy-max)))

(defun min-max-rsu-for-ke ()
  (let (n-v mk-v my-v qz-v mz-v qy-v k n mk my qz mz qy )
    (dolist (x *test*)
      (progn
        (setf n-v (first x) mk-v (second x) my-v (third  x) qz-v (fourth  x) mz-v (fifth  x) qy-v (sixth x))
        (collector n n-v)
        (collector mk  mk-v)
        (collector my  my-v)
        (collector qz  qz-v)
        (collector mz  mz-v)
        (collector qy  qy-v)))
    (values (list (ocrugl-list-1.00 (min-max-ex (filter-nil n) t))
                  (ocrugl-list-1.00 (min-max-ex (filter-nil mk) t))
                  (ocrugl-list-1.00 (min-max-ex (filter-nil my) t))
                  (ocrugl-list-1.00 (min-max-ex (filter-nil qz) t))
                  (ocrugl-list-1.00 (min-max-ex (filter-nil mz) t))
                  (ocrugl-list-1.00 (min-max-ex (filter-nil qy) t))))))

(defun foo-test-analiz-one ()  ;заготовка для измерения производительности (foo-test-analiz-one)
  
  (labeled-time (analiz**))
  )
