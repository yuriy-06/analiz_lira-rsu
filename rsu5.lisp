(in-package #:analiz)

;(defmacro rsu-list-csv () ;  (rsu-list-csv)
;  `(rsu-split (file-m-2 *rsu-file-csv*)))
(defvar *list-rsu-csv* nil)
(defvar *list-v-ke* nil)
(defvar *v-ke*)

(defun rsu-split (list-rsu-csv);(rsu-split (rsu-csv "c:/lisp/rsu.csv"))
  (let (m)
    (dolist (x list-rsu-csv) (setq m (append m (list  (split ";"  x)))))
    (values m)))

(defun rsu-split-parallel (list-rsu-csv);(rsu-split-parralel (rsu-csv "c:/lisp/rsu.csv"))
  (let ((m nil) (m2 nil))
    (parallel-dolist (x list-rsu-csv :number-of-threads 4 :vars ((:list m))) (setq m (append m (list  (split ";"  x)))))
    (dolist (x m) (setf m2 (append m2 x)))
    (values m2)))

(defun usil-ocrugl (list-usil)  ; (list-usil (list n mk my qz my qy))
  (let (m)
    (dolist (x list-usil) 
      (collector-l m (min-max-ex x t))) 
  (values  m)))

(defun min-max-rsu-for-ke* (); (min-max-rsu-for-ke (rsu-split-parralel (file-m-2 "c:/lisp/rsu.csv")) "325")
; 3,5 сек очень долго, можно один раз сделать сплит и не париться
(declare (optimize (speed 3) (safety 0)))
  (declare (string n-v mk-v my-v qz-v mz-v qy-v))
  (let (n-v mk-v my-v qz-v mz-v qy-v n mk my qz mz qy)
    (dolist (v-ke *list-v-ke*)
      (dolist (x (pop *rsu-list*)) 
        (progn
          (if (equal (first  x) v-ke)     
              (progn 
                (setf n-v (second x) mk-v (third x) my-v (fourth x) qz-v (fifth x) mz-v (sixth x) qy-v (seventh x))
                (usil*)
                )))))
    (push (usil-ocrugl (list n mk my qz mz qy)) *test*)))
