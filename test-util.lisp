(in-package #:analiz)

(defmacro labeled-time (form)
  `(progn 
     (format *trace-output* "~2&~a" ',form)
     (time, form)))


(defun foo-test ()  ;заготовка для измерения производительности
  ;(setn-list-0 '(*n* *mk* *my* *qz* *mz* *qy*))
  ;(setq s (rsu-split (rsu-csv "c:/lisp/rsu.csv")))
  ;(labeled-time (min-max-rsu-for-ke s "76"))
  ;(labeled-time (geom (nodes "c:/lisp/node.csv")))
  ;(labeled-time (rsu-csv "c:/lisp/rsu.csv"))
  ;(labeled-time (rsu-split (rsu-csv "c:/lisp/analiz-poz/1/rsu.csv")))
  ;(labeled-time (file-m "c:/lisp/analiz-poz/1/rsu.csv"))
  ;(labeled-time (file-m "c:/lisp/analiz-poz/1/rsu_result.csv"))
  ;(labeled-time (file-m-2 "c:/lisp/analiz-poz/1/rsu_result.csv"))
  ;(labeled-time (min-max-rsu-for-ke-parallel (rsu-split (rsu-csv "c:/lisp/analiz-poz/1/rsu_result.csv")) "261"))
  ;(labeled-time (min-max-rsu-for-ke (rsu-split (rsu-csv "c:/lisp/analiz-poz/1/rsu_result.csv")) "261"))
  ;(labeled-time (main-ris :list-x (list 0 7.7 15.4) :list-y nil :list-z nil))
  ;(labeled-time (main-ris :list-x (list 0 6 12) :list-y (list 0 6 12) :list-z (list 6.6 11.4 15.1 19.9 23.5 30.8 33.6 39.2 42.1)))
	(labeled-time (rsu-list**))
  )
  
 (defmacro test-unit-0 ()
	`())