(ql:quickload :cl-ppcre)
(ql:quickload :parse-float)
(defpackage :analiz-new
  (:use :common-lisp :cl-ppcre :parse-float ))
(in-package :analiz-new)
(defun analiz (file-txt)
  (declare (optimize (debug 3)))
  (break "5555455")
  (let (m v)
    (format t  "~a~%" "начали разбор файла регулярными выражениями")
    (with-open-file (str file-txt :direction :input :external-format :latin-1)
      (do
       ((line ;; переменная
	 (read-line str nil 'eof) ;; init-form пока вычисляется в nil -- итерации происходят
	 (read-line str nil 'eof))) ;; step-form -- вычисляется на каждой итерации и присваивается line
       ((eql line 'eof))  ;; end-test-form -- когда вычислится в true выполнится result-form (пропущена) и выведется как результат всего выражения do
	;; statement- тело цикла
	(setf v (nth-value 3 (scan-to-strings  "^\\|{1}(\\s+\\d+){2,4}\\s+[A-D]{1}\\d{1}\s*((-??\d*\.??\d+\s+){6})" line))
	      m (append m (list v)))))

    
    (format t "~a~%" "закончили разбор файла")
    (values m)
    ))
