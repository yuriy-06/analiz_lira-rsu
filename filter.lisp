(in-package #:analiz)

(defun filter (line)  ;функция заменяет символы из нижеследующего массива на разрешенный ";"
  (let ((list* (list "\\t+" "\\s+" "\\n+" ":" "," )))
    (dolist (x list*) 
      (setf line (regex-replace-all x line ";")))
    (setf line (regex-replace-all "-0.000" line "0.000")) ;заменяет глюк лиры "-0.000" на "0.000"
    (values line)))

(defun filter-rsu-txt (line)  ;функция не дописал фильтрует входящий поток
  (let ((list* (list "[а-я]+" "[А-Я]+" "---" "\\s{60}" "#" "\\")))
    (dolist (x list*) 
      (if (equal (nth-value 0 (scan-to-strings x line)) nil) 
          ()
        (return-from filter-rsu-txt nil)))
    (values line)))
; то же самое но для списка
;(mapcar #'filter-rsu-txt (nth-value 0 (rsu-txt2 "c:/lisp/rsu.txt")))

(defun filter-str-list (list*)
  (let (v m)
    (dolist (x list*) 
      (setf v (filter x)
            m (append m (list v))))
    (values m)))
