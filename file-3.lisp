(in-package #:analiz)

(defun file-m (file) ;более быстрый вариант без регулярных выражений, без проверки входного потока
  (let (m (file*
	     (open file :if-does-not-exist nil :external-format :latin-1)))
    (if (equal file* nil) (format t "~a~a~%" "нет такого файла  " file))
    (loop for line = (read-line file* nil)
          while line do 
          ( setf m (append m (list line))))
    (close file*)
    (values m)))
;experimental+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!!!!!!!!!!!!!!!!

(defun file-m-2 (file) ;более быстрый вариант без регулярных выражений, без проверки входного потока
  (let (m)
  (format t "~%~a~%" "начали разбор файла csv")
    (with-open-file (str file :direction :input :if-does-not-exist nil :external-format :latin-1)
      (do 
          ((line (read-line str nil 'eof) (read-line str nil 'eof)))
          ((eql line 'eof))
        (setf m (append m (list line)))))
   ; (setf m (filter-str-list m))    ;; костыль -- нельзя без регулярных выражений совсем, более безопасный   ; но с ними возникают другие ошибки
	(format t "~a~%" "закончили разбор файла csv")
    (values m)))

(defmacro defun-file* (func-name  regex 'val) ;макрос генерирует функции обрабатывающие текстовые файлы (они идут как аргумент функции)
  `(defun ,func-name (file)       ;и возвращает массив строк , удовлетворяющих регулярному выражению
     (let (m v (schet 0) file*)
       ;;(declare (optimize (debug 3)))
       ;;(break)
       (setf file* (open file :if-does-not-exist nil :external-format :latin-1 ))
       (if (equal file* nil) (progn 
			       (format t "нет такого файла ~S" file) 
			       (return-from ,func-name)))
	(format t  "~%~a" "начался процесс импорта файла txt , функция --- ")
	(format t  "~a~%" (symbol-name ',func-name))
     (loop for line = (read-line file* nil)
           while line do  
           (progn (setf v (nth-value 0 (scan-to-strings ,regex (filter line))))
		  (if (equal v nil)() ( setf m (append m (list ,val)) ))
             (incf schet)))
     (close file*)
	 (format t  "~a~%" "закончился процесс импорта файла")
     (values m schet))))

(defmacro defun-file (func-name regex)
	`(defun-file* ,func-name ,regex 'v))

(defun-file nodes "\\d+;(-??\\d+\.{1}\\d+;){3}");(nodes "file")
(defun-file ke "(\\d+;){3}");(ke "file")
(defun-file rsu-csv "(\\d*;+)(-??\\d*\.??\\d+;){6}");(rsu-csv "file")(rsu-csv "c:/lisp/rsu.csv")
(defun-file kb "^.+\\d+.+\\d+.+КБ\\d+") ;(kb "c:/lisp/kb.txt")
(defun-file* rsu-txt "^|(\\s*\\d+){2}" 'line)  ;остальное через фильтр

(defmacro nodes-float-list (func-name n2)
  `(defun ,func-name (file-name-path)
    (declare (optimize (debug 3)))
    (let (nod m line)
    
      (setf nod (nodes file-name-path))
      (dolist (x nod)
	(setf
	 line  (do ((v 1 (+ v 1)) m) ((= v ,n2) m) (setf m (append m  (list (parse-float (nth v (split ";" x)) :type 'double-float)))))  ;; из елемента списка строк
	 ;;мы делаем сплитом список чисел и из них получаем срез из 2-го и 4-го элементов (координаты)
	 m (append m (list line))  ;;получаем список координат
	 ))
      (values m)
      )))
(nodes-float-list nodes-xyz-float 4)
(nodes-float-list nodes-xy-float 3)

