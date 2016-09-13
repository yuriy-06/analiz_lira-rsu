(in-package #:analiz)
;идея этого метода, чтобы сгенерировать новый ke.csv с элементами, замененными на kb
;и дописать в конец существующего rsu.csv kb с РСУ
; не забываем делать ПЕРЕНУМЕРАЦИЮ !!!!!

(defun base-func (v-ke base) ;(base-func v-ke base)
  (dolist (x base) (if (equal x v-ke)(return-from base-func 1))));возвращает 1 если КЭ есть в базе

(defun analiz-kb (path-kb) ;  (analiz-kb "c:/lisp/kb.txt");возвращает список констр-ых элементов вместе со списком КЕ включенным в каждый из Констр. Эл. --- по 2 раза возвращает номера КЕ 
  (let* ((m2) (m (kb path-kb)) (kb-base) (k-base) (massive) (kb-v) (kb-v-p))
    (dolist (x m) (progn 
                    (setf s (regex-replace-all "\\s+" x "")
                          s (split "\\|" s)
                          k (second s) kb-v (fourth s))
                    (if (not (equal 1  (base-func kb-v kb-base)))
                        (setf massive (append massive (list (list kb-v-p k-base))) k-base nil));если kb-v есть в базе base-func вернет 1
                    (setf kb-v-p kb-v
                          k-base (append k-base (list k))
                          kb-base (append kb-base (list kb-v)))))
    (setf massive (append massive (list (list kb-v-p k-base))) k-base nil) ;\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
    (setf massive (rest massive);здесь уже список всех КЭ входящих в КБ
          i-number 0)
    (dolist (x massive) ;;выдает список ((КБ1 КЭ-1 КЭ-N)...(list-2)(list-3)... (list-n))
      (progn 
        (setf n1 (first x) n2 (first (first (rest x))) n3 (first (last (first (rest x))))
              m2 (append m2 (list (list n1 n2 n3))))
        (incf i-number)))
    (dolist (x massive) (setf (second x) (base-dubl (second x))))
    (values massive m2 i-number)))

(defun ke-nodes (n-ke list-ke-m) ; (ke-nodes "9575" (ke "c:/lisp/ke.csv")) возвращает список из номера КЕ и двух его узлов
  (let ((s) (v-ke) (v-node1) (v-node2))
    (dolist (x list-ke-m)(progn 
                           (setf s (split ";" (filter x))
                                 v-ke (first s)
                                 v-node1 (second s)
                                 v-node2 (third s))
                           (if (equal v-ke n-ke)(return-from ke-nodes (list v-ke v-node1 v-node2)))))
    (print (concatenate 'string "функция ke-nodes -- нет такого KE " n-ke))
    (values nil))) ;на вход след. функции идет CDR, нельзя выводить строку, только список или nil

(defun kb-nodes (kb-list list-ke) ; (kb-nodes (analiz-kb "c:/lisp/kb.txt") "c:/lisp/ke.csv") 
;здесь должен возвращаться список ke-list, без КЭ , участвующих в КБ
  (let* ((m) (list-ke-m list-ke) (list-ke))
    (dolist (x kb-list) ( progn
                          (setf kb (first x)
                                list-ke (second x)
                                first-ke (first list-ke)  ;первый и последний КЕ нужны для выбора координат начала и конца стержня
                                last-ke (first (last list-ke))
                                first-nodes (second (ke-nodes first-ke list-ke-m))
                                last-nodes (third (ke-nodes last-ke list-ke-m))
                                stroka-kb (list kb first-nodes last-nodes)
                                m (append m (list stroka-kb))
                                )))
    (values m)))

(defun kb-rsu-files (rsu-csv-file kb-file) ; (kb-rsu-files "c:/lisp/rsu.csv" "c:/lisp/kb.txt")
  (print "импортируем РСУ -- (setf list-rsu (rsu-split (file-m rsu-csv-file)))")
  (setf list-rsu (rsu-split (file-m rsu-csv-file)))
  (multiple-value-setq (kb-list not-need-value kb-number) (analiz-kb kb-file))
  (print (concatenate 'string "KB number " (write-to-string kb-number)))
  (print "0%")
  (with-open-file (f rsu-csv-file  :direction :output :if-exists :append )  ;открывается на добавление файл РСУ
    (setf kb-number-10 (floor (/ kb-number 10)) ;десятая часть итераций
          procent 10
          n-iter 0) ;счетчик итераций

    (dolist (x kb-list) (progn
                          (incf n-iter) ;счетчик итераций увеличивается
                          (if (equal (nth-value 1 (floor (/ n-iter kb-number-10))) 0)
                              (progn 
                                (print (concatenate 'string (write-to-string procent) "%"))
                                (setq procent (+ procent 10))))
                          (setf a (min-max-rsu-for-ke-list list-rsu (base-dubl (second x))))
                          (push (first x) a)
                          (setf st1 (list (first a) (write-to-string (first (second a)))  (write-to-string (first (third a))) 
                                          (write-to-string (first (fourth a)))  (write-to-string (first (fifth a)))  
                                          (write-to-string (first (sixth a))) (write-to-string (first (seventh a))))
                                st2 (list (first a) (write-to-string (second (second a))) (write-to-string (second (third a))) 
                                          (write-to-string (second (fourth a))) (write-to-string (second (fifth a)))
                                          (write-to-string (second (sixth a))) (write-to-string (second (seventh a)))))
                          
                          (setf st1 (concatenate 'string (first st1) ";" (second st1) ";" (third st1) ";" 
                                                 (fourth st1) ";" (fifth st1) ";" (sixth st1) ";" (seventh st1) ";"))
                          (setf st2 (concatenate 'string (first st2) ";" (second st2) ";" (third st2) ";" 
                                                 (fourth st2) ";" (fifth st2) ";" (sixth st2) ";" (seventh st2) ";"))
                          (write-line st1 f)
                          (write-line st2 f)))
    ))

(defun base-dubl (base) ;(base-dubl (base-ke (analiz-kb "c:/lisp/kb.txt")))   ;убирает из единой базы (списка) КЕ дубли
  (let ((base-m (list )))
    (dolist (x base) (progn 
                       (if (equal (base-func x base-m) 1) 
                           () 
                         (setf base-m (append base-m (list x))))))
    (values base-m)))

(defun base-ke (kb-list) ;(base-ke (analiz-kb "c:/lisp/kb.txt"))  ; создает единую базу КЕ (список)
  (let ((m (list )))
    (dolist (x kb-list) (setf m (append m (second x) )))
    (values (base-dubl m))))

(defun ke-kb-file (kb-file ke-file) ; (ke-kb-file "c:/lisp/kb.txt" "c:/lisp/ke.csv")
  (setf ke-list (ke ke-file)
        kb-base (base-ke kb-list)
        note-kb-nodes (kb-nodes kb-list ke-list))
  (multiple-value-setq (not-need-v not-need-value kb-number) (analiz-kb kb-file))
  (setf m (list ));обнуление общей переменной m
  ;генерация нового КЕ файла
  (dolist (x note-kb-nodes) (setf a (concatenate 'string (first x) ";" (second x) "," (third x) ";") ;генерация КБ элементов
                                  m (append m (list a))))
  (setf note-kb-nodes m
        m (list ))
  (dolist (x ke-list) (progn 
                        (setf s (split ";" (filter x)) ;режем ke-list для проверки, есть ли такие элементы в КБ
                              n-ke (first s))
                        (if (equal (base-func n-ke kb-base) 1) () (setf m (append m (list x))))))
  (setf m (append m note-kb-nodes)) ;затем построчно выводим массив в файл
  (with-open-file (f ke-file  :direction :output :if-exists :supersede )
    (dolist (x m) (write-line x f))))

(defun kb-main (rsu-csv-file kb-file ke-file) 
; (kb-main "c:/lisp/1/rsu.csv" "c:/lisp/1/kb.txt" "c:/lisp/1/ke.csv")  
  (print "kb-rsu-files - внедрение КБ в файл РСУ")
  (kb-rsu-files rsu-csv-file kb-file)
  (print "ke-kb-file - внедрение КБ в файл КЭ")
  (ke-kb-file kb-file ke-file))
