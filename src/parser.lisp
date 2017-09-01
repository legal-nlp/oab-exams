
(ql:quickload :parser-combinators)
(ql:quickload :parser-combinators-cl-ppcre)
(ql:quickload :alexandria)
(ql:quickload :cl-ppcre)
(ql:quickload :cxml)

; https://github.com/Ramarren/cl-parser-combinators/blob/master/doc/parser-combinators.org

(defpackage :oab-parser
  (:use :cl
        :parser-combinators
        :parser-combinators-cl-ppcre
        :alexandria)
  (:export #:parse-oab
           #:parse-oab-file
           #:dir-to-xml
           #:exam-to-xml))

(in-package :oab-parser)


(defun line? ()
  (named-seq? (<- a (atleast? (choices (word?) #\Space ":" "ª" "-" "/"  "," ".") 1))
	      #\Newline
	      a))

(defun paragraph? ()
  (many? (line?)))

(defun block? ()
  (sepby? (paragraph?) #\Newline))

(defun sep-options? ()
  (seq-list? (between? #\Newline 1 2) "OPTIONS"
             (between? #\Newline 1 2)))

(defun option? ()
  (named-seq? #\Newline
	      (<- a (choices "A" "B" "C" "D"))
	      (<- b (opt? ":CORRECT"))
	      ")"
	      (<- txt (paragraph?))
	      (list a b txt)))


(defun combinator-parser (question)
  (let ((data (parse-string* (named-seq? "ENUM Questão "
					 (<- a (word?))
					 (between? #\Newline 1 2)
					 (<- enum (block?))
					 (sep-options?)
					 (<- ops (between? (option?) 1 4))
					 (list :num a :enum enum :options ops))
			     question :complete nil)))
    (list (getf data :num)
	  (format nil "~{~{~{~a~}~%~}~%~}" (getf data :enum))
	  (mapcar (lambda (op)
		    (list (car op) (cadr op) (format nil "~{~{~a~}~%~}" (caddr op))))
		  (getf data :options)))))


(defun naive-parser (question)
  (let ((sc1 (cl-ppcre:create-scanner "ENUM Questão ([0-9]+)(.+)"
				      :single-line-mode t))
	(sc2 (cl-ppcre:create-scanner "([A-D])(:CORRECT)?\\)(([^\\n]+\\n)+\\n)"
				      :single-line-mode t))
	(res))
    (destructuring-bind (enum ops)
	(cl-ppcre:split "\\sOPTIONS\\s" question)
      (multiple-value-bind (a m)
	  (cl-ppcre:scan-to-strings sc1 enum)
	(declare (ignore a))
	(cl-ppcre:do-scans (s e rs re sc2 ops)
	  (push (list (subseq ops (aref rs 0) (aref re 0))
		      (if (aref rs 1) (subseq ops (aref rs 1)
                                              (aref re 1)))
		      (subseq ops (aref rs 2) (aref re 2)))
		res))
	(list (aref m 0) (aref m 1) res)))))


(defun parse-oab-file (filename &key (fn-parsing #'naive-parser))
  (let ((questions (cdr
                    (cl-ppcre:split "---\\s"
                                    (read-file-into-string filename)))))
    (mapcar fn-parsing questions)))

;;
;; XML

(defun txt-in-directory (dir-path)
  "return list of all .txt files in a directory"
  (let ((wild-path (merge-pathnames dir-path
                                    (parse-namestring "*.txt"))))
    (directory wild-path)))

(defun true-or-false (boolean)
  (if boolean
      "t"
      "nil"))

(defun item-to-tree (item)
  (destructuring-bind (i-letter i-correct? i-text) item
    (list "item" (list (list "letter" i-letter)
                       (list "correct" (true-or-false i-correct?)))
          i-text)))

(defun question-to-tree (question)
  (destructuring-bind (q-number q-enum items) question
    (list "question" (list (list "number" q-number))
          (list "statement" nil q-enum)
          (append (list "items" nil)
                  (reverse (mapcar #'item-to-tree items))))))

(defun questions-to-tree (questions year)
  (list "OAB-exam" (list (list "year" year) (list "edition" ""))
        (append (list "questions" nil)
                (mapcar #'question-to-tree questions))))

(defun tree-to-xml (tree path)
  (with-open-file (out path :direction :output
                       :element-type '(unsigned-byte 8))
    (cxml-xmls:map-node (cxml:make-octet-stream-sink out)
                        tree :include-namespace-uri nil)))

(defun exam-to-xml (path)
  (let* ((questions (oab-parser:parse-oab-file path))
         (year (subseq (file-namestring path) 0 4))
         (tree (questions-to-tree questions year))
         (xml-path (make-pathname :type "xml" :defaults path)))
    (tree-to-xml tree xml-path)))

(defun dir-to-xml (dir-path)
  (let ((file-paths (txt-in-directory dir-path)))
    (mapcar #'exam-to-xml file-paths))
  nil)
