
(ql:quickload :alexandria)
(ql:quickload :cl-ppcre)
(ql:quickload :cxml)
(ql:quickload :yason)


(defpackage :oab-parser
  (:use :cl :alexandria :cl-ppcre :cxml)
  (:export :oab-to-xml :mirror-oab-to-xml :parse-oab-file))


(in-package :oab-parser)


(defun naive-parser (question)
  (let ((sc1 (cl-ppcre:create-scanner "ENUM (NULL )?Questão ([0-9]+)(.+)"
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
	(list (aref m 1) (aref m 0) (aref m 2) res)))))


(defun parse-oab-file (filename &key (fn-parsing #'naive-parser))
  (let ((questions (cdr
                    (cl-ppcre:split "---\\s"
                                    (read-file-into-string filename)))))
    (mapcar fn-parsing questions)))


;; JSON ES

(defun question-to-json (question)
  "...")


;; XML

(defun item-to-tree (item)
  (destructuring-bind (i-letter i-correct? i-text) item
    (list "item" (list (list "letter" i-letter)
                       (list "correct" (if i-correct? "true" "false")))
          i-text)))


(defun question-to-tree (question)
  (destructuring-bind (q-number q-null? q-enum items) question
    (list "question" (list (list "number" q-number)
                           (list "valid" (if q-null? "false" "true")))
          (list "statement" nil q-enum)
          (append (list "items" nil)
                  (reverse (mapcar #'item-to-tree items))))))


(defun questions-to-tree (questions year edition)
  (list "OAB-exam" (list (list "year" year) (list "edition" edition))
        (append (list "questions" nil)
                (mapcar #'question-to-tree questions))))


(defun tree-to-xml (tree path)
  (with-open-file (out path :direction :output
                       :element-type '(unsigned-byte 8)
                       :if-exists :supersede)
    (cxml-xmls:map-node (cxml:make-octet-stream-sink out)
                        tree :include-namespace-uri nil)))


(defun oab-to-xml (txt-path xml-path &key year edition)
  (let* ((questions (parse-oab-file txt-path))
         (tree (questions-to-tree questions year edition)))
    (tree-to-xml tree xml-path)))

(defun get-year-edition-from-path (filepath &key (sep "-"))
  (let ((name (pathname-name filepath)))
    (cl-ppcre:split sep name)))

(defun mirror-oab-to-xml (txt-path)
  ;; will create a xml version in the same path as txt-path
  ;; filename-name must be YYYY-ed, as in doc/README
  (destructuring-bind (year edition)
      (get-year-edition-from-path txt-path)
    (let ((xml-path (make-pathname :type "xml" :defaults txt-path)))
      (oab-to-xml txt-path xml-path :year year :edition edition))))


; (oab-parser:oab-to-xml #P"../OAB/raw/2010-official-1.txt" #P"2010-01.xml" :year "2010" :edition "01")
; (mapc #'oab-parser:mirror-oab-to-xml (directory #p"/*.txt"))
