
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
           #:parse-oab-file))

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
	      (list "item" a "correct?" b "text" txt)))


(defun combinator-parser (question)
  (let ((data (parse-string* (named-seq? "ENUM Questão "
					 (<- a (word?))
					 (between? #\Newline 1 2)
					 (<- enum (block?))
					 (sep-options?)
					 (<- ops (between?
                                                  (option?) 1 4))
					 (list :num a
                                               :enum enum
                                               :options ops))
			     question :complete nil)))
    (list (getf data :num)
	  (format nil "~{~{~{~a~}~%~}~%~}" (getf data :enum))
	  (mapcar (lambda (op)
		    (list (car op) (cadr op)
                          (format nil "~{~{~a~}~%~}" (caddr op))))
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


