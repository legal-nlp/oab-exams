
(ql:quickload :parser-combinators)
(ql:quickload :alexandria)
(ql:quickload :cl-ppcre)

; https://github.com/Ramarren/cl-parser-combinators/blob/master/doc/parser-combinators.org

(defpackage :oab-parser
  (:use :cl :parser-combinators :alexandria)
  (:export #:parse-oab #:parse-oab-file))

(in-package :oab-parser)


(defun line? ()
  (named-seq? (<- a (atleast? (choices (word?) #\Space "ª" "-" "/"  "," ".") 1))
	      #\Newline
	      a))

(defun paragraph? ()
  (many? (line?)))

(defun block? ()
  (sepby? (paragraph?) #\Newline))

(defun sep-options? ()
  (seq-list? (between? #\Newline 1 2) "OPTIONS" (between? #\Newline 1 2)))

(defun option? ()
  (named-seq? #\Newline
	      (<- a (choices "A" "B" "C" "D"))
	      (<- b (opt? ":CORRECT"))
	      ")"
	      (<- txt (paragraph?))
	      (list a b txt)))


(defun parse-question (question)
  (let ((data (parse-string* (named-seq? "ENUM Questão "
					 (<- a (word?))
					 (between? #\Newline 1 2)
					 (<- enum (block?))
					 (sep-options?)
					 (<- ops (between? (option?) 1 4))
					 (list :num a :enum enum :options ops))
			     question)))
    (list (getf data :num)
	  (format nil "~{~{~{~a~}~%~}~%~}" (getf data :enum))
	  (mapcar (lambda (op)
		    (list (car op) (cadr op) (format nil "~{~{~a~}~%~}" (caddr op))))
		  (getf data :options)))))


(defun parse-oab-file (filename num)
  (let ((questions (cdr (cl-ppcre:split "---\\s" (read-file-into-string filename)))))
    (mapcar #'parse-question (subseq questions 0 num))))


