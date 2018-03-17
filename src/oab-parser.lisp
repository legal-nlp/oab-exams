
(ql:quickload :alexandria)
(ql:quickload :cl-ppcre)
(ql:quickload :cxml)
(ql:quickload :yason)

(defpackage :oab-parser
  (:use :cl :alexandria))

(in-package :oab-parser)

(defclass question ()
  ((filename :accessor question-filename :initarg :filename)
   (number   :accessor question-number)
   (valid    :accessor question-valid)
   (enum     :accessor question-enum)
   (areas    :accessor question-areas    :initform nil)
   (options  :accessor question-options  :initform nil)))

(defclass option ()
  ((letter  :accessor option-letter  :initarg :letter)
   (correct :accessor option-correct :initarg :correct)
   (text    :accessor option-text    :initarg :text :initform nil)))


(defun join-lines (lines)
  (string-trim '(#\Space) (format nil "~{~a~^ ~}" lines)))

(defun enum-pp (lines &optional (paragraph nil) (paragraphs nil))
  (cond ((null lines)
	 (format nil "~{~a~^~%~% ~}" (reverse paragraphs)))
	((equal "" (car lines))
	 (enum-pp (cdr lines)
		  nil
		  (if paragraph
		      (cons (join-lines (reverse paragraph)) paragraphs)
		      paragraphs)))
	(t
	 (enum-pp (cdr lines)
		  (cons (car lines) paragraph)
		  paragraphs))))


(defun naive-parser (inputs filename)
  (macrolet ((flush-line ()
	       `(setq line (let ((l (car lines)))
			     (if l (string-trim '(#\Space) l) l))
		      lines (cdr lines))))
    (prog (line (lines inputs) enum (item nil) (items nil)
	   (q (make-instance 'question :filename filename)))
       
     label-1
     (flush-line)
     (multiple-value-bind (a b)
	 (cl-ppcre:scan-to-strings "^ENUM[ ]*(NULL)?[ ]*Questão ([0-9]+)" line)
       (cond 
	 (a (setf (question-number q) (aref b 1)
		  (question-valid q)  (if (equal "NULL" (aref b 0)) nil t))
	    (go label-2))
	 ((null a) (error "invalid question"))))

     label-2
     (flush-line)
     (multiple-value-bind (a b)
	 (cl-ppcre:scan-to-strings "^AREA[ ]*([^ ]+)?" line)
       (cond
	 ((equal "" line)
	  (go label-2))
	 (a (push (aref b 0) (question-areas q))
	    (go label-2))
	 ((null a)
	  (push line enum)
	  (go label-3))))

     label-3
     (flush-line)
     (multiple-value-bind (a b)
	 (cl-ppcre:scan "^OPTIONS" line)
       (declare (ignore b))
       (cond
	 ((or (null a) (equal "" line))
	  (push line enum)
	  (go label-3))
	 (a (setf (question-enum q) (enum-pp (reverse enum)))
	    (go label-4))))

     label-4
     (flush-line)
     (multiple-value-bind (a b)
	 (cl-ppcre:scan-to-strings "^([A-D])(:CORRECT)?\\)(.*)$" line)
       (cond
	 ((null line)
	  (if item
	      (progn (setf (option-text item)
			   (join-lines (reverse (option-text item))))
		     (push item items)))
	  (go label-5))
	 (a (setf item (make-instance 'option
				      :letter  (aref b 0)
				      :correct (aref b 1)))
	    (push (aref b 2) (option-text item))
	    (go label-4))
	 ((and (null a) (not (equal "" line)))
	  (push line (option-text item))
	  (go label-4))
	 ((equal "" line)
	  (if item
	      (progn (setf (option-text item)
			   (join-lines (reverse (option-text item))))
		     (push item items)))
	  (setf item nil)
	  (go label-4))))

     label-5
     (setf (question-options q) (reverse items))
     (assert (or (member ":CORRECT" (mapcar #'option-correct items) :test #'equal)
		 (not (question-valid q))))
     (return q))))


(defun parse-oab-file (filename &key (parser #'naive-parser))
  (let ((questions (cdr (cl-ppcre:split "---[ \\t]*\\n" (read-file-into-string filename)))))
    (mapcar (lambda (txt)
	      (funcall parser
		       (cl-ppcre:split "\\n" txt)
		       (pathname-name filename)))
	    questions)))


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
