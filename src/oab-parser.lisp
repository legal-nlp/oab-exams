(ql:quickload :alexandria)
(ql:quickload :cl-ppcre)
(ql:quickload :yason)

(defpackage :oab-parser
  (:use :cl :alexandria))

(in-package :oab-parser)

;; utils

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


;; data

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


;; JSON encoding

(defmethod yason:encode ((q question) &optional (stream *standard-output*))
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:encode-object-element "filename" (question-filename q))
      (yason:encode-object-element "number" (question-number q))
      (yason:encode-object-element "valid"  (question-valid q))
      (yason:encode-object-element "enum"   (question-enum q))
      (yason:with-object-element ("areas")
	(yason:with-array ()
	  (dolist (quest (question-areas q))
	    (yason:encode-array-element quest))))
      (yason:with-object-element ("options")
	(yason:with-array ()
	  (dolist (opt (question-options q))
	    (yason:encode-array-element opt)))))))

(defmethod yason:encode ((opt option) &optional (stream *standard-output*))
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:encode-object-element "letter" (option-letter opt))
      (yason:encode-object-element "correct" (option-correct opt))
      (yason:encode-object-element "text"  (option-text opt)))))


;; parser

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
				      :correct (if (aref b 1) t nil)))
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
     (assert (or (some #'option-correct items)
		 (not (question-valid q))))
     (return q))))


(defun parse-oab-file (filename &key (parser #'naive-parser))
  (let ((questions (cdr (cl-ppcre:split "---[ \\t]*\\n" (read-file-into-string filename)))))
    (mapcar (lambda (txt)
	      (funcall parser
		       (cl-ppcre:split "\\n" txt)
		       (format nil "~a.txt" (pathname-name filename))))
	    questions)))


(defun answer-sheet (questions)
  (mapcar (lambda (q)
	    (list (question-number q)
		  (and (question-valid q)
		       (option-letter (car (member-if #'option-correct
						      (question-options q)))))))
	  questions))



(defun file-to-es (fn stream)
  (let ((questions (parse-oab-file fn))
	(head "{ \"index\" : { \"_index\" : \"oab\", \"_type\" : \"doc\" }}"))
    (loop for q in questions
	  do (progn
	       (write-line head stream)
	       (yason:encode q stream)
	       (format stream "~%")))))

(defun files-es ()
  (dolist (path (directory "*.txt"))
    (with-open-file (ss (format nil "~a.json" (pathname-name path))
			:direction :output :if-exists :supersede)
      (file-to-es path ss))))



;; utils

(defun compr (elt n 1st)
  (if (null 1st)
      (list (n-elts elt n))
      (let ((next (car 1st)))
	(if (eql next elt)
	    (compr elt (+ n 1) (cdr 1st))
	    (cons (n-elts elt n)
		  (compr next 1 (cdr 1st)))))))

(defun n-elts (elt n)
  (if (> n 1)
      (list n elt)
      elt))

(defun compress (x)
  (if (consp x)
      (compr (car x) 1 (cdr x)) x))


(defun classify ()
  (let ((tab (make-hash-table :test #'equal))) 
    (dolist (fn (directory "*.txt") tab) 
      (mapc (lambda (q) 
	      (dolist (a (slot-value q 'areas))
		(push (slot-value q 'number) (gethash a tab)))) 
	    (parse-oab-file fn)))
    (loop for k being each hash-key of tab
	  do (format t "~a: ~a~%" k
		     (compress (sort (mapcar #'parse-integer (gethash k tab)) #'<))))))


(defun questions-by-area (area)
  (let ((questions nil))
    (dolist (fn (directory "/Users/ar/work/oab-exams/official/raw/*.txt") questions)
      (dolist (q (parse-oab-file fn)) (push q questions)))
    (mapcar (lambda (q) (list (slot-value q 'filename) (slot-value q 'number)))
	    (remove-if-not (lambda (q) (member area (slot-value q 'areas) :test #'equal))
			   questions))))

