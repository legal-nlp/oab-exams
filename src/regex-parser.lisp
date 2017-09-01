
(ql:quickload :cl-ppcre)

;;
;; pre-processing
(defun read-file (filename)
  (coerce (with-open-file (stream filename)
	    (loop for char = (read-char stream nil)
		  while char
		  collect char))
	  'string))

(defun split-exam (exam-str)
  (rest (ppcre:split "---" exam-str))) ;; rest because starts with ---

(defun exam-to-questions (exam-path)
  (split-exam (read-file exam-path)))

;; nr-questão, enunciado
"ENUM\\s*Questão\\s*(\\d+)\\s+([\\S|\\s]+?.)\\s*OPTIONS"
;;item, correta?, texto-item
"\\s*([A-E])(\:CORRECT)?\\)\\s+([\\S|\\s]+?)\\n{2,}"

;;
;; parser

(defun get-str (str start end)
  (if (null start)
      nil
      (subseq str start end)))

(defun trim-str (question-str ix)
  (subseq question-str ix))

(defun pair-ix (begin-vector end-vector)
  (let ((begin (coerce begin-vector 'list))
        (end (coerce end-vector 'list)))
    (pairlis begin end)))

(defun get-strings (str ix-list &optional strings)
  (if (endp ix-list)
      strings
      (get-strings str (rest ix-list)
                   (cons (get-str str (caar ix-list) (cdar ix-list))
                         strings))))

(defun aux-parse-enum (question-str)
  (ppcre:scan "ENUM\\s*Questão\\s*(\\d+)\\s+([\\S|\\s]+?.)\\s*OPTIONS"
              question-str))

(defun parse-enum (question-str)
  (multiple-value-bind (* end begin-vec end-vec)
      (aux-parse-enum question-str)
    (values (trim-str question-str end)
            (get-strings question-str (pair-ix begin-vec end-vec)))))

(defun aux-parse-item (question-str)
  (ppcre:scan "\\s*([A-E])(\:CORRECT)?\\)\\s+([\\S|\\s]+?)\\n{2,}"
                         question-str))

(defun parse-item (question-str)
  (when (= (length question-str) 0)
    (return-from parse-item (values nil (list nil nil nil))))
  (multiple-value-bind (* end begin-vec end-vec)
      (aux-parse-item question-str)
    (values (trim-str question-str end)
            (get-strings question-str (pair-ix begin-vec end-vec)))))

(defun parse-items (question-str &optional parsed-items)
  (multiple-value-bind (trimmed-str matches)
      (parse-item question-str)
    (destructuring-bind (i-letter i-correct? i-text) matches
      (if (null trimmed-str)
          (reverse parsed-items)
          (parse-items trimmed-str
                       (cons (list i-letter i-correct? i-text)
                             parsed-items))))))

(defun parse-question (question-str)
  (multiple-value-bind (trimmed-str matches)
      (parse-enum question-str)
    (destructuring-bind (q-nr q-enum) matches
      (cons q-nr (cons q-enum (parse-items trimmed-str))))))

(defun parse-questions (questions &optional parsed-questions)
  (if (endp questions)
      parsed-questions
      (parse-questions (rest questions)
                       (cons (parse-question (first questions))
                             parsed-questions))))

;;
;; xml
;; check xml syntax with xmlint and book

;; <test a="b"><child/></test>
;; (name ((name value)*) child*).



(cxml:with-xml-output (cxml:make-octet-stream-sink stream :indentation 2 :canonical nil)
  (cxml:with-element "exam"
    (cxml:attribute "year" "xxxx")
    (cxml:attribute "number" "xx")
    (cxml:with-element "questions"
      (cxml:with-element "question"     (cxml:attribute "blub" "bla"))
    (cxml:text "Hi there.")))


(defun number-heading (question-nr)
  (concatenate 'string "<question-number>"
               question-nr
               "</question-number>"))

(defun enum-heading (question-enum)
  (concatenate 'string "<question-enum>"
               question-enum
               "</question-enum>"))

(defun items-heading (items)
  )

(defun question-heading (question)
  (concatenate 'string "<question>"
               question-enum
               "</question>"))

(defun question-to-xml (question)
  (destructuring-bind (q-nr q-enum items) question
    (question-heading (number-heading q-nr)
                      (enum-heading q-enum)
                      (items-heading items))))
  

(defun exam-to-xml (exam &optional xml-exam) ;;output of parse-questions
  (if (endp exam)
      xml-exam
      (exam-to-xml (rest exam)
                   (cons (question-to-xml (first exam))
                         xml-exam))))

;; example

(let* ((questions (exam-to-questions #p"/home/bruno/git/oab-exams/OAB/raw/2010-official-1.txt")))
  (parse-questions questions))
