
(defun read-file (filename)
  (coerce (with-open-file (stream filename)
	    (loop for char = (read-char stream nil)
		  while char
		  collect char))
	  'string))


