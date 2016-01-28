; Cavriana Massimo
;	Ultima modifica: 07/29/2014
;

(defun string-to-list (string)
  (loop for i from 0 to (- (length string) 1) collecting (elt string i)))

(defun list-to-string (list)
  (if (atom list)
      NIL
      (coerce list 'string)))

(defun parse-uri (uri)
  (parse-scheme (string-to-list uri) NIL
		(list NIL NIL NIL NIL NIL NIL NIL)))

(defun parse-scheme (uri-list temp result)
  (if (atom uri-list)
      (format t "URI Syntax Error - please insert a valid URI")
      (progn
	(if (is-special-char (car uri-list))
	    (format t "URI Syntax Error : please insert a valid URI")
	    (progn
	      (push (car uri-list) temp)
	      (if (equalp (car (cdr uri-list)) '#\:)
		  (progn
		    (setf (first result) (reverse temp))
		    (verify-scheme-syntax (cdr (cdr uri-list)) result))
		  (parse-scheme (cdr uri-list) temp result)))))))

(defun verify-scheme-syntax (uri-list result)
  (let ((scheme-string (list-to-string (first result))))
    (cond
      ((equalp scheme-string "mailto")
       (parse-mailto-userinfo uri-list NIL result))
      ((equalp scheme-string "news") (parse-special-host uri-list NIL result))
      ((or (equalp scheme-string "tel") (equalp scheme-string "fax"))
       (parse-telfax-userinfo uri-list NIL result))
      ((and (equalp (car uri-list) '#\/) (equalp (car (cdr uri-list)) '#\/))
       (parse-authorithy (cdr (cdr uri-list)) NIL result))
      ((and (equalp (car uri-list) '#\/)       
	    (equalp (car (cdr uri-list)) NIL)) 
       result)                                
      ((and (equalp (car uri-list) '#\/)                  
	    (equalp (car (cdr uri-list)) '#\?))           
       (parse-query (cdr (cdr uri-list)) NIL result))     
      ((and (equalp (car uri-list) '#\/)                  
	    (equalp (car (cdr uri-list)) '#\#))          
       (parse-fragment (cdr (cdr uri-list)) NIL result))  
      ((equalp (car uri-list) '#\/)
       (parse-path (cdr uri-list) NIL result))
      (T (format t "URI Syntax Error: please insert a valid URI")))))

(defun is-special-char (my-char)
  (if (or (equalp my-char '#\/)
	  (equalp my-char '#\?)
	  (equalp my-char '#\#)
	  (equalp my-char '#\@)
	  (equalp my-char '#\:)
	  (equalp my-char '#\ ))
      T NIL))

(defun parse-mailto-userinfo (uri-list temp result)
  (if (atom uri-list)
      (format t "URI Syntax Error: atomic-mailto-userinfo")
      (progn
	(if (is-special-char (car uri-list))
	    (format t "URI Syntax Error : mailto-special-char in wrong position")
	    (progn
	      (push (car uri-list) temp)
	      (cond
		((equalp (car (cdr uri-list)) '#\@)
		 (setf (second result) (reverse temp))
		 (parse-special-host (cdr (cdr uri-list)) NIL result))
		((equalp (car (cdr uri-list)) NIL)
		 (setf (second result) (reverse temp))
		 result)
		(T
		 (parse-mailto-userinfo (cdr uri-list) temp result))))))))

(defun parse-special-host (uri-list temp result)
  (if (atom uri-list)
      (format t "URI Syntax Error: Atomic Special Host")
      (progn
	(if (or (is-special-char (car uri-list))
		(equalp (car uri-list) '#\.))
	    (format t "URI Syntax Error: Char Special Host")
	    (progn
	      (push (car uri-list) temp)
	      (cond
		((equalp (car (cdr uri-list)) '#\.)
		 (push (car (cdr uri-list)) temp)
		 (parse-special-host (cdr (cdr uri-list)) temp result))
		((equalp (car (cdr uri-list)) NIL)
		 (setf (third result) (reverse temp))
		 result)
		(T
		 (parse-special-host (cdr uri-list) temp result))))))))

(defun parse-telfax-userinfo (uri-list temp result)
  (if (atom uri-list)
      (format t "URI Syntax Error: Atomic Telfax Userinfo")
      (progn
	(if (is-special-char (car uri-list))
	    (format t "URI Syntax Error: Char Telfax Userinfo")
	    (progn
	      (push (car uri-list) temp)
	      (cond
		((equalp (car (cdr uri-list)) NIL)
		 (setf (second result) (reverse temp))
		 result)
		(T
		 (parse-telfax-userinfo (cdr uri-list) temp result))))))))

(defun parse-authorithy (uri-list temp result)
  (if (atom uri-list)
      (format t "URI Syntax Error: Atomic Authorithy")
      (progn
	(if (is-special-char (car uri-list))
	    (format t "URI Syntax Error: Char Authorithy")
	    (progn
	      (push (car uri-list) temp)
	      (cond
		((equalp (car (cdr uri-list)) '#\@)
		 (setf (second result) (reverse temp))
		 (parse-host (cdr (cdr uri-list)) NIL result))
		((equalp (car (cdr uri-list)) '#\:)
		 (if (dots-check temp)
		     (progn
		       (setf (third result) (reverse temp))
		       (parse-port (cdr (cdr uri-list)) NIL result))
		     (format t "URI Syntax Error: too many dots")))
		((equalp (car (cdr uri-list)) '#\/)
		 (if (dots-check temp)
		     (progn
		       (setf (third result) (reverse temp))
		       (cond
			 ((equalp (car (cdr (cdr uri-list))) '#\?)
			  (parse-query (cdr (cdr (cdr uri-list))) NIL result))
			 ((equalp (car (cdr (cdr uri-list))) '#\#)
			  (parse-fragment (cdr (cdr (cdr uri-list))) NIL result))
			 ((equalp (car (cdr (cdr uri-list))) NIL)
			  result)
			 (T
			  (parse-path (cdr (cdr uri-list)) NIL result))))
		     (format t "URI Syntax Error: too many dots")))
		((equalp (car (cdr uri-list)) NIL)
		 (if (dots-check temp)
		     (progn 
		       (setf (third result) (reverse temp))
		       result)
		     (format t "URI Syntax Error: too many dots")))
		(T
		 (parse-authorithy (cdr uri-list) temp result))))))))

(defun parse-path (uri-list temp result)
  (if (atom uri-list)
      (format t "URI Syntax Error: Atomic Path")
      (progn 
	(if (is-special-char (car uri-list))
	    (format t  "URI Syntax Error: Char Path")
	    (progn
	      (push (car uri-list) temp)
	      (cond 
		((equalp (car (cdr uri-list)) '#\/)
		 (if (equalp (car (cdr (cdr uri-list))) NIL)
		     (progn
		       (setf (fifth result) (reverse temp))
		       result)
		     (progn
		       (push (car (cdr uri-list)) temp)
		       (parse-path (cdr (cdr uri-list)) temp result))))
		((equalp (car (cdr uri-list)) '#\?)
		 (setf (fifth result) (reverse temp))
		 (parse-query (cdr (cdr uri-list)) NIL result))
		((equalp (car (cdr uri-list)) '#\#)
		 (setf (fifth result) (reverse temp))
		 (parse-fragment (cdr (cdr uri-list)) NIL result))
		((equalp (car (cdr uri-list)) NIL)
		 (setf (fifth result) (reverse temp))
		 result)
		(T
		 (parse-path (cdr uri-list) temp result))))))))

(defun parse-query (uri-list temp result)
  (if (atom uri-list)
      (format t "URI Syntax Error: Atomic Query")
      (progn
	(if (or (equalp (car uri-list) '#\ )        
		(equalp (car uri-list) '#\#))       
	    (format t "URI Syntax Error: Char query")
	    (progn
	      (push (car uri-list) temp)
	      (cond
		((equalp (car (cdr uri-list)) '#\#)
		 (setf (sixth result) (reverse temp))
		 (parse-fragment (cdr (cdr uri-list)) NIL result))
		((equalp (car (cdr uri-list)) NIL)
		 (setf (sixth result) (reverse temp))
		 result)
		(T
		 (parse-query (cdr uri-list) temp result))))))))

(defun parse-fragment (uri-list temp result)
  (if (atom uri-list)
      (format t "URI Syntax Error: Atomic Fragment")
      (progn
	(if (or (equalp (car uri-list) '#\ )
		(equalp (car uri-list) NIL))
	    (format t "URI Syntax Error: Char Fragment")
	    (progn
	      (push (car uri-list) temp)
	      (cond                                     
		((equalp (car (cdr uri-list)) NIL)      
		 (setf (seventh result) (reverse temp)) 
		 result)
		(T
		 (parse-fragment (cdr uri-list) temp result))))))))

(defun is-number (mychar)
  (cond
    ((equalp mychar '#\0) T)
    ((equalp mychar '#\1) T)
    ((equalp mychar '#\2) T)
    ((equalp mychar '#\3) T)
    ((equalp mychar '#\4) T)
    ((equalp mychar '#\5) T)
    ((equalp mychar '#\6) T)
    ((equalp mychar '#\7) T)
    ((equalp mychar '#\8) T)
    ((equalp mychar '#\9) T)
    (T NIL)))

(defun parse-port (uri-list temp result)
  (if (atom uri-list)
      (format t "URI Syntax Error: Atomic Port")
      (progn
	(if (not (is-number (car uri-list)))
	    (format t "URI Syntax Error: Number Port")
	    (progn
	      (push (car uri-list) temp)
	      (cond
		((equalp (car (cdr uri-list)) NIL)
		 (setf (fourth result) (reverse temp))
		 result)
		((equalp (car (cdr uri-list)) '#\/)
		 (setf (fourth result) (reverse temp))
		 (cond                                                       
		   ((equalp (car (cdr (cdr uri-list))) '#\?)                 
		    (parse-query (cdr (cdr (cdr uri-list))) NIL result))     
		   ((equalp (car (cdr (cdr uri-list))) '#\#)                 
		    (parse-fragment (cdr (cdr (cdr uri-list))) NIL result))  
		   ((equalp (car (cdr (cdr uri-list))) NIL)                  
		    result)                                                  
		   (T                                                        
		    (parse-path (cdr (cdr uri-list)) NIL result))))          
		(T
		 (parse-port (cdr uri-list) temp result))))))))

(defun parse-host (uri-list temp result)
  (if (atom uri-list)
      (format t "URI Syntax Error: Atomic Host")
      (progn 
	(if (is-special-char (car uri-list))
	    (format t "URI Syntax Error: Char Host"))
	(if (equalp (car uri-list) '#\.)
	    (format t "URI Syntax Error: Dots")
	    (progn
	      (push (car uri-list) temp)
	      (cond
		((equalp (car (cdr uri-list)) NIL)
		 (setf (third result) (reverse temp))
		 result)
		((equalp (car (cdr uri-list)) '#\.)
		 (push (car (cdr uri-list)) temp)
		 (parse-host (cdr (cdr uri-list)) temp result))
		((equalp (car (cdr uri-list)) '#\:)
		 (setf (third result) (reverse temp))
		 (parse-port (cdr (cdr uri-list)) NIL result))
		((equalp (car (cdr uri-list)) '#\/)
		 (setf (third result) (reverse temp))
		 (cond
		   ((equalp (car (cdr (cdr uri-list))) '#\?)
		    (parse-query (cdr (cdr (cdr uri-list))) NIL result))
		   ((equalp (car (cdr (cdr uri-list))) '#\#)
		    (parse-fragment (cdr (cdr (cdr uri-list))) NIL result))
		   ((equalp (car (cdr (cdr uri-list))) NIL)
		    result)
		   (T
		    (parse-path (cdr (cdr uri-list)) NIL result))))
		(T
		 (parse-host (cdr uri-list) temp result))))))))

(defun dots-check (list)
  (cond
    ((atom list) T)
    ((and (equalp (car list) '#\.) (equalp (car (cdr list)) '#\.))
     NIL)
    (T
     (dots-check (cdr list)))))

(defun uri-scheme (result)
  (list-to-string (first result)))

(defun uri-userinfo (result)
  (list-to-string (second result)))

(defun uri-host (result)
  (list-to-string (third result)))

(defun uri-port (result)
  (list-to-string (fourth result)))

(defun uri-path (result)
  (list-to-string (fifth result)))

(defun uri-query (result)
  (list-to-string (sixth result)))

(defun uri-fragment (result)
  (list-to-string (seventh result)))
