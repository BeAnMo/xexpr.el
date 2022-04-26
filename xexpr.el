;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

;; file: 2022-04-24_21-08-28.json
;; realty--file-view 0.25s
;; realty--prop-listing 0.23s
;; realty--render-template 18s

(defun xexpr--atom-p (xexpr)
  (or (numberp xexpr)
      (stringp xexpr)
      (symbolp xexpr)))

(defun xexpr--atom-to-string (xexpr)
  (format "%s" xexpr))

(defun xexpr--attrs-p (xexpr)
  (and (listp xexpr)
       (not nil)
       (and (listp (car xexpr))
	    (xexpr--atom-p (cdr (car xexpr))))))

(defun xexpr--attrs-to-string (xexpr)
  (let ((results nil))
    (dolist (attr xexpr results)
      (setq results (cons (format "%s=\"%s\""
				  (car attr)
				  (cdr attr))
			  results)))
    (string-join (nreverse results) " ")))

(defun xexpr--2-nodes-to-string (xexpr)
  (pcase (cadr xexpr)
    ((pred xexpr--atom-p)
     (format "<%s>%s</%s>"
	     (car xexpr)
	     (xexpr-to-string (cadr xexpr))
	     (car xexpr)))
    ((pred xexpr--attrs-p)
     (format "<%s %s></%s>"
	     (car xexpr)
	     (xexpr--attrs-to-string (cadr xexpr))
	     (car xexpr)))
    ((pred listp)
     (format "<%s>%s</%s>"
	     (car xexpr)
	     (xexpr-to-string (cadr xexpr))
	     (car xexpr)))))

(defun xexpr--n-nodes-to-string (xexpr)
  (let ((tag (car xexpr))
	(children (cdr xexpr))
	(attrs nil)
	(args nil))
    (when (xexpr--attrs-p (car children))
      (setq attrs (xexpr--attrs-to-string (car children)))
      (setq children (cdr children)))
    (dolist (child children args)
      (setq args (cons (xexpr-to-string child) args)))
    (let ((start (if attrs
		     (format "<%s %s>" tag attrs)
		   (format "<%s>" tag)))
	  (end (format "</%s>" tag)))
      (format "%s%s%s" start (string-join (nreverse args) "\n") end))))

(defun xexpr-to-string (xexpr)
  "Converts the given XEXPR to a string.
   ex:
    '(hr) -> <hr />
    '(p \"hello\") -> <p>hello</p>
    '(p \"hey, \" (a ((href . \"example.com\")) \"click\") \" here\") 
       -> <p>hey, <a href=\"example.com\">click</a> here</p>"
  (cond
   ((null xexpr) "")
   ((xexpr--atom-p xexpr) (xexpr--atom-to-string xexpr))
   (t (pcase xexpr
	((and (pred listp) (app length 1))
	 (format "<%s />" (car xexpr)))
	((and (pred listp) (app length 2))
	 (xexpr--2-nodes-to-string xexpr))
	((pred listp)
	 (xexpr--n-nodes-to-string xexpr))))))

(provide 'xexpr)
