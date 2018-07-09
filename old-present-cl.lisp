;;; -*- Mode:LISP; Base:10; Package: RULE; Syntax: Common-lisp -*-
;;;++  itemize, key, etc should be in keyword package!!


;;; The format of the presentation stored under the PRESENTATION property
;;; of an msg object is:
;;;
;;; (:presentation
;;;    (:prolog PART)
;;;    (:body PARTS)
;;;    (:epilog PART))
;;;
;;; At least one of a prolog, body, or epilog must be present.
;;;
;;; The syntax for PARTS is:
;;;
;;; (optional PART PART PART ... )
;;; or
;;; ((key1 PARTS)
;;;  (key2 PARTS)
;;;  (key3 PARTS)
;;;       .
;;;       .
;;;  (keyn PARTS))
;;; where optional is ITEMIZE or SERIES
;;;
;;; The syntax for PART is:
;;;
;;; (SUBPART SUBPART .....)
;;; or
;;; ((key1 PART)
;;;  (key2 PART)
;;;  (key3 PART)
;;;       .
;;;       .
;;;  (keyn PART))
;;;
;;; key1, key2,..., keyn are dispatched to with corresponding names in the directions
;;; structure. (See below.)
;;;+++ revise this....
;;;  A SUBPART can be a:
;;;  - string: output string
;;;  - a list with the head element being: funcall,  apply, format: form. evaluate
;;;  - other list: treat it as a PART
;;;  - symbol: get its value and if there is no value convert to string and output else if:
;;;	- value is an object: o ':PRESENT it.
;;;     - value is a string: output it.
;;;     - value is a file pathname: open file and dump contents to user.
;;;
;;; *directions* is a global variable that has the directions for PPs in it.
;;; directions can be also attached to a particular object.
;;;
;;; directions have the following format:
;;; association list with one entry being keyed with "global"
;;; other keys being class names or names of msg objects.

(defmacro instance (thing)
  `(and (boundp ',thing) (instancep ,thing)  ,thing))

(defmacro object (thing)
  nil)

(defmacro objectp (thing)
  nil)

(declare (special object))

(declare (special stream))

(defvar *directions*  ())

(defvar *presented-objects* nil)

(defvar *mouseable-objects* nil)

(defvar *functions* '(prog prog1 progn do dolist eval-and-replace series itemize key
			eval loop mapc ormat apply present mouse-item when))

(defun get-type-of (object)
  (type-of object))

(defun make-sensititive (object stream &optional capitalize)

  (mouseify-it
    (say-simple object capitalize)
    (get-type-of object)
    object
    stream)
  )

(defmacro present-it (obj stream &rest keys)
  ;;(:clear-window stream)
  (if (object obj)
      `(:present ',(object obj) ',stream ,@keys)
      `(:present ',(class obj) ',stream ,@keys)))


(defun present-object (object stream &rest keys &aux presentation)
  (if (instancep object)
      (apply 'present object stream keys)	;instance  -- message
      (if (listp object)			;(instance ivar) pair ?
	  (setq presentation (apply 'flavor-ivar-get2 :present object))
	  (setq presentation (get object :present)))
      (unless presentation (present-simple object stream keys)) 
      (if (listp presentation) (present-with-presentation object presentation stream keys)
	  ;;encountered a symbol-- check to see if this has a presentation
	  (unless (and (setq presentation (get presentation ':present))
		       (present-with-presentation object presentation stream keys)
		       (error "No presenatation for ~A" presentation))))))


;;;+++ when I fix up capitalization delete the capitalize
(defun present-simple (object stream ignore)
  (output-string (say-simple object t) stream)) 

(defun say-simple (object &optional (capitalizep t))
  (if (instancep object)
      (let ((stuff (:say object)))
	(if (listp stuff)
	    (pretty-pname-list stuff t)
	    (format nil "~A" (say object))))
      (format nil "~A" (pretty-pname object capitalizep))))

(defun present-with-presentation (object presentation stream keys)
  (let ((prolog (assoc :prolog presentation))
	(body   (assoc :body   presentation))
	(epilog (assoc :epilog presentation)))
    ;;(if (typep object 'class) (:SET-MOUSE-PPS object stream))
    (if prolog (present-part1 object (cadr prolog) stream keys))
    (if body   (present-parts1 object (cadr body) stream keys))
    (if epilog (present-part1 object (cadr epilog) stream keys))))

(defun present-part1 (object part stream keys)
(present-part object part stream (get-directions object keys)))

;;;+++ meta direction of adding a period and capitilize needed?
(defun present-part (object part stream directions)
  (if (typep (car part) LIST)			;part indexed by keywords?
      (dolist (direction directions)		;+ this syntax is ambiguous! (with eval forms)
	(let ((subpart (assoc direction part)))
	  (when subpart
	    (present-part object (cadr subpart) stream directions))))
      (dolist (subpart part)			;list of parts
	(typecase subpart
	  (LIST (if (and (typep (car subpart) ATOM)
			  (member (car subpart) *functions*))
		    (case (car subpart)
		       
		       (EVAL-AND-REMEMBER
			(eval-and-remember object
					   (cddr subpart)		;things
					   stream
					   (eval (cadr subpart))	;Space
					   ))
							     
		       (EVAL-AND-REPLACE
			 (eval-and-replace object
					   (eval (caddr subpart))
					   stream
					   (cadr subpart)))
		       ;;ITEM,ITEM,...,AND ITEM
		       (SERIES (apply 'series object
					 (eval (caddr subpart))		;things
					 stream
					 (cadr subpart)			;directions
					 (cdddr subpart)))		;options
		       ;;OUTLINE FORM
		       (ITEMIZE (apply 'itemize object
					 (eval (caddr subpart))		;things
					 stream
					 (cadr subpart)			;directions
					 (cdddr subpart)))		;options
		       (PRESENT (apply 'PRESENT
				    (eval (cadr subpart)) stream (cddr subpart)))
		       (FORMAT (output-string (eval subpart) stream))
		       (WHEN (when (eval (cadr subpart))
			     (present-part object (cddr subpart) stream directions)))
		       (KEY (when (member (cadr subpart) directions)
			      (present-part object (cddr subpart) stream directions)))
;>>		       ;;I changed these to be evaled
		       (MOUSE-ITEM  (mouseify-it (eval (cadr subpart))	;string
						 (eval (caddr subpart))	;mouse-item alist key
						 (eval (cadddr subpart));item
						stream))
		       ;;ELSE SOMETHING TO CALL
		       (OTHERWISE  (eval subpart)))  ;+do this better
		     (present-part object subpart stream directions)))	;else list to present
	  (STRING (unless (= (string-length subpart) 0) (output-string subpart stream)))
	  (SYMBOL
	    (cond ((eq subpart '*)    (output-string
					  (say-simple object nil)
					  stream))	;way of saying me tersly
		 ((eq subpart '**)   (output-string
					  (say-simple object t)
					  stream))
		 ;;++ this needs extending to symbols sometime
		 ((eq subpart '***)      (make-sensititive object stream))
		 ((eq subpart '****)     (make-sensititive object stream t))
		 ((object subpart) (apply 'PRESENT (object subpart) stream directions))
		 ((class subpart) (or (zl:errset (apply 'PRESENT
						  (class subpart) stream directions) nil)
				      (output-string
					(string-capitalize-words subpart) stream)))
		 ;;++This bound stuff is cludgy What if something was
		 ;;++bound by accident?
		 ((boundp subpart)
		  (setq subpart (symeval subpart))	;get value
		  (cond ((class subpart) (apply 'PRESENT
					     (class subpart) stream directions))
			((object subpart) (apply 'PRESENT
					      (object subpart) stream directions))
			((typep subpart 'STRING) (output-string subpart stream))
			((typep subpart 'SYMBOL)
			 (output-string (string-capitalize-words subpart) stream))
			#+ignore
			((typep subpart 'FS:LMFS-PATHNAME) (output-file subpart stream))
			((instancep subpart) (apply 'PRESENT subpart stream directions))
			(T (error "I do not know how to present ~A." subpart))))
		 ;;;needds work
		 ((get subpart ':presentation) (apply 'present subpart
							      stream directions))
		 (T (output-string (string-capitalize-words subpart) stream))))
	  (:fixnum
	   (output-string (format nil "~D" subpart) stream))
	  (:single-float
	   (output-string (format nil "~A" subpart) stream))
	  (OTHERWISE (if (instancep subpart)
			 (apply 'PRESENT subpart stream directions) ;I put it back!
			 (error "I do not know how to present ~A." subpart)))))))


(defun present-parts1 (object body stream keys)
  (present-parts object body stream (get-directions object keys)))

(defun present-parts (object body stream directions)
  (if (typep (car body) LIST)			;part indexed by keywords?
      (dolist (direction directions)
	(let ((subpart (assoc direction body)))
	  (when subpart
	    (present-parts object (cadr subpart) stream directions))))
      (case (car body)
	(SERIES    (apply 'series object
				  (eval (caddr body))		;things
				  stream
				  (cadr body)			;directions
				  (cdddr body)))		;options
	(ITEMIZE   (apply 'itemize object
				  (eval (caddr body))		;things
				  stream
				  (cadr body)			;directions
				  (cdddr body)))		;options
	(PRESENT   (apply 'PRESENT (eval (cadr body)) stream (cddr body)))
	(WHEN (when (eval (cadr body))
		(present-part object (cddr body) stream directions)))
	(FORMAT    (output-string (eval body) stream))
	(MOUSE-ITEM  (mouseify-it (eval (cadr body))	;string
				  (eval (caddr body))	;mouse-item alist key
				  (eval (cadddr body))
				 stream))	;thing to do
	(OTHERWISE (present-part object body stream directions))))) 



;;; forms like this are used to define the different types of mouse items
;;;(tv:add-typeout-item-type
;;;  '*mouse-items*
;;;  'subprocedure
;;;  "expand this subprocedure"
;;;  nil
;;;  t
;;;  "expand this subprocedure")
(defun mouseify-it (string alist-key eval-form stream)
  (words-out stream string alist-key eval-form))

;;; for now...
(defun eval-and-replace (object things stream directions)
  (present-part object things stream directions))

(defun eval-and-remember (object things stream space)
  (stuff-out stream `((progn ,@things) ,space)))

;;;+++ Should I impose the rule that all who supply values for itemize add periods and caps?
;;;+++ or should I do this?
;;;+++ want "flattened" lists sometime?
;;; marker is either:
;;; 'number - numbers are used to itemize
;;; character - that character is used in current font
;;; list - (character font) the character is used in that font
;;; (0 fonts:medfnb) is also good
;;;
(defun itemize (object things stream directions
		&optional (marker `(30 ,fonts:mouse)) (limit 99))
  (:INDENT-RELATIVE stream 3)
  (:new-LINE stream)
  (if (= (length things) 1) (present-thing object (car things) stream directions)
      (loop with last-thing = (length things)
	    with pentultimate = (- last-thing 1)
	    for thing in things
	    for i from 1 to limit
	    ;; output list item number
	    do (when marker
		 (:INDENT-RELATIVE stream 3)
		 (if (equal marker 'number)
		     ;;So this comes out hard hardcopy
		     (words-out stream (format  nil "~D.~2T" i))
		     #+ignore
		     (:stuff-out stream
			   `((format ',stream "~D.~2T" ,i) 4))
		     (:stuff-out stream
		     `((multiple-value-bind
			 (xpos ypos)
			   (:read-cursorpos ',stream)
			 (format ',stream " ")
			 ,(if (listp marker)
			     `(':draw-char
			    ',stream ',(cadr marker) ',(car marker)
				    xpos ypos)
			     `(:draw-char ',stream
				(:current-font ',stream)
				 ',marker xpos ypos))
		       (format ',stream "~2T"))
		       4))))			;How much it uses
	    do (present-thing object thing stream directions)
	    when marker do (:INDENT-RELATIVE stream -3)
	    when (user-quit-p) do (loop-finish)	;quit if user doesn't want to see
	    ;; need to fix auto space between strings for this to work....
	    ;;when (eq i pentultimate) do (output-string (format nil ", and ") stream)
	    ;;when (< i  pentultimate) do (output-string (format nil ",")stream)
	    ;;when (eq i last-thing) do (output-string (format nil ".") stream)
	    ;;do (output-string (format nil "~C." #\bs) stream)
	    do (:new-LINE stream)))
  (:INDENT-RELATIVE stream -3)
  (:new-LINE stream)	;+ really need this for indenting to work
  )


(defun series (object things stream directions) 'NYI)

(defun present-thing (object thing stream directions)
      (if (listp thing) (funcall 'PRESENT-PART  object thing stream directions)
	                (funcall 'PRESENT-PART  object (list thing) stream directions)))

(defun user-quit-p  () ())

(defmacro direction-push (item list)
  `(loop for it in ,item
	do (push it ,list)))

;; handle case when object isn't a name
(defun get-directions (object keys)		;this will break when msg-class = object
  (let ((directions nil))
    (unless (or (member ':FORCE keys) ;don't collect keys from other sources if force is set
		(symbolp object)
		(listp object)			;or object is a symbol +++ FIX SOMETIME +++
		t)
      (let* ((class       (if (objectp object) (:CLASS object) ()))
	     (class-name  (if class (:FLAVOR-NAME class) ()))
	     (object-name (:IDENTIFICATION object)))
	;;GET INTERACTION WITH CALLING SYSTEM/USER	 
	;;CO-ROUTINE STRUCTURE?
	(get-interaction)
	;;GET GLOBAL KEYS
	(direction-push (cdr (assoc :global *directions*)) directions)
	;;GET GLOBAL KEYS OF CLASS
	(when class (direction-push (cdr (assoc class-name *directions*)) directions))
	;;GET GLOBAL KEYS OF INSTANCE
	(direction-push (cdr (assoc object-name *directions*)) directions)
	;;GET CLASS KEYS
	(when class (direction-push (PRESENTATION-KEYS class) directions))
	;;GET INSTANCE KEYS
	(direction-push (PRESENTATION-KEYS object) directions)))
    ;;IF KEYS ARE PASSED PUT THEM ON
    (when keys (direction-push keys directions))
    ;;CHECK FOR NO DIRECTION CASE
    (unless directions (setq directions (list 'simple)))
    ;; record the keys on presentation-history instance var...
    ;; ++ add markers to the history mechanism
    #+ignore
    (if (typep object 'presentable-mixin)
	(push directions (:presentation-history self)))
    (reverse directions)))


(defun output-file (pathname) ())

(defun get-interaction () ())

(defun output-string (string stream)
  (:WORDS-OUT stream string)) 
