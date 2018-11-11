(in-package 3bil2)

(defmacro/3bil2 setf (&whole form &environment env
                             place new-value-form
                             &rest more-pairs)
  (cond ((null more-pairs)
	 (multiple-value-bind (variables
			       values
			       store-variables
			       writer-form
			       reader-form)
	     (sicl-global-environment:get-setf-expansion place env)
	   (declare (ignore reader-form))
	   (print
            `(let* ,(mapcar #'list variables values)
	       ;; Optimize a bit when there is only one store variable.
	       ,(if (= 1 (length store-variables))
                    ;; optimize a bit more when value is a constant
		    (if (constantp new-value-form)
                        `(symbol-macrolet ((,(first store-variables)
                                             ,new-value-form))
                           ,writer-form)
                        `(let ((,(first store-variables) ,new-value-form))
		           ,writer-form))
		    `(multiple-value-bind ,store-variables
			 ,new-value-form
		       ,writer-form))))))
	((not (null (cdr more-pairs)))
	 `(progn (setf ,place ,new-value-form)
		 (setf ,@more-pairs)))
	(t
         (error "odd-number-of-arguments-to-setf :form ~s" form))))

(defun define-default-setf-expander (environment)
  (setf (sicl-genv:default-setf-expander environment)
	(lambda (form environment)
          (declare (ignore environment))
	  (if (symbolp form)
	      (let ((new (gensym)))
		(values '()
			'()
			`(,new)
			`(setq ,form ,new)
			form))
	      (let ((temps (loop for arg in (rest form) collect (gensym)))
		    (new (gensym)))
		(values temps
			(rest form)
			`(,new)
			`(funcall #'(setf ,(first form)) ,new ,@temps)
                        `(,(first form) ,@temps)))))))

(define-default-setf-expander *3bil2-environment*)



(defmacro define-setf-expander/3bil (name lambda-list &body body)
  `(let ((expander ,(cleavir-code-utilities:parse-macro name lambda-list body))
	 (global-env *3bil2-environment*))
     (setf (sicl-global-environment:setf-expander ',name global-env)
           expander)))

;;; FIXME: handle the long form
(defmacro defsetf/3bil2 (access-fun update-fun-or-lambda-list &rest rest)
  (cond ((symbolp update-fun-or-lambda-list)
	 (assert (or (null rest) (stringp (car rest))))
	 `(define-setf-expander/3bil ,access-fun (&rest args)
	    (let* ((vars (loop for arg in args collect (gensym)))
		   (store-var (gensym))
		   (writer-form `(,',update-fun-or-lambda-list
				  ,@vars ,store-var))
		   (reader-form `(,',access-fun ,@vars)))
	      (values vars
		      args
		      (list store-var)
		      writer-form
		      reader-form))))
        (t (error "can't handle the long form of defsetf yet"))))


(defsetf/3bil2 slot-value %set-slot-value)

