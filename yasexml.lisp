(cl:defpackage :yasexmla
  (:documentation 
   "YASEXML: Yet Another Symbolic Expression eXtensible Markup Language")
  (:use :cl)
  (:nicknames :yasexml/yasexmla)
  (:import-from :cxml)
  (:import-from :sax)
  (:export #:<>
	   #:tag
	   #:call-with-tag 
	   #:call-with-handler
	   #:call-with-current-handler))
(cl:in-package :yasexmla)

(defgeneric call-with-tag (function tag &rest tag-attributes))

(defgeneric call-with-handler (function handler)
  (:method (function handler)  
    (funcall function handler)))

(defgeneric default-handler (package)
  (:method (package)
    (cxml:make-string-sink 
     :canonical nil
     :omit-xml-declaration-p t)))

(defun wrap-in-handler (tag function)
  (cond ((and (not (boundp '*handler*))
	      (not (eql (if (listp tag) (first tag) tag)
			:handler)))
	 (<> (:handler (default-handler *package*))
	   (wrap-in-handler tag function)))
	(t
	 (funcall function))))

(defgeneric tag (tag &optional function)
  (:method 
      :around (tag &optional (function (constantly nil))) 
      (wrap-in-handler
       tag (lambda () (call-next-method tag function))))
  (:method ((tag symbol) &optional (f (constantly nil)))
    (call-with-tag (lambda (tag) 
		     (declare (ignore tag))
		     (funcall f)) 
		   tag))
  (:method ((tag string) &optional (f (constantly nil)))
    (call-with-tag (lambda (tag) 
		     (declare (ignore tag))
		     (funcall f))
		   :text tag))
  (:method ((tag list) &optional (f (constantly nil)))
    (apply #'call-with-tag (lambda (tag) 
			     (declare (ignore tag))
			     (funcall f)) tag)))

(progn 
  (defvar *handler*)
  (setf (documentation '*handler* 'variable)
	"The current SAX handler for CALL-WITH-TAG to use")
  
  (defvar *document*)
  (setf (documentation '*document* 'variable)
	"The current SAX handler for CALL-WITH-TAG to use"))

(defun call-with-current-handler (function)
  (call-with-handler function *handler*))


(defmethod call-with-tag (fn (tag (eql :handler)) 
			  &rest handler)
  (let ((*handler* (first handler))
	(*document* (when (boundp '*document*)
		      *document*)))
    (call-with-handler (lambda (h)
			 (tag :document (lambda () (funcall fn h))))
			 handler)))

(defmethod call-with-tag (fn (tag (eql :document)) 
			  &rest _)
  (declare (ignore _))
  (call-with-current-handler #'sax:start-document)
  (let ((*document* t))
    (funcall fn tag))
  (call-with-current-handler #'sax:end-document))

(defvar *prefix-maps* 
  nil)

(defun call-with-prefix-mapping (fn prefix namespace-uri)
  (call-with-current-handler 
   (lambda (handler)
     (let ((*prefix-maps* (acons prefix namespace-uri *prefix-maps*)))
       (funcall fn (sax:start-prefix-mapping handler prefix namespace-uri))
       (sax:end-prefix-mapping handler prefix)))))

(defmethod call-with-tag (fn (tag (eql :xmlns)) 
			  &rest |(prefix uri)|)
  
  (destructuring-bind (prefix uri) |(prefix uri)|
    (call-with-prefix-mapping fn prefix uri)))

(defmethod call-with-tag (fn (tag (eql :text)) &rest text)
  
  (call-with-current-handler 
   (lambda (handler) 
     (funcall fn (map 'list #'(lambda (text)
				(sax:characters handler (princ-to-string text))) text)))))

(defmethod call-with-tag (fn (tag (eql :unescaped)) &rest text)
  (call-with-current-handler 
   (lambda (handler) 
     (funcall fn (map 'list #'(lambda (text)
				(sax:unescaped handler (princ-to-string text))) text)))))

(defmethod call-with-tag (fn (tag (eql :comment)) &rest text)
  (call-with-current-handler 
   (lambda (handler) 
     (funcall fn (map 'list #'(lambda (text)
				(sax:comment handler 
					     (princ-to-string text))) text)))))

(defun lowercase-p (tag)
  (every (lambda (char) 
	   (or (and (alpha-char-p char) 
		    (upper-case-p char))
	       (and (not (alpha-char-p char))
		    t)))
	 (symbol-name tag)))
  
  
(defmethod call-with-tag (fn (tag string) &rest tag-attributes)
  (call-with-current-handler 
   (lambda (handler)
     (let* ((sep (position #\: tag))

	    (prefix (when sep (subseq tag 0 sep)))
	    (local-name (when sep (subseq tag (1+ sep))))
	    (local-prefix-map (loop :for (name value) 
				 :on tag-attributes :by #'cddr
				 :nconc (when (ignore-errors 
						(string= "xmlns:" (string name) 
							 :end2 6))
					  (let ((prefix (subseq (string name) 6)))
					    (sax:start-prefix-mapping handler prefix value)
					    (list (cons prefix value))))))
	    (*prefix-maps* (append local-prefix-map *prefix-maps*))
	    (namespace-uri (cdr (assoc prefix *prefix-maps* 
				       :test #'string=))))
      ; (break "Tag? ~A ~A ~A" tag namespace-uri *prefix-maps*)
       
     (sax:start-element 
      handler namespace-uri (or local-name tag) (runes:rod tag)
      (loop :for (name value) 
	 :on tag-attributes :by #'cddr
	 :nconc 
	 (progn ;(break "~A : ~A" name value)
	 (unless (ignore-errors 
		   (string= "xmlns:" (string name) 
			    :end2 6))
	  
	   (let ((local-name 
		  (etypecase name
		    (string name)
		    (symbol 
		     (cond ((lowercase-p name)
			    (string-downcase name))
			   (t (symbol-name name)))))))
	   (list (sax:make-attribute  
		  :local-name local-name
		  :qname local-name
		  :value (princ-to-string value))))))))
     (funcall fn tag)
     (sax:end-element handler namespace-uri (or local-name tag) (runes:rod tag))))))

(defmethod call-with-tag (fn (tag symbol) &rest tag-attributes)
  (apply #'call-with-tag fn 
	 (cond ((lowercase-p tag)
		(string-downcase tag))
	       (t (symbol-name tag))) tag-attributes))

(defmacro <> (tag &body body)
  `(tag
    , (typecase tag 
	(symbol `',tag)
	(list
	 (if (let* ((*package* (find-package :cl))
		    (tag-prefix (aref (princ-to-string tag) 0)))
	       (or (eql #\` tag-prefix)
		   (eql #\' tag-prefix)))
	     ;; This must be a quote or backquote, so pass it along.
	     tag
	     `(list ',(first tag)
		    ,@(rest tag))))
	(t tag))
      (lambda () ,@body)))

(setf (documentation '<> 'function)
 #.(symbol-name '#:|=> /whatever &body returns/
Example : (<> (:handler (cxml:make-string-sink
		     :indentation 1))
	   (<> (test-tag :test-attribute "test-attribute-value")
	     (<> "Test Text as Tag" (<> (br)))
	     (<> (:text "Test :TEXT as Tag") 
	       (<> ("foo bar=baz" :bat "?"))
	       (<> (:text "Test " ":TEXT" "many strings"))
	       (<> `(:text ,(concatenate 
			     'string "Test " ":TEXT "
			      "many strings with backquote " 
			      "and concatenate" ))
		 (<> `(,(funcall (constantly 'backquoted-tag-name))
			,@(list 'list "attribute")))))))
	     
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<test-tag test-attribute=\"test-attribute-value\">
 Test Text as Tag
 <br/>
 Test :TEXT as Tag
 <foo bar=baz bat=\"?\"/>
 Test
 :TEXT
 many strings
 Test :TEXT many strings with backquote and concatenate
 <backquoted-tag-name list=\"attribute\"/>
</test-tag>"|))


;; Copyright (c) 2014 Drew Crampsie <me@drewc.ca>
;; All rights reserved. 
;; 
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;; 
;;  - Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 
;;  - Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;;  - Neither the name of Drew Crampsie nor the names
;;    of its contributors may be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; ")AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
