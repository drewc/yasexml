#-asdf3 (error "YASEXML requires ASDF 3 or later with \"asdf-package-system\"")
#+quicklisp (ql:quickload "asdf-package-system")

(asdf:register-system-packages 
 :cxml '(:sax :cxml))		       

(asdf:defsystem :yasexml
  :description "YASEXML : Yet Another Symbolic Expression eXtensible Markup Language"
  :class :package-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on (:yasexml/yasexml))



			       
