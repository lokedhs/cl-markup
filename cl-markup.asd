(asdf:defsystem #:cl-markup
  :description "Describe cl-markup here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (:alexandria
               :cl-ppcre
               :string-case)
  :components ((:file "package")
               (:file "misc")
               (:file "url")
               (:file "cl-markup")))
