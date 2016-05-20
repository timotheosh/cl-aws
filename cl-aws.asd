;;;; cl-aws.asd

(asdf:defsystem #:cl-aws
  :description "Describe cl-aws here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:drakma
               #:cl-json)
  :serial t
  :components ((:file "package")
               (:file "cl-aws")))

