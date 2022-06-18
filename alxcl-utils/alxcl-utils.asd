(asdf:defsystem #:alxcl-utils
  :serial t
  :description "My personal collections of utils for Common Lisp"
  :author "Alexander Yavorovsky <alxndr.yav@gmail.com>"
  :license "MIT"

  :depends-on (#:flexi-streams
               #:usocket
               #:bordeaux-threads
               #:ironclad
               #:log4cl)

  :components ((:file "package")
               (:file "utils")
               (:file "network")
               (:file "bencoding"
                      :depends-on ("package" "utils"))
               (:file "debug")
               (:file "collections")))
