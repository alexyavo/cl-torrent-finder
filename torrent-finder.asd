;;;; torrent-finder.asd

(asdf:defsystem #:torrent-finder
  :description "DHT crawler"
  :author "Alexander Yavorovsky <alxndr.yav@gmail.com>"
  :license "MIT"
  :depends-on (#:alxcl-utils
               #:flexi-streams
               #:usocket
               #:ironclad
               #:log4cl)
  :serial t
  :components ((:file "package")
               (:file "torrent-dht")
               (:file "torrent-finder"
                      :depends-on ("torrent-dht"))))
