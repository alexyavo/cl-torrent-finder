;;;; torrent-finder.asd

(asdf:defsystem #:torrent-finder
  :description "DHT crawler"
  :author "Alexander Yavorovsky <alxndr.yav@gmail.com>"
  :license "MIT"
  :depends-on (#:utils
               #:flexi-streams
               #:bencode
               #:usocket
               #:ironclad
               #:log4cl)
  :serial t
  :components ((:file "package")
               (:file "torrent-finder")))
