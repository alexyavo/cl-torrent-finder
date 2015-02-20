;;;; torrent-finder.asd

(asdf:defsystem #:torrent-finder
  :description "Describe torrent-finder here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:flexi-streams
               #:bencode
               #:usocket)
  :serial t
  :components ((:file "package")
               (:file "torrent-finder")))
