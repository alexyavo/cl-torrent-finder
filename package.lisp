;;;; package.lisp

(defpackage #:torrent-finder
  (:use #:cl)
  (:import-from #:flexi-streams
                #:octets-to-string
                #:string-to-octets)
  (:import-from #:usocket
                #:socket-connect
                #:socket-send
                #:socket-receive
                #:socket-close
                #:with-connected-socket))
