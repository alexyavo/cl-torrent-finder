;;;; package.lisp

(defpackage #:torrent-finder
  (:use #:cl)
  (:import-from #:flexi-streams
                #:octets-to-string
                #:string-to-octets
                #:make-in-memory-input-stream
                #:make-flexi-stream)

  (:import-from #:usocket
                #:socket-connect
                #:socket-send
                #:socket-receive
                #:socket-close
                #:with-connected-socket)

  (:import-from #:ironclad
                #:byte-array-to-hex-string))