(defpackage #:torrent-dht
  (:use #:cl
        #:alxcl-utils)
  (:nicknames #:dht)

  (:import-from #:flexi-streams
                #:octets-to-string
                #:string-to-octets
                #:make-in-memory-input-stream
                #:make-flexi-stream
                #:peek-byte
                #:with-output-to-sequence)

  (:import-from #:usocket
                #:socket-connect
                #:socket-send
                #:socket-receive
                #:socket-close
                #:with-connected-socket)

  (:import-from #:ironclad
                #:byte-array-to-hex-string))

(defpackage #:torrent-finder
  (:use #:cl
        #:alxcl-utils))
