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
                #:byte-array-to-hex-string)
  (:export
   #:ping
   #:find-node
   #:get-peers
   #:announce-peer

   #:get-peers-response
   #:peer-nodes
   #:peer-values
   #:token

   #:error-response
   #:error-code
   #:error-msg

   #:node-info
   #:node-id
   #:node-address

   #:*query-id*
   #:*node-id*
   #:+well-known-nodes+
   #:get-torrent-peers))

(defpackage #:torrent-finder
  (:use #:cl
        #:alxcl-utils)

  (:import-from #:usocket
                #:socket-connect
                #:socket-send
                #:socket-receive
                #:socket-close
                #:socket-stream
                #:wait-for-input
                #:with-connected-socket)

  (:import-from #:flexi-streams
                #:octets-to-string
                #:string-to-octets
                #:with-output-to-sequence)

  (:import-from #:ironclad
                #:octets-to-integer))
