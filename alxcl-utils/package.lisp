(defpackage #:alxcl-utils
  (:use #:cl)

  (:export :hexdump
           :hexdump-to-stream

           :bencoding/encode
           :bencoding/decode

           :ipv4-vec->string
           :endpoint-address
           :host
           :port

           :read-uint16
           :read-octets
           :iterate-pair
           :with-gensyms
           :ipv4-vec->endpoint-address
           :tcp/read-data
           :tcp/server
           :socket-has-data?
           :make-binary-buffer
           :make-address
           :while
           :udp/send-receive

           :add-item
           :remove-item
           :get-items
           :set-collection
           :make-set-collection
           :items-count
           :add-items
           :pop-item)

  (:import-from #:flexi-streams
                #:flexi-stream
                #:peek-byte
                #:make-flexi-stream
                #:make-external-format
                #:make-in-memory-input-stream
                #:with-output-to-sequence
                #:with-input-from-sequence
                #:string-to-octets
                #:octets-to-string))
