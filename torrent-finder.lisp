(in-package #:torrent-finder)

(defun decode-torrent-file (path)
  (with-open-file (torrent-file path :direction :input :element-type '(unsigned-byte 8))
    (bencoding/decode torrent-file)))

(defun torrent-file->info-hash (path)
  (with-open-file (torrent-file path :direction :input :element-type '(unsigned-byte 8))
    (let* ((decoded-file (bencoding/decode torrent-file))
           (info-encoded (bencoding/encode (gethash "info" decoded-file))))
      (ironclad:digest-sequence :sha1 (subseq info-encoded 0)))))
