#! /bin/sh

cd "$(dirname "$0")"

./thoughtful-theridion.bin --non-interactive --eval "
(let* 
((buffer (make-array ${2:-(expt 10 7)} :element-type (quote (unsigned-byte 8))))
 (fetcher (make-instance 'thoughtful-theridion:http-fetcher))
 (n (read-sequence buffer *standard-input*))
 (buffer (subseq buffer 0 n)))
(setf (thoughtful-theridion:current-content-bytes fetcher) buffer
      (thoughtful-theridion:current-headers fetcher) (quote ((:content-type . \"text/html\")))
      (thoughtful-theridion:current-url fetcher) \"$1\"
      (thoughtful-theridion:current-status-code fetcher) 200
      (thoughtful-theridion::decode-bytes-policy fetcher) 
      (lambda (&key content) 
         (thoughtful-theridion::decode-guessed-encoding 
            :content (map '(vector (unsigned-byte 8)) 'identity content) 
            :content-type \"text/html\"))
)
(thoughtful-theridion:parse-obtained-content fetcher)
(princ 
  (or (ignore-errors
         (thoughtful-theridion:html-element-to-text
            (make-instance (quote thoughtful-theridion:html-textifier-protocol-inspector))
            fetcher))
      \"\")))"
