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
(loop
  with ht := (make-hash-table :test 'equal)
  for el in 
  (css-selectors:query 
    \"img\"
    (thoughtful-theridion:parsed-content fetcher))
  for src :=
  (html5-parser:element-attribute el \"src\")
  when (and src (not (gethash src ht))) 
  do
  (progn 
    (format t \"~a~%\" src)
    (setf (gethash src ht) t))
  ))"

