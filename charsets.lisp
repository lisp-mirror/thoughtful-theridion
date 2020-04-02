(in-package :thoughtful-theridion)

(defparameter *babel-encodings* (make-hash-table :test 'equalp))
(maphash (lambda (k v)
           (setf (gethash (string-downcase k) *babel-encodings*) v))
         babel-encodings::*character-encodings*)
(maphash (lambda (k v)
           (loop
             with ks := (string-downcase k)
             for k-prime in
             (list
               (cl-ppcre:regex-replace-all "-" ks "")
               (cl-ppcre:regex-replace-all "[^a-z0-9]" ks "")
               (cl-ppcre:regex-replace-all "[^a-z0-9]" ks "-")
               (cl-ppcre:regex-replace-all
                 "([a-z])([0-9])|([0-9])([a-z])" ks "\\1\\3-\\2\\4"))
             do
             (setf (gethash k-prime *babel-encodings*) 
                   (or (gethash k-prime *babel-encodings*) v))))
         babel-encodings::*character-encodings*)

(defun content-type-encoding (content-type)
  (let* ((components (cl-ppcre:split " *(;|$) *" content-type))
         (options (rest components))
         (options-split (mapcar (lambda (x) (cl-ppcre:split "=" x)) options))
         (option-charset (find "charset" options-split
                               :key 'first :test 'equalp))
         (charset (second option-charset))
         (charset (cl-ppcre:regex-replace-all "\"" charset "")))
    charset))

(defun html-meta-encoding (data)
  (cond
    ((typep data '(array (unsigned-byte 8)))
     (html-meta-encoding (babel:octets-to-string data :encoding :latin-1)))
    ; Do not try to parse a huge file which might not even be HTML
    ((typep data 'string)
     (html-meta-encoding 
       (html5-parser:parse-html5 
         (subseq data 0 (min 65536 (length data))))))
    (t
      (ignore-errors
        (content-type-encoding
          (html5-parser:element-attribute
            (css-selectors:query1
              "meta[http-equiv='Content-Type']" data)
            "content"))))))

(defun extract-encoding (data content-type)
  (or 
    (content-type-encoding content-type)
    (html-meta-encoding data)
    "utf-8"))

(defun decode-guessed-encoding (&key content content-type)
  (when content
    (babel:octets-to-string
      content
      :encoding
      (or 
        (gethash (extract-encoding content content-type) *babel-encodings*)
        :utf-8)
      :errorp nil)))
