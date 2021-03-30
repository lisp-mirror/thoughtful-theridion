(in-package :thoughtful-theridion)

(defgeneric
  urldecode (object encoding)
  (:method (object encoding)
           (babel:octets-to-string
             (urldecode object nil)
             :encoding
             (gethash (string encoding) *babel-encodings* :utf-8)))
  (:method ((object null) encoding) nil)
  (:method ((object string) encoding)
           (urldecode
             (babel:string-to-octets
               object
               :encoding
               (gethash (string encoding) *babel-encodings* :utf-8))
             encoding))
  (:method (object (encoding (eql t)))
           (urldecode object :utf-8))
  (:method ((object vector) (encoding null))
           (let ((l (length object))
                 (i 0))
             (flet ((next () (prog1 (when (< i l) (elt object i)) (incf i))))
               (coerce
                 (loop for code := (next)
                       while code
                       if (= code 37)
                       collect
                       (parse-integer
                         (map 'string 'code-char
                              (remove nil (list (next) (next))))
                         :radix 16)
                       else
                       collect code)
                 '(vector (unsigned-byte 8)))))))

(defmethod urldecode ((object puri:uri) encoding)
  (urldecode (format nil "~a" object) encoding))

(defmethod maybe-urldecode (object encodings)
  (loop for e in encodings
        for d := (ignore-errors (urldecode object e))
        when d return d
        finally (return object)))

(defparameter *base16-digits*
  (map 'vector 'char-code "0123456789ABCDEF"))

(defgeneric
  urlencode (object encoding)
  (:method (object (encoding t))
           (babel:octets-to-string
             (urlencode object nil)
             :encoding
             (gethash (string encoding) *babel-encodings* :utf-8)))
  (:method ((object string) encoding)
           (urlencode
             (babel:string-to-octets
               object
               :encoding
               (gethash (string encoding) *babel-encodings* :utf-8))
             encoding))
  (:method (object (encoding (eql t)))
           (urlencode object :utf-8))
  (:method ((object vector) (encoding null))
           (coerce
             (loop for code across object
                   collect 37
                   collect (elt *base16-digits* (truncate code 16))
                   collect (elt *base16-digits* (mod code 16)))
             '(vector (unsigned-byte 8)))))

(defgeneric
  urlencode-unsafe (object encoding)
  (:method (object encoding)
           (coerce
             (reduce 'append
                     (map 'list
                          (lambda (x)
                            (cond
                              ((char<= #\a x #\z) (list x))
                              ((char<= #\A x #\Z) (list x))
                              ((char<= #\0 x #\9) (list x))
                              ((position x ":/?&_-.=*,;[]%+") (list x))
                              (t (coerce (urlencode (string x) encoding) 'list))))
                          object))
             'string)))
