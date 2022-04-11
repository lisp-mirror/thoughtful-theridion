(in-package :thoughtful-theridion)

(defvar *default-user-agent*
  "Thoughtful-Theridion/0.0 (magic matching strings: Gecko Firefox)")

(defclass http-fetcher ()
  ((cookie-jar :accessor cookie-jar
               :initform (make-instance 'drakma:cookie-jar))
   (pending-redirect :accessor pending-redirect :initform nil)
   (current-url :accessor current-url :initform nil)
   (current-decoded-url :accessor current-decoded-url :initform nil)
   (current-intended-url :accessor current-intended-url :initform nil)
   (current-status-code :accessor current-status-code :initform nil)
   (current-status-line :accessor current-status-line :initform nil)
   (current-headers :accessor current-headers :initform nil)
   (current-content :accessor current-content :initform nil)
   (current-content-bytes :accessor current-content-bytes :initform nil)
   (parsed-content :accessor parsed-content :initform nil)
   (parser-warnings :accessor parser-warnings :initform nil)
   (referer-policy :accessor referer-policy
                   :initform (lambda (&key old new)
                               (declare (ignorable old))
                               new)
                   :initarg :referer-policy)
   (redirect-policy :accessor redirect-policy
                    :initform
                    (lambda (&key code header old new)
                      (declare (ignorable code header old new))
                      nil)
                    :initarg :redirect-policy)
   (timeout-policy :accessor timeout-policy
                   :initarg :timeout-policy
                   :initform
                   (lambda (&key url) (declare (ignorable url)) 0))
   (basic-auth-policy :accessor basic-auth-policy
                      :initform (lambda (&key url)
                                  (declare (ignorable url)) nil)
                      :initarg :basic-auth-policy)
   (user-agent-policy :accessor user-agent
               :initform (lambda (&key url) (declare (ignorable url))
                           *default-user-agent*)
               :initarg :user-agent)
   (headers-policy :accessor headers-policy
                   :initform (lambda (&key)
                               `(("Cache-Control" . "none")))
                   :initarg :headers-policy)
   (url-preprocessing-policy :accessor url-preprocessing-policy
                             :initform (lambda (&key url) url)
                             :initarg :url-preprocessing-policy)
   (url-encoder-policy :accessor url-encoder-policy
                       :initform (lambda (url encoding)
                                   (declare (ignore encoding))
                                   (urlencode-unsafe url :utf8))
                       :initarg :url-encoder-policy)
   (decode-bytes-policy :accessor decode-bytes-policy
                        :initform (lambda (&key content content-type
                                                url headers)
                                    (declare (ignorable url headers))
                                    (let* ((vector-content
                                             (map
                                               '(vector (unsigned-byte 8))
                                               'identity
                                               content))
                                           (compressors
                                             (list 'chipz:gzip 'chipz:deflate
                                                   'chipz:zlib 'chipz:bzip2))
                                           (decompressed 
                                             (loop for c in compressors
                                                   for d :=
                                                   (ignore-errors
                                                     (chipz:decompress 
                                                       nil c vector-content))
                                                   when d return d)))
                                      (decode-guessed-encoding
                                        :content (or decompressed content)
                                        :content-type content-type)))
                        :initarg :decode-bytes-policy)
   (parsing-policy
     :accessor parsing-policy
     :initform (lambda (&key content-type content url
                             headers code)
                 (declare (ignorable url headers code))
                 (cond ((cl-ppcre:scan "^text/html(;|$)" content-type)
                        (ignore-errors (html5-parser:parse-html5 content)))
                       ((cl-ppcre:scan "^text/xhtml(;|$)" content-type)
                        (ignore-errors (html5-parser:parse-html5 content)))
                       ((cl-ppcre:scan "^application/xhtml([+]xml)?(;|$)" content-type)
                        (ignore-errors (html5-parser:parse-html5 content)))
                       ((cl-ppcre:scan "^(text|application)/(x-)?json(;|$)" 
                                       content-type)
                        (ignore-errors (cl-json:decode-json-from-string
                                         content)))
                       ((cl-ppcre:scan "^text/plain(;|$)" content-type)
                        content)
                       (t (format *trace-output*
                                  "Unrecognised content-type: ~a~%"
                                  content-type)
                          (let* ((document (html5-parser:make-document))
                                 (pre (html5-parser:make-element document "pre" nil))
                                 (text (html5-parser:make-text-node document content)))
                            (html5-parser:node-append-child pre text)
                            (html5-parser:node-append-child document pre)
                            document))))
     :initarg :parsing-policy)
   (proxy :accessor proxy :initform drakma:*default-http-proxy*
          :initarg :proxy)
   ))

(defgeneric parse-obtained-content (fetcher))

(defmethod parse-obtained-content ((fetcher http-fetcher))
  (setf (current-content fetcher)
        (funcall (decode-bytes-policy fetcher)
                 :allow-other-keys t
                 :url (current-url fetcher)
                 :content-type (cdr (assoc :content-type (current-headers fetcher)))
                 :headers (current-headers fetcher)
                 :content (current-content-bytes fetcher)))
  (if (current-content fetcher)
    (multiple-value-bind (parsed warnings)
      (funcall (parsing-policy fetcher)
               :allow-other-keys t
               :content (current-content fetcher)
               :url (current-url fetcher) :code (current-status-code fetcher)
               :content-type (cdr (assoc :content-type (current-headers fetcher)))
               :headers (current-headers fetcher))
      (setf (parsed-content fetcher) parsed
            (parser-warnings fetcher) warnings))
    (setf (parsed-content fetcher) nil
          (parser-warnings fetcher) "Content is binary")))

(defgeneric navigate (fetcher url &rest drakma-args))

(defmethod navigate ((fetcher http-fetcher) (url string) &rest drakma-args)
  (when (and (not (cl-ppcre:scan "^[a-z]+:" (string-downcase url)))
             (cl-ppcre:scan "^[a-z]+[%]3a" (string-downcase url)))
    (setf url (urldecode url :utf-8)))
  (setf url (cl-ppcre:regex-replace "#.*" url ""))
  (when (and 
          (getf drakma-args :parameters)
          (eq :get (or (getf drakma-args :method) :get)))
    (setf url (cl-ppcre:regex-replace "[?].*" url ""))
    (setf url (format nil "~a?~{~a=~a~#[~:;&~]~}"
                      url
                      (loop for p in (getf drakma-args :parameters)
                            when p
                            collect
                            (quri:url-encode (or (car p) ""))
                            collect
                            (quri:url-encode (or (cdr p) ""))
                            ))))
  (setf url (funcall (url-encoder-policy fetcher) url :utf-8))
  (multiple-value-bind
    (content
      status-code server-headers
      reply-url
      reply-stream reply-stream-needs-closing-p
      status-line)
    (handler-case
      (bordeaux-threads:with-timeout
        ((funcall (timeout-policy fetcher)
                  :url url :allow-other-keys t))
        (apply 'drakma:http-request
               (funcall (url-preprocessing-policy fetcher)
                        :url url :allow-other-keys t)
               (append
                 (let* ((additional-headers
                          (getf drakma-args :additional-headers)))
                   (list
                     :additional-headers
                     (append
                       additional-headers
                       (funcall (or (headers-policy fetcher) (constantly nil))
                                :fetcher fetcher :new url :old (current-url fetcher)
                                :url url
                                :allow-other-keys t)
                       (unless (assoc "Referer" additional-headers :test 'equalp)
                         `(("Referer" . ,(funcall (referer-policy fetcher)
                                                  :old (current-url fetcher)
                                                  :new url
                                                  :allow-other-keys t)))))))
                 drakma-args
                 (when (url-encoder-policy fetcher)
                   (list :url-encoder (url-encoder-policy fetcher)))
                 (list
                   :preserve-uri t
                   :cookie-jar (cookie-jar fetcher)
                   :user-agent (funcall (user-agent fetcher)
                                        :allow-other-keys t
                                        :url url)
                   :proxy (proxy fetcher)
                   :redirect nil
                   :force-binary t
                   ))))
      (error (e)
             (values
               (babel:string-to-octets (format nil "~a" e) :encoding :utf-8)
               503
               `((:content-type . "text/plain; charset=utf-8"))
               nil nil nil
               "Fetching failed"))
      (bordeaux-threads:timeout (e)
             (values
               (babel:string-to-octets (format nil "~a" e) :encoding :utf-8)
               504
               `((:content-type . "text/plain; charset=utf-8"))
               nil nil nil
               "Fetching failed: timeout"))
      )
    (when reply-stream-needs-closing-p (close reply-stream))
    (when (getf drakma-args :cookie-jar)
      (setf (cookie-jar fetcher) (getf drakma-args :cookie-jar)))
    (setf
      (current-content-bytes fetcher) content
      (current-status-code fetcher) status-code
      (current-status-line fetcher) status-line
      (current-url fetcher) (and reply-url (urlencode-unsafe (puri:render-uri reply-url nil) :latin-1))
      (current-decoded-url fetcher) (maybe-urldecode (current-url fetcher) (list :utf-8 :latin-1))
      (current-intended-url fetcher) url
      (current-headers fetcher) server-headers
      )
    (parse-obtained-content fetcher)
    (or
      (and (= status-code 401)
           (null (getf drakma-args :basic-authorization))
           (let* ((auth-data (funcall (basic-auth-policy fetcher)
                                      :url url
                                      :allow-other-keys t)))
             (when auth-data
               (apply 'navigate fetcher url
                      :basic-authorization auth-data
                      drakma-args))))
      (and
        (or (<= 300 status-code 399)
            (assoc :location server-headers)
            (assoc :refresh server-headers))
        (let* ((location (cdr (assoc :location server-headers)))
               (refresh (cl-ppcre:regex-replace
                          "^ *[0-9]+ *; *"
                          (cdr (assoc :refresh server-headers)) ""))
               (policy-url
                 (funcall (redirect-policy fetcher)
                          :status-code status-code
                          :header (cond (location :location)
                                (refresh :refresh)
                                (t nil))
                          :old url
                          :new (or location refresh reply-url url)
                          :content content
                          :allow-other-keys t)))
          (when policy-url (apply 'navigate fetcher policy-url drakma-args))))
      (current-content fetcher))))

