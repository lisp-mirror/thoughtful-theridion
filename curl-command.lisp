(in-package :thoughtful-theridion)

(defun named-curl-cookie-jar-at (location name)
  (let*
    ((name (format
             nil "~a/thoughtful-theridion-cookie-jars/cookie-jar.~a.cookies"
             location name)))
    (ensure-directories-exist name)
    name))

(defun new-curl-cookie-jar-at (location)
  (let*
    ((timestamp
       (multiple-value-bind
         (seconds minutes hours days months years)
         (get-decoded-time)
         (format nil "~4,'0d-~2,'0d-~2,'0d/~2,'0d-~2,'0d-~2,'0d"
                 years months days
                 hours minutes seconds)))
     (directory (format nil "~a/thoughtful-theridion-cookie-jars/~a/"
                        location timestamp))
     (name
       (namestring
         (uiop:call-with-temporary-file
           #'identity
           :want-stream-p nil
           :keep t
           :directory directory
           :prefix "cookie-jar"
           :type "cookies"))))
    name))

(defun default-curl-cookie-jar-location ()
  (or (uiop:getenv "XDG_RUNTIME_DIR")
      (uiop:temporary-directory)))

(defun executables-in-path ()
  (loop
    for cmd in
    (mapcar
      'namestring
      (loop for dir in (cl-ppcre:split ":" (uiop:getenv "PATH"))
            append (directory (format nil "~a/*.*" dir))))
    collect (cl-ppcre:regex-replace ".*/" cmd "")))

(defun latest-numbered-version-in-path (template)
  (loop
    with max-num := -1
    with result := nil
    for exe in (executables-in-path)
    for number :=
    (ignore-errors
      (parse-integer
        (cl-ppcre:regex-replace ".*[^0-9]" exe "")))
    when (cl-ppcre:scan template exe)
    when number
    when (> number max-num)
    do (setf max-num number result exe)
    finally (return result)))

(defun curl-chrome-latest ()
  (latest-numbered-version-in-path "curl_chrome[0-9]*"))
(defun curl-firefox-latest ()
  (latest-numbered-version-in-path "curl_ff[0-9]*"))

(defclass http-fetcher-curl-command (http-fetcher)
  ((curl-command-policy :accessor curl-command-policy
                        :initarg :curl-command-policy
                        :initform
                        (lambda (&key url)
                          (declare (ignore url))
                          "curl"))
   (curl-flags-policy :accessor curl-flags-policy
                      :initarg :curl-flags-policy
                      :initform
                      (lambda (&key url)
                        (declare (ignore url))
                        (list)))
   (cookie-jar :accessor cookie-jar
               :initarg :cookie-jar
               :initform
               (new-curl-cookie-jar-at (default-curl-cookie-jar-location)))))

(defmethod navigate ((fetcher http-fetcher-curl-command)
                     (url string)
                     &rest
                     key-args
                     &key
                     (method :get) parameters
                     curl-extra-args &allow-other-keys)
  (when (and parameters (equal method :get))
    (setf url (cl-ppcre:regex-replace "[?].*" url ""))
    (setf url (format nil "~a?~{~a=~a~#[~:;&~]~}"
                      url
                      (loop for p in parameters
                            when p
                            collect
                            (quri:url-encode (or (car p) ""))
                            collect
                            (quri:url-encode (or (cdr p) ""))))))
  (setf url (funcall (url-encoder-policy fetcher) url :utf-8))
  (multiple-value-bind
    (content-bytes error-bytes exit-code)
    (uiop:run-program
      `(,(funcall (curl-command-policy fetcher)
                  :url url
                  :allow-other-keys t)
         ,url
         ,@(when (funcall (redirect-policy fetcher)
                          :old url
                          :allow-other-keys t)
             `("-L"))
         "-v"
         "--no-progress-meter"
         "--referer"
         ,(or
            (funcall (referer-policy fetcher)
                     :old (current-url fetcher)
                     :new url
                     :allow-other-keys t)
            "")
         "--max-time"
         ,(format
            nil "~a"
            (or
              (funcall (timeout-policy fetcher)
                       :url url
                       :allow-other-keys t)
              0))
         ,@ (let* ((ua (funcall (user-agent-policy fetcher)
                                :url url
                                :allow-other-keys t)))
              (when ua `("--user-agent" ,ua)))
         ,@ (let* ((ba (funcall (basic-auth-policy fetcher)
                                :url url
                                :allow-other-keys t)))
              (when ba `("--user" ,(format nil "~a:~a"
                                           (urlencode-unsafe (car ba) :utf-8)
                                           (urlencode-unsafe (cdr ba) :utf-8)))))
         ,@ (loop for h in (funcall (headers-policy fetcher)
                                    :fetcher fetcher
                                    :new url
                                    :old (current-url fetcher)
                                    :url url
                                    :allow-other-keys t)
                  collect "--header"
                  collect (format nil "~a: ~a"
                                  (car h) (cdr h)))
         ,@ (when (cookie-jar fetcher)
              `("--cookie" ,(cookie-jar fetcher)
                "--cookie-jar" ,(cookie-jar fetcher)))
         ,@ (funcall (curl-flags-policy fetcher)
                     :url url
                     :allow-other-keys t)
         ,@ curl-extra-args)
      :element-type '(unsigned-byte 8)
      :output #'alexandria:read-stream-content-into-byte-vector
      :error-output #'alexandria:read-stream-content-into-byte-vector
      :ignore-error-status t)
    (let*
      ((error-text (decode-guessed-encoding :content error-bytes))
       (error-lines (cl-ppcre:split
                      (format nil "[~{~a~}]+" (list #\Newline #\Return))
                      error-text))
       (complaint-line (when (> exit-code 0) (last error-lines)))
       (status-lines (loop for l in error-lines
                           when
                           (cl-ppcre:scan "^[<>*]" l)
                           collect
                           l))
       (last-dialog (loop
                      with request := nil
                      with response := nil
                      with ssl := nil
                      with ssl-fresh := nil
                      for l in status-lines
                      for requestp := (cl-ppcre:scan "^>" l)
                      for responsep := (cl-ppcre:scan "^<" l)
                      when (cl-ppcre:scan "^[*] *SSL connection( |$)" l)
                      do (setf ssl t ssl-fresh t)
                      when (and requestp response)
                      do (setf request nil response nil ssl ssl-fresh ssl-fresh nil)
                      when requestp do (push l request)
                      when responsep do (push l response)
                      finally (return (list (reverse request) (reverse response) ssl))))
       (request (first last-dialog))
       (response (second last-dialog))
       (ssl (third last-dialog))
       (requested-path (second (cl-ppcre:split " " (first request))))
       (requested-host (loop for l in request
                             when (cl-ppcre:scan "^host:" (string-downcase l))
                             return (cl-ppcre:regex-replace "^[^:]*: *" l "")))
       (requested-url (or (and requested-path requested-host
                               (format nil
                                       "~a://~a~a"
                                       (if ssl "https" "http")
                                       requested-host requested-path))
                          url))
       (server-headers
         (loop for l in (rest response)
               for lc := (cl-ppcre:regex-replace "^< *" l "")
               for hname := (cl-ppcre:regex-replace ":.*" lc "")
               for hvalue := (cl-ppcre:regex-replace "^[^:]*: *" lc "")
               for hkey := (intern (string-upcase hname)
                                   (find-package :keyword))
               collect (cons hkey hvalue)))
       (server-headers
         (append
           server-headers
           (when complaint-line
             `((:content-type . "text/plain; charset=utf-8")))))
       (status-code
         (if
           (> exit-code 0)
           503
           (ignore-errors
             (parse-integer
               (third
                 (cl-ppcre:split " " (first response)))))))
       (status-line (or
                      complaint-line
                      (cl-ppcre:regex-replace
                      "^< *[^ ]* +[0-9]* +" (first response) "")))
       (content-bytes
         (if complaint-line
           (coerce
             (concatenate 'vector content-bytes #(10 10) error-bytes)
             '(vector (unsigned-byte 8)))
           content-bytes)))
    (setf
      (current-content-bytes fetcher) content-bytes
      (current-status-code fetcher) status-code
      (current-status-line fetcher) status-line
      (current-url fetcher) (and requested-url (urlencode-unsafe requested-url :utf-8))
      (current-decoded-url fetcher) (maybe-urldecode (current-url fetcher) (list :utf-8 :latin-1))
      (current-intended-url fetcher) url
      (current-headers fetcher) server-headers)
    (parse-obtained-content fetcher)
    (when
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
                        :new (or location refresh requested-url url)
                        :content content-bytes
                        :allow-other-keys t)))
        (when policy-url (apply 'navigate fetcher policy-url key-args))))
    (current-content fetcher))))
