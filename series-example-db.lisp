(defpackage :thoughtful-theridion/series-example-db
  (:use
    :common-lisp :thoughtful-theridion)
  (:export
    #:series-example-db-content-series
    ))

(in-package :thoughtful-theridion/series-example-db)

;; Helper stuff

(defun timestamp ()
  (local-time:format-timestring
    nil (local-time:now)
    :format `((:year 4) (:month 2) (:day 2) #\- (:hour 2) (:min 2) (:sec 2))))

(defun datestamp ()
  (local-time:format-timestring
    nil (local-time:now)
    :format `((:year 4) (:month 2) (:day 2))))

(defun timestamp-path ()
  (local-time:format-timestring
    nil (local-time:now)
    :format `((:year 4) #\/ (:month 2) #\/ (:day 2) #\/
                        (:hour 2) #\- (:min 2) #\- (:sec 2))))

(defun octet-array-to-base16-string (a)
  (let*
    (
      (strings (map 'list
        (lambda (x) (format nil "~16,2,'0r" x))
        a))
      (full-string (apply 'concatenate 'string strings))
    )
    full-string))

(defun hash (algo plaintext &key len)
  (let* (
         (keyword-package (find-package :keyword))
         (digest-key (intern (string-upcase algo) 
                                  keyword-package))
         (digester (ironclad:make-digest digest-key))
         (plainoctets (babel:string-to-octets
                       plaintext :encoding :utf-8))
         (hashedtext (ironclad:digest-sequence digester 
                                               plainoctets))
         )
        (subseq (octet-array-to-base16-string hashedtext) 0 len)))

;; The series class definition proper

#.(clsql-sys:restore-sql-reader-syntax-state)
#.(clsql-sys:locally-enable-sql-reader-syntax)

(defclass series-example-db-content-series (content-series)
  ((id :type string :initarg :id :reader content-series-id
       :initform (error "Specify an ID for the series"))
   (db-connection :initarg :db-connection
                  :initform (error "Specify a DB")
                  :reader content-series-db-connection
                  :documentation 
                  "DB connection e.g. (clsql:connect … :make-default nil)")
   (db-table :type string :initarg :db-table
             :initform (error "Specify a table name")
             :reader content-series-db-table
             :documentation "Table name in use")
   (storage-directory :type string :initarg :storage-directory
                      :initform (error "Specify a storage directory")
                      :reader content-series-storage-directory
                      :documentation 
                      "Directory for retrieved entries as files")
   (fetcher :initarg :fetcher :initform (make-instance 'http-fetcher)
            :reader content-series-fetcher
            :documentation "Fetcher to use for retrieving content from a URL")
   (html-textifier :initarg :html-textifier
                   :initform (make-instance 
                               'html-textifier-protocol-formatting-inspector)
                   :reader content-series-html-textifier
                   :documentation
                   "HTML textifier protocol to use when fetching web pages")
   (drop-duplicate-urls :type boolean :initarg :drop-duplicate-urls
                        :initform nil
                        :reader content-series-drop-duplicate-urls
                        :documentation
                        "Whether to ignore entry addition with a known URL")
   (drop-duplicate-eids :type boolean :initarg :drop-duplicate-eids
                        :initform t
                        :reader content-series-drop-duplicate-eids
                        :documentation
                        "Whether to ignore entry addition with a known EID")
   (eid-ordering :initarg :eid-ordering
                 :initform [desc [eid]]
                 :reader content-series-eid-ordering
                 :documentation
                 "DB order expression to sort for latest EIDs first"))
  (:documentation "Series indexing the retrieved content in a DB"))

;;      Column     |            Type             | Nullable |                 Default                 
;; ----------------+-----------------------------+----------+-----------------------------------------
;;  id             | integer                     | not null | nextval('web_streams_id_seq'::regclass)
;;  eid            | character varying           |          | 
;;  series         | character varying           |          | 
;;  url            | character varying           |          | 
;;  retrieved      | character varying           |          | 
;;  view           | character varying           |          | 
;;  kind           | character varying           |          | 
;;  timestamp      | character varying           |          | 
;;  fresh          | boolean                     |          | 
;;  ancient        | boolean                     |          | 
;;  deferred       | character varying           |          | 
;;  containing_url | character varying           |          | 
;;  read_timestamp | timestamp without time zone |          | 


(defmethod content-series-eids ((series series-example-db-content-series))
  (let* ((clsql:*default-database* (content-series-db-connection series)))
    (clsql:select
      [eid] :from (content-series-db-table series)
      :where [= [series] (content-series-id series)]
      :order-by (content-series-eid-ordering series))))
(defmethod content-series-eids-among ((series series-example-db-content-series)
                                      (eids list))
  (let* ((clsql:*default-database* (content-series-db-connection series)))
    (clsql:select
      [eid] :from (content-series-db-table series)
      :where [and
               [= [series] (content-series-id series)]
               [in [eid] (or eids (list ""))]
               ]
      :order-by (content-series-eid-ordering series))))
(defmethod content-series-latest-eids ((series series-example-db-content-series) n)
  (let* ((clsql:*default-database* (content-series-db-connection series)))
    (clsql:select
      [eid] :from (content-series-db-table series)
      :where [= [series] (content-series-id series)]
      :order-by (content-series-eid-ordering series)
      :limit n)))
(defmethod content-series-eid-known-p ((series series-example-db-content-series) eid)
  (let* ((clsql:*default-database* (content-series-db-connection series)))
    (clsql:select
      [eid] :from (content-series-db-table series)
      :where [and [= [series] (content-series-id series)]
                  [= [eid] eid]]
      :limit 1)))
(defmethod content-series-eid-url ((series series-example-db-content-series) eid)
  (let* ((clsql:*default-database* (content-series-db-connection series)))
    (caar
      (clsql:select
        [url] :from (content-series-db-table series)
        :where [and [= [series] (content-series-id series)]
                    [= [eid] eid]]
        :order-by [desc [id]]
        :limit 1))))
(defmethod content-series-eid-containing-url ((series series-example-db-content-series) 
                                              eid)
  (let* ((clsql:*default-database* (content-series-db-connection series)))
    (caar
      (clsql:select
        [containing-url]
        :from (content-series-db-table series)
        :where [and [= [series] (content-series-id series)]
                    [= [eid] eid]]
        :order-by [desc [id]]
        :limit 1))))
(defmethod content-series-eid-contained-urls ((series series-example-db-content-series) 
                                              url)
  (let* ((clsql:*default-database* (content-series-db-connection series)))
    (clsql:select
      [url]
      :from (content-series-db-table series)
      :where [and [= [series] (content-series-id series)]
                  [= [containing-url] url]]
      :order-by [id])))
(defmethod content-series-url-known-p ((series series-example-db-content-series) url)
  (let* ((clsql:*default-database* (content-series-db-connection series)))
    (caar
      (clsql:select
        [eid]
        :from (content-series-db-table series)
        :where [and [= [series] (content-series-id series)]
                    [= [url] url]]
        :order-by (content-series-eid-ordering series)
        :limit 1))))
(defmethod
  content-series-containing-url-known-p ((series series-example-db-content-series) 
                                         url)
  (let* ((clsql:*default-database* (content-series-db-connection series)))
    (caar
      (clsql:select
        [eid]
        :from (content-series-db-table series)
        :where [and [= [series] (content-series-id series)]
                    [= [url] url]]
        :order-by (content-series-eid-ordering series)
        :limit 1))))

;; Entry addition

;; General case for this content-series
(defmethod
  content-series-register-content ((series series-example-db-content-series)
                                   eid url placer
                                   &key
                                   (fresh t)
                                   (deferred nil)
                                   (containing-url nil)
                                   (kind "txt")
                                   (view-ext (if (equal kind "txt")
                                               "html.txt" kind))
                                   (retrieved-ext (if (equal kind "txt")
                                                    "html" kind)))
  (let* ((destination (format nil "~a/~a/" 
                              (content-series-storage-directory series)
                              (timestamp-path)))
         (base-eid (if (alexandria:starts-with-subseq 
                         (format nil "~a:" (content-series-id series))
                         eid)
                     (subseq eid
                             (1+ (length (content-series-id series))))
                     eid))
         (full-eid (format nil "~a:~a" (content-series-id series) base-eid))
         (basename 
           (format nil
                   "~a:~a"
                   (subseq base-eid 0 (min (length base-eid) 32))
                   (hash
                     "sha3"
                     (format nil "~a:~a" (local-time:now) url)
                     :len 32)))
         (retrieved (format nil "~a/~a.~a" destination basename retrieved-ext))
         (view (format nil "~a/~a.~a" destination basename view-ext)))
    (unless
      (or
        (and (content-series-drop-duplicate-eids series)
             (content-series-eid-known-p series eid))
        (and (content-series-drop-duplicate-eids series)
             (content-series-url-known-p series url)))
      (ensure-directories-exist destination)
      (funcall placer url destination basename retrieved-ext series)
      (let* ((clsql:*default-database* 
               (content-series-db-connection series)))
        (clsql:insert-records
          :into (content-series-db-table series)
          :av-pairs
          `(([eid] ,full-eid)
            ([series] ,series)
            ([url] ,url)
            ([retrieved] ,retrieved)
            ([view] ,view)
            ([kind] ,kind)
            ([timestamp] ,(timestamp))
            ([fresh] ,fresh)
            ([deferred] ,deferred)
            ([containing_url] ,containing-url)))))))

(defmethod
  content-series-add-webpage ((series series-example-db-content-series) 
                              eid url 
                              &key 
                              (fresh t)
                              (deferred nil)
                              (containing-url nil)
                              &allow-other-keys)
  (content-series-register-content
    series
    eid url
    (lambda (url destination basename ext series)
      (declare (ignore ext))
      (save-web-page
        url destination basename
        :fetcher (content-series-fetcher series)
        :drakma-args `(:redirect 10)
        :html-textifier-protocol (content-series-html-textifier series)
        :referrer containing-url))
    :fresh fresh :deferred deferred :containing-url containing-url
    :kind "txt" :view-ext "html.txt" :retrieved-ext "html"))
(defmethod 
  content-series-add-file ((series series-example-db-content-series) 
                           eid url &key 
                           containing-url
                           (filename (cl-ppcre:regex-replace ".*/" url ""))
                           (kind 
                             (string-downcase
                               (cl-ppcre:regex-replace ".*[.]" filename "")))
                           (retrieved-ext kind)
                           (view-ext retrieved-ext)
                           (fresh t)
                           (deferred nil)
                           &allow-other-keys)
  (content-series-register-content
    series
    eid url
    (lambda (url destination basename ext series)
      (when containing-url
        (save-web-page containing-url destination 
                       (format nil "~a.webpage" basename)
                       :fetcher (content-series-fetcher series)
                       :drakma-args `(:redirect 10)
                       :html-textifier-protocol 
                       (content-series-html-textifier series)))
      (navigate (content-series-fetcher series) url :redirect 10)
      (with-open-file (f (format nil "~a/~a.~a" destination basename ext)
                         :direction :output 
                         :element-type '(unsigned-byte 8))
        (write-sequence (current-content-bytes 
                          (content-series-fetcher series)) f))
      (with-open-file (f (format nil "~a/~a.~a.~a" 
                                 destination basename ext "url")
                         :direction :output )
        (format f "~a~%~a~%~a~%~a~%" 
                url (current-url (content-series-fetcher series))
                (or containing-url "") filename)))
    :fresh fresh :deferred deferred :containing-url containing-url
    :kind kind :view-ext view-ext :retrieved-ext retrieved-ext))
(defmethod content-series-add-plain-text ((series series-example-db-content-series) 
                                          eid text
                                          &key url containing-url 
                                          (fresh t)
                                          (deferred nil)
                                          &allow-other-keys)
  (let* ((kind "txt")
         (retrieved-ext kind)
         (view-ext retrieved-ext))
    (content-series-register-content
      series
      eid url
      (lambda (url destination basename ext series)
        (declare (ignore series))
        (with-open-file (f (format nil "~a/~a.~a" destination basename ext)
                           :direction :output)
          (write-sequence text f))
        (with-open-file (f (format nil "~a/~a.~a.~a" 
                                 destination basename ext "url")
                         :direction :output )
        (format f "~a~%~a~%~a~%" 
                (or url "") "" (or containing-url ""))))
      :fresh fresh :deferred deferred :containing-url containing-url
      :kind kind :view-ext view-ext :retrieved-ext retrieved-ext)))
(defmethod 
  content-series-add-generated-file ((series series-example-db-content-series) 
                                     eid path
                                     &key url containing-url 
                                     (filename 
                                       (cl-ppcre:regex-replace ".*/" path ""))
                                     (kind 
                                       (string-downcase
                                         (cl-ppcre:regex-replace 
                                           ".*[.]" filename "")))
                                     (retrieved-ext kind)
                                     (view-ext retrieved-ext)
                                     (fresh t)
                                     (deferred nil)
                                     &allow-other-keys)
  (content-series-register-content
    series
    eid url
    (lambda (url destination basename ext series)
      (declare (ignore series))
      (with-open-file (f (format nil "~a/~a.~a" destination basename ext)
                         :direction :output :element-type '(unsigned-byte 8))
        (with-open-file (src path :element-type '(unsigned-byte 8))
          (alexandria:copy-stream src f))
        (with-open-file (f (format nil "~a/~a.~a.~a" 
                                 destination basename ext "url")
                         :direction :output )
        (format f "~a~%~a~%~a~%~a~%" 
                url "" (or containing-url "") filename))))
    :fresh fresh :deferred deferred :containing-url containing-url
    :kind kind :view-ext view-ext :retrieved-ext retrieved-ext))
