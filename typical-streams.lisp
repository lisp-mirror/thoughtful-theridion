(in-package :thoughtful-theridion)

(defclass web-post-series () 
  ((fetcher :type http-fetcher :initarg :fetcher
            :accessor web-post-series-fetcher
            :initform (make-instance 'http-fetcher)))
  (:documentation "An interface name for enumerable series of web posts"))

(defclass web-post-series-entry ()
  ((eid :type string :initarg :eid
        :accessor web-post-series-entry-eid
        :initform (error "Specify URL"))
   (url :type (or string null) :initarg :url
        :accessor web-post-series-entry-url
        :initform nil)
   (containing-url 
     :type (or string null) 
     :initarg :containing-url
     :accessor web-post-series-entry-containing-url
     :initform nil)))

(defclass web-post-series-webpage (web-post-series-entry) ())
(defclass web-post-series-file (web-post-series-entry)
  ((generated-content-file 
     :type (or string null)
     :initarg :content-file
     :accessor web-post-series-file-content
     :initform nil)))
(defclass web-post-series-text (web-post-series-entry)
  ((text-content :type string :initarg :text-content
                 :accessor web-post-series-text-content
                 :initform (error "Specify text content"))))

(defgeneric web-post-entry-store (entry series
                                        &rest args &key
                                        &allow-other-keys))

(defmethod web-post-entry-store ((entry web-post-series-webpage)
                                 (series content-series)
                                 &rest args
                                 &key &allow-other-keys)
  (apply 'content-series-add-webpage
    series (web-post-series-entry-eid entry)
    (web-post-series-entry-url entry)
    :containing-url (web-post-series-entry-containing-url entry)
    args))
(defmethod web-post-entry-store ((entry web-post-series-file)
                                 (series content-series)
                                 &rest args
                                 &key &allow-other-keys)
  (if (web-post-series-file-content entry)
    (apply 'content-series-add-generated-file
           series (web-post-series-entry-eid entry)
           (web-post-series-file-content entry)
           :url (web-post-series-entry-url entry)
           :containing-url (web-post-series-entry-containing-url entry)
           args)
    (apply 'content-series-add-file
           series (web-post-series-entry-eid entry)
           (web-post-series-entry-url entry)
           :containing-url (web-post-series-entry-containing-url entry)
           args)))
(defmethod web-post-entry-store ((entry web-post-series-text)
                                 (series content-series)
                                 &rest args
                                 &key &allow-other-keys)
  (apply 'content-series-add-plain-text
         series (web-post-series-entry-eid entry)
         (web-post-series-text-content entry)
         :containing-url (web-post-series-entry-containing-url entry)
         args))

(defclass paginated-web-posts (web-post-series)
  ((base-url :type string
             :initarg :base-url
             :accessor web-post-series-base-url
             :initform (error "Base URL must be specified"))
   (eid-extractor :type function
                  :accessor web-post-series-eid-extractor
                  :initarg :eid-extractor
                  :initform (lambda (&key)))))

(defgeneric paginated-web-posts-previous-page (state)
            (:documentation "Go to the previous page of posts if any"))
(defgeneric paginated-web-posts-page-map-entries (state fn)
            (:documentation
              "Call fn with each entry for the page"))
(defmethod paginated-web-posts-page-entries ((state paginated-web-posts))
           (let ((res nil))
             (paginated-web-posts-page-map-entries
               state
               (lambda (entry &key &allow-other-keys) (push entry res)))
             (reverse res)))
(defgeneric paginated-web-posts-page-entries (state)
            (:documentation "Return the entries of the page"))
(defmethod web-post-series-fetch-new ((state paginated-web-posts)
                                      (target content-series)
                                      &key
                                      (max-entries 1e9)
                                      (max-new-entries 1e9)
                                      (min-entries 0)
                                      (dry-run nil)
                                      (storage-arguments nil))
  (thoughtful-theridion/try:ok
    (loop with all-seen := 0
          with new-seen := 0
          with has-new := t
          for state := state then 
          (paginated-web-posts-previous-page state)
          while (or
                  (< all-seen min-entries)
                  (and
                    has-new
                    (< all-seen max-entries)
                    (< new-seen max-new-entries)))
          do
          (progn
            (setf has-new nil)
            (paginated-web-posts-page-map-entries
              state
              (lambda (entry)
                (incf all-seen)
                (thoughtful-theridion/try:ok)
                (unless (content-series-eid-known-p
                          target
                          (web-post-series-entry-eid entry))
                  (incf new-seen)
                  (setf has-new t)
                  (if dry-run
                    (try:is
                      (list
                        (type-of entry)
                        (web-post-series-entry-eid entry)
                        (web-post-series-entry-url entry)))
                    (apply 'web-post-entry-store 
                           entry target storage-arguments)))))))))

(defclass paginated-web-posts-css-selectable (paginated-web-posts)
  ((selector-prev :type (or string function null) 
                  :initarg :selector-prev 
                  :accessor paginated-web-posts-css-selector-prev
                  :initform nil)
   (selector-entry :type (or string function) :initarg :selector-prev 
                  :accessor paginated-web-posts-css-selector-entry
                  :initform (error "Specify entry selector"))
   (entry-link-extractor :type function :initarg :entry-link-extractor
                         :accessor paginated-web-posts-entry-link-extractor
                         :initform (lambda (link &key &allow-other-keys)
                                     (html5-parser:element-attribute
                                       link "href")))
   (prev-link-extractor :type function :initarg :prev-link-extractor
                         :accessor paginated-web-posts-prev-link-extractor
                         :initform (lambda (link &key &allow-other-keys)
                                     (html5-parser:element-attribute
                                       link "href")))
   (entry-constructor :type function :initarg :entry-constructor
                      :accessor paginated-web-posts-entry-constructor
                      :initform
                      (lambda (&key 
                                eid
                                url containing-url
                                entry-element
                                &allow-other-keys)
                        (make-instance 'web-post-series-webpage
                                       :eid eid :url url
                                       :containing-url containing-url)))))

(defmethod 
  paginated-web-posts-previous-page 
  ((state paginated-web-posts-css-selectable))
  (let* ((parsed-page
           (parsed-content
             (web-post-series-fetcher state)))
         (selector (paginated-web-posts-css-selector-prev state))
         (prev-element
           (etypecase selector
             (null nil)
             (function (funcall selector parsed-page))
             (string (css-selectors:query1 selector parsed-page))))
         (prev-url
           (funcall (paginated-web-posts-prev-link-extractor state)
                    prev-element))
         (

