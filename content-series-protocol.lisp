(in-package :thoughtful-theridion)

; Content stream (series) definitions for storage-side
;
; String IDs are normally used, for DB/FS storage
; A series has its own unique ID
; In general we have content entries, each having its own unique EID
; Full EID starts with the series ID
; Parts of EID are separated with colons «:»
;
; A content entry has series ID, EID, source URL, possibly containing URL
; A containing URL is e.g. a web page URL where the image was obtained
; A content entry may be a web page, pure text, or a file

(defgeneric content-series-id (series)
            (:documentation
              "The identifier string of content series"))
(defgeneric content-series-eids (series)
            (:documentation
              "All the EIDs (entry IDs) known for the series"))
(defgeneric content-series-eids-among (series eids)
            (:documentation
              "All the known EIDs (entry IDs) among the given ones"))
(defgeneric content-series-latest-eids (series count)
            (:documentation
              "At most count latest EIDs from the series"))
(defgeneric content-series-eid-known-p (series eid)
            (:documentation
              "Whether the EID is present in the series"))
(defgeneric content-series-eid-url (series eid)
            (:documentation
              "The URL corresponding to the eid"))
(defgeneric content-series-eid-containing-url (series eid)
            (:documentation
              "Containing URL (if any) corresponding to the eid"))
(defgeneric content-series-contained-urls (series url)
            (:documentation
              "URLs corresponding to a containing URL"))
(defgeneric content-series-url-known-p (series url)
            (:documentation
              "Whether the URL is used by any entry in the series"))
(defgeneric content-series-containing-url-known-p (series url)
            (:documentation
              "Whether the containing URL is used by any entry in the series"))
(defgeneric content-series-add-webpage (series eid url 
                                               &key containing-url 
                                               &allow-other-keys)
            (:documentation
              "Add a webpage via a link"))
(defgeneric content-series-add-file (series eid url 
                                            &key containing-url 
                                            &allow-other-keys)
            (:documentation
              "Add a file via a link"))
(defgeneric content-series-add-plain-text (series 
                                            eid text
                                            &key url containing-url 
                                            &allow-other-keys)
            (:documentation
              "Add plain text"))
(defgeneric content-series-add-generated-file (series 
                                                eid path
                                                &key url containing-url 
                                                &allow-other-keys)
            (:documentation
              "Add a file from local path"))

(defclass content-series () ())

(defclass blackhole-content-series (content-series)
  ((id :type string :initarg :id :reader content-series-id)
   (fixed-entries :type list :initarg :entries 
                  :reader content-series-entries
                  :documentation "A list of plists of entries"))
  (:documentation "Series that accepts anything without changing content"))

(defmethod content-series-eids ((series blackhole-content-series))
  (loop for e in (content-series-entries series)
        collect (getf e :eid)))
(defmethod content-series-latest-eids ((series blackhole-content-series) n)
  (loop for e in (content-series-entries series)
        for k upfrom 1
        while (<= k n)
        collect (getf e :eid)))
(defmethod content-series-eid-known-p ((series blackhole-content-series) eid)
  (loop for e in (content-series-entries series)
        when (equal eid (getf e :eid))
        return t))
(defmethod content-series-eid-url ((series blackhole-content-series) eid)
  (loop for e in (content-series-entries series)
        when (equal eid (getf e :eid))
        return (getf e :url)))
(defmethod content-series-eid-containing-url ((series blackhole-content-series) 
                                              eid)
  (loop for e in (content-series-entries series)
        when (equal eid (getf e :eid))
        return (getf e :containing-url)))
(defmethod content-series-contained-urls ((series blackhole-content-series) 
                                              url)
  (loop for e in (content-series-entries series)
        when (equal url (getf e :url))
        collect (getf e :containing-url)))
(defmethod content-series-url-known-p ((series blackhole-content-series) url)
  (loop for e in (content-series-entries series)
        when (equal url (getf e :url))
        return t))
(defmethod
  content-series-containing-url-known-p ((series blackhole-content-series) 
                                         url)
  (loop for e in (content-series-entries series)
        when (equal url (getf e :containing-url))
        return t))
(defmethod
  content-series-add-webpage ((series blackhole-content-series) 
                              eid url 
                              &key containing-url
                              &allow-other-keys)
  (declare (ignore url) (ignore containing-url) (ignore eid)))
(defmethod 
  content-series-add-file ((series blackhole-content-series) 
                           eid url &key containing-url &allow-other-keys)
  (declare (ignore eid) (ignore url) (ignore containing-url)))
(defmethod content-series-add-plain-text ((series blackhole-content-series) 
                                            eid text
                                            &key url containing-url 
                                            &allow-other-keys)
  (declare (ignore url) (ignore containing-url) (ignore eid) (ignore text)))
(defmethod content-series-add-generated-file ((series blackhole-content-series) 
                                                eid path
                                                &key url containing-url 
                                                &allow-other-keys)
  (declare (ignore url) (ignore containing-url) (ignore eid) (ignore path)))
