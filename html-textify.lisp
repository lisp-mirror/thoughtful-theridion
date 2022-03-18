(in-package :thoughtful-theridion)

(defvar *base-url* nil)

(defun real-url (url)
  (cond ((null *base-url*) url)
        ((equal url "") *base-url*)
        ((cl-ppcre:scan "^[a-z]+:" url) url)
        ((cl-ppcre:scan "^//" url)
         (format nil "~a~a"
                 (cl-ppcre:regex-replace
                   "(^[a-z]+:)/.*$" *base-url* "\\1")
                 url))
        ((cl-ppcre:scan "^/" url)
         (format nil "~a~a"
                 (cl-ppcre:regex-replace
                   "(^[a-z]+:/*[^/]+)/.*$" *base-url* "\\1")
                 url))
        (t (format nil "~a/~a"
                 (cl-ppcre:regex-replace
                   "(^[a-z]+:/*[^/].*)/.*$" *base-url* "\\1")
                   url))))

(defclass html-textifier-protocol () ())

(defgeneric html-element-to-text (protocol element &key base-url))
(defgeneric html-element-to-text-dispatch (protocol type tag element))

(defmethod html-element-to-text
  ((protocol html-textifier-protocol) element &key base-url)
  (let* ((*base-url* (or base-url *base-url*)))
    (html-element-to-text-dispatch
      protocol (html5-parser:node-type element)
      (and (html5-parser:node-name element)
           (intern (string-upcase (html5-parser:node-name element)) :keyword))
      element)))

(defmethod html-element-to-text
  ((protocol html-textifier-protocol) (element null) &key base-url)
  (declare (ignorable base-url))
  "")

(defmethod html-element-to-text
  ((protocol html-textifier-protocol) (element http-fetcher) &key base-url)
  (html-element-to-text
    protocol (parsed-content element)
    :base-url (or base-url 
                  (and (current-url element) 
                       (format nil "~a" (current-url element)))
                  *base-url*)))

(defmethod html-element-to-text-dispatch
  ((protocol html-textifier-protocol) (type (eql :comment)) tag element)
  "")

(defmethod html-element-to-text-dispatch
  ((protocol html-textifier-protocol) (type (eql :document-type)) tag element)
  "")

(defparameter *whitespace-list* `(#\Newline #\Return #\Space #\Tab))

(defmethod html-element-to-text-dispatch
  ((protocol html-textifier-protocol) (type (eql :text)) tag element)
  (cl-ppcre:regex-replace-all
    (if *whitespace-list* (format nil "[~{~a~}]+" *whitespace-list*) " ")
    (html5-parser:node-value element)
    " "))

(defmethod html-element-to-text-dispatch
  ((protocol html-textifier-protocol) (type (eql :element)) tag element)
  (apply 'concatenate 'string
         (loop for c := (html5-parser:node-first-child element)
               then (html5-parser:node-next-sibling c)
               while c collect
               (html-element-to-text protocol c))))

(defmethod html-element-to-text-dispatch
  ((protocol html-textifier-protocol) (type (eql :element)) (tag (eql :pre))
                                      element)
  (let* ((*whitespace-list* nil))
    (call-next-method)))

(defmethod html-element-to-text-dispatch
  ((protocol html-textifier-protocol) (type (eql :document)) tag element)
  (apply 'concatenate 'string
         (loop for c := (html5-parser:node-first-child element)
               then (html5-parser:node-next-sibling c)
               while c collect
               (html-element-to-text protocol c))))

(defmethod html-element-to-text-dispatch
  ((protocol html-textifier-protocol)
   (type (eql :document-fragment)) tag element)
  (apply 'concatenate 'string
         (loop for c := (html5-parser:node-first-child element)
               then (html5-parser:node-next-sibling c)
               while c collect
               (html-element-to-text protocol c))))

(defmethod html-element-to-text-dispatch
  ((protocol html-textifier-protocol)
   (type (eql :element)) (tag (eql :br)) element)
  (format nil "~%"))

(defmethod html-element-to-text-dispatch
  ((protocol html-textifier-protocol)
   (type (eql :element)) (tag (eql :script)) element)
  "")

(defmethod html-element-to-text-dispatch
  ((protocol html-textifier-protocol)
   (type (eql :element)) (tag (eql :style)) element)
  "")

(defmethod html-element-to-text-dispatch
  ((protocol html-textifier-protocol)
   (type (eql :element)) (tag (eql :div)) element)
  (format nil "~%~a~%" (call-next-method)))

(defmethod html-element-to-text-dispatch
  ((protocol html-textifier-protocol)
   (type (eql :element)) (tag (eql :p)) element)
  (format nil "~%~a~%" (call-next-method)))

(defmethod html-element-to-text-dispatch
  ((protocol html-textifier-protocol)
   (type (eql :element)) (tag (eql :li)) element)
  (format nil "~%~a~%" (call-next-method)))

(defmethod html-element-to-text-dispatch
  ((protocol html-textifier-protocol)
   (type (eql :element)) (tag (eql :ol)) element)
  (format nil "~%~a~%" (call-next-method)))

(defmethod html-element-to-text-dispatch
  ((protocol html-textifier-protocol)
   (type (eql :element)) (tag (eql :ul)) element)
  (format nil "~%~a~%" (call-next-method)))

(defmethod html-element-to-text-dispatch
  ((protocol html-textifier-protocol)
   (type (eql :element)) (tag (eql :tr)) element)
  (format nil "~%~a~%" (call-next-method)))

(defmethod html-element-to-text-dispatch
  ((protocol html-textifier-protocol)
   (type (eql :element)) (tag (eql :table)) element)
  (format nil "~%~a~%" (call-next-method)))

(defclass show-urls-mixin (html-textifier-protocol) ())

(defmethod html-element-to-text-dispatch
  ((protocol show-urls-mixin) (type (eql :element)) tag element)
  (let* ((attributes '("href" "src" "data-expanded-url" "data-url"))
         (urls (loop for a in attributes
                     for v := (html5-parser:element-attribute element a)
                     when v collect (real-url v)))
         (seen-urls (make-hash-table :test 'equal))
         (uniq-urls nil)
         (text (call-next-method)))
    (loop with stack := urls
          while stack
          for u := (pop stack)
          when u
          unless (gethash u seen-urls)
          do 
          (progn 
            (setf (gethash u seen-urls) t)
            (push u uniq-urls)
            (push (ignore-errors (urldecode u :utf-8)) stack)
            (when (cl-ppcre:scan ".+[/?&=]https?:[/][/]" u)
              (push (cl-ppcre:regex-replace-all
                      "^.+[/?&=](https?:[/][/])" u "\\1")
                    stack))
            (when (cl-ppcre:scan "^https://([^/]+[.])?twitter.com/" u)
              (push (cl-ppcre:regex-replace
                      "/([^/]+[.])?twitter.com/" u "/twitit.gq/")
                    stack)
              (push (cl-ppcre:regex-replace
                      "/([^/]+[.])?twitter.com/" u "/twiiit.com/")
                    stack)
              )
            (when (cl-ppcre:scan "^https://[^/]+/[^/]+/status/[0-9]+($|#)" u)
              (push (cl-ppcre:regex-replace "^https://[^/]+/" u "https://twitter.com/") 
                    stack))
            (loop with query-params := (second (cl-ppcre:split "[?]" u))
                  with params := (cl-ppcre:split "[&]" u)
                  for p in params
                  for v := (second (cl-ppcre:split "[=]" p))
                  for dv := (ignore-errors (urldecode v :utf-8))
                  when dv
                  when (cl-ppcre:scan "^https?://" dv)
                  do (push dv stack))))
    (if urls (format nil "{ ~a }~{→[ ~a ]~}" text (reverse uniq-urls)) text)))

(defmethod html-element-to-text-dispatch
  ((protocol show-urls-mixin) (type (eql :element)) (tag (eql :meta)) element)
  (let ((attributes nil))
    (html5-parser:element-map-attributes
      (lambda (k ns v) (push (list k v ns) attributes))
      element)
    (format nil "[meta]:{ ~{~{~a: { ~a }~} ~}}"
            (loop for x in attributes
                  for k := (first x)
                  for v := (second x)
                  for ns := (third x)
                  for ve := ""
                  when (and 
                         (equalp k "content")
                         (equalp 
                           (html5-parser:element-attribute
                             element "http-equiv") "refresh")
                         (cl-ppcre:scan "^ *[0-9]+ *; *" v))
                  do (setf 
                       ve (format
                            nil "~a →[ ~a ]" ve
                            (real-url
                              (cl-ppcre:regex-replace
                                "^ *[0-9]+ *; *((URL|url)=)?" v ""))))
                  collect (list 
                            (format nil "~a:~a" (or ns "") k)
                            (concatenate 'string v ve))))))

(defclass show-ui-texts-mixin (html-textifier-protocol) ())

(defmethod html-element-to-text-dispatch
  ((protocol show-ui-texts-mixin) (type (eql :element)) tag element)
  (let* ((attributes '( "alt" "title"))
         (names (loop for a in attributes
                     for v := (html5-parser:element-attribute element a)
                     when v collect v))
         (text (call-next-method)))
    (if names (format nil "~{ ⚓[ ~a ]~}~a" names text) text)))

(defclass show-id-texts-mixin (html-textifier-protocol) ())

(defmethod html-element-to-text-dispatch
  ((protocol show-id-texts-mixin) (type (eql :element)) tag element)
  (let* ((attributes '("name" "id"))
         (names (loop for a in attributes
                     for v := (html5-parser:element-attribute element a)
                     when v collect v))
         (text (call-next-method)))
    (if names (format nil "~{ ⚓[ ~a ]~}~a" names text) text)))

(defclass show-formatting-mixin (html-textifier-protocol) ())

(defmethod html-element-to-text-dispatch
  ((protocol show-formatting-mixin)
   (type (eql :element)) (tag (eql :s)) element)
  (format nil "{-- ~a --}" (call-next-method)))

(defmethod html-element-to-text-dispatch
  ((protocol show-formatting-mixin)
   (type (eql :element)) (tag (eql :strike)) element)
  (format nil "{-- ~a --}" (call-next-method)))

(defmethod html-element-to-text-dispatch
  ((protocol show-formatting-mixin)
   (type (eql :element)) (tag (eql :i)) element)
  (format nil "{// ~a //}" (call-next-method)))

(defmethod html-element-to-text-dispatch
  ((protocol show-formatting-mixin)
   (type (eql :element)) (tag (eql :b)) element)
  (format nil "{** ~a **}" (call-next-method)))

(defmethod html-element-to-text-dispatch
  ((protocol show-formatting-mixin)
   (type (eql :element)) (tag (eql :u)) element)
  (format nil "{__ ~a __}" (call-next-method)))

(defmethod html-element-to-text-dispatch
  ((protocol show-formatting-mixin)
   (type (eql :element)) (tag (eql :sup)) element)
  (format nil "{↑↑ ~a ↑↑}" (call-next-method)))

(defmethod html-element-to-text-dispatch
  ((protocol show-formatting-mixin)
   (type (eql :element)) (tag (eql :sub)) element)
  (format nil "{↓↓ ~a ↓↓}" (call-next-method)))

(defmethod html-element-to-text-dispatch
  ((protocol show-formatting-mixin)
   (type (eql :element)) (tag (eql :li)) element)
  (format nil "{>>> ~a <<<}" (call-next-method)))

(defmethod html-element-to-text-dispatch
  ((protocol show-formatting-mixin)
   (type (eql :element)) (tag (eql :blockquote)) element)
  (format nil ">[ ~a ]" (call-next-method)))

(defclass recur-on-noscript-mixin (html-textifier-protocol) ())

(defmethod html-element-to-text-dispatch
  ((protocol recur-on-noscript-mixin)
   (type (eql :element)) (tag (eql :noscript))
   element)
  (html-element-to-text
    protocol
    (when (html5-parser:node-first-child element)
      (html5-parser:parse-html5-fragment
        (html5-parser:node-value
          (html5-parser:node-first-child
            element))))))

(defclass html-textifier-protocol-formatting-inspector
  (recur-on-noscript-mixin
    show-formatting-mixin show-ui-texts-mixin show-urls-mixin
    html-textifier-protocol) ())

(defclass html-textifier-protocol-inspector
  (show-id-texts-mixin
    html-textifier-protocol-formatting-inspector) ())

(defun html-inner-text (element)
  (html-element-to-text
    (make-instance 'html-textifier-protocol)
    element))
