(in-package :thoughtful-theridion)

;;; Stretch goal: implement https://github.com/mozilla/readability/ algorithm

(defun html-score-content (protocol element cache)
  "Evaluate content density in ELEMENT according to PROTOCOL.
Use CACHE as a hash-table of precomputed results. This function allows CACHE
reuse with the same PROTOCOL.

Return values: total score, list of scores by level (own, children, etc.),
text that the parent node may consider as its own for scoring, the data to use
as readable content in case this element is picked as a part of main content.
"
  (let* ((cached (gethash element cache))
         (data (or cached
                   (multiple-value-list
                     (html-score-content-dispatch
                       protocol (html5-parser:node-type element)
                       (and (html5-parser:node-name element)
                            (intern (string-upcase (html5-parser:node-name element))
                                    :keyword))
                       element cache)))))
    (unless cached (setf (gethash element cache) data))
    (apply 'values data)))

(defgeneric html-score-content-dispatch (protocol type tag element cache))

(defmacro with-html-score-content-protocol ((protocol-type) (protocol element &optional (cache (gensym)))
                                            &body body)
  `(symbol-macrolet ((html-score-content-protocol-type ,protocol-type)
                     (html-score-content-protocol ,protocol)
                     (html-score-content-element ,element)
                     (html-score-content-cache ,cache))
                    ,@body))

(defmacro def-html-score-content ((type tags) &body body &environment env)
  (let* ((type-var (gensym))
         (tag-var (if (or (null (first tags)) (keywordp (first tags)))
                    (gensym) (first tags)))
         (tag-options (remove-if (lambda (x) (and x (not (keywordp x)))) tags)))
    `(progn
       ,@(loop for tag-option in tag-options collect
               `(defmethod html-score-content-dispatch
                  ((,(macroexpand-1 'html-score-content-protocol env)
                     ,(macroexpand-1 'html-score-content-protocol-type env))
                   (,type-var (eql ,type))
                   (,tag-var (eql ,tag-option))
                   (,(macroexpand-1 'html-score-content-element env) t)
                   (,(macroexpand-1 'html-score-content-cache env) hash-table))
                  ,@body)))))

(defclass html-text-score-protocol ()
  ())

(defgeneric html-score-text (protocol text))

(defvar *unicode-property-cache* (make-hash-table :test 'equal))

(defun unicode-property-counts (text &rest properties)
  (apply 'values
         (loop for p in properties
               collect (count-if
                         (lambda (x)
                           (multiple-value-bind
                             (value presentp)
                             (gethash (list x p) *unicode-property-cache*)
                             (if presentp value
                               (setf
                                 (gethash (list x p) *unicode-property-cache*)
                                 (cl-unicode:has-property x p)))))
                         text))))

(defmethod html-score-text ((protocol html-text-score-protocol) (text null)) 0)
(defmethod html-score-text ((protocol html-text-score-protocol) (text string))
  (multiple-value-bind
    (letters sentences punctuation)
    (unicode-property-counts
      text
      "Alphabetic" "Sentence_Terminal" "Punctuation")
    (let* ((commas (- punctuation sentences))
           (length-points (min 3 (truncate letters 90)))
           (sentence-points (min 20 (truncate sentences 5))))
      (if (< letters 25) 0 (+ commas length-points sentence-points 1)))))

(defmethod html-content-score-summarize ((protocol html-text-score-protocol)
                                         (scores list))
  (case (length scores)
    (0 0)
    (1 (first scores))
    (t (+ (first scores) (/ (second scores) 2)
          (loop for x in (cddr scores)
                for k upfrom 1
                sum (/ x 3 k))))))

(defun list-binary-+ (&optional a b)
  (cond ((null a) b)
        ((null b) a)
        (t (cons (+ (first a) (first b))
                 (list-binary-+ (rest a) (rest b))))))

(defun list-+ (&rest lists)
  (reduce #'list-binary-+ lists :initial-value '()))

(defun collect-child-scoring-data (protocol element cache)
  (let* ((top-level-scores (list))
         (texts (list))
         (content-children (list)))
    (loop for c := (html5-parser:node-first-child element)
          then (html5-parser:node-next-sibling c)
          while c do
          (multiple-value-bind
            (score level-scores text content)
            (html-score-content protocol c cache)
            (declare (ignorable score))
            (push level-scores top-level-scores)
            (when text (push text texts))
            (when content (push content content-children))))
    (let* ((own-text (apply 'concatenate 'string (reverse texts)))
           (sub-scores (apply 'list-+ top-level-scores))
           (content (cons element (reverse content-children))))
      (values sub-scores own-text content))))

(defmethod html-score-content-dispatch ((protocol html-text-score-protocol)
                                        type tag element cache)
  (multiple-value-bind
    (sub-scores own-text content)
    (collect-child-scoring-data protocol element cache)
    (let* ((own-direct-score (html-score-text protocol own-text))
           (all-level-scores (cons own-direct-score sub-scores))
           (score (html-content-score-summarize protocol all-level-scores)))
      (values score all-level-scores nil content))))

(with-html-score-content-protocol
  (html-text-score-protocol) (protocol element cache)
  (def-html-score-content (:comment (nil)) (values 0 () "" nil))
  (def-html-score-content (:document-type (nil)) (values 0 () "" nil))
  (def-html-score-content (:text (nil))
    (let* ((text 
             (cl-ppcre:regex-replace-all
               (if *whitespace-list*
                 (format nil "[~{~a~}]+" *whitespace-list*) " ")
               (html5-parser:node-value element)
               " "))
           (score (html-score-text protocol text)))
      (values score (list) text element)))
  (def-html-score-content (:document (nil)) (call-next-method))
  (def-html-score-content (:document-fragment (nil)) (call-next-method))
  (def-html-score-content (:element (:br))
      (values -1 (list) " " element))
  (def-html-score-content (:element (:style :script :head))
      (values -1 (list) nil nil))
  (def-html-score-content (:element (:a))
    (multiple-value-bind
      (sub-scores own-text content)
      (collect-child-scoring-data protocol element cache)
      (declare (ignorable sub-scores))
      (values -1 (list -0.1) own-text content)))
  (def-html-score-content
    (:element 
      (:abbr :b :bdo :cite :code :data :datalist :dfn :em
             :i :kbd :label
             :mark :math :meter :noscript :output
             :progress :q :ruby :samp :small :span
             :strong :sub :sup :time :var :wbr

             :h1 :h2 :h3 :h4 :h5 :h6
             :section :p :td :pre))
    (multiple-value-bind
      (sub-scores own-text content)
      (collect-child-scoring-data protocol element cache)
      (declare (ignorable sub-scores))
      (values (html-score-text protocol own-text)
              (list) own-text content))))

(defmethod build-content-dom ((protocol html-text-score-protocol)
                              (content html5-parser::document)
                              parent document)
  (declare (ignorable content parent document))
  (html5-parser:make-document))
(defmethod build-content-dom ((protocol html-text-score-protocol)
                              (content html5-parser::document-fragment)
                              parent document)
  (declare (ignorable content parent document))
  (html5-parser:make-fragment document))
(defmethod build-content-dom ((protocol html-text-score-protocol)
                              content
                              (parent null) document)
  (let* ((parent (html5-parser:make-document))
         (child (build-content-dom protocol content parent parent)))
    (if (html5-parser:node-first-child parent) parent child)))
(defmethod build-content-dom ((protocol html-text-score-protocol)
                              (content list)
                              (parent html5-parser::node)
                              document)
  (let* ((element (build-content-dom protocol (first content)
                                     parent document)))
    (loop for c in (rest content)
          do (build-content-dom protocol c element document))
    element))
(defmethod build-content-dom ((protocol html-text-score-protocol)
                              (content html5-parser::node)
                              (parent html5-parser::node)
                              document)
  nil)
(defmethod build-content-dom ((protocol html-text-score-protocol)
                              (content html5-parser::element)
                              (parent html5-parser::node)
                              document)
  (let* ((element
           (html5-parser:make-element 
             document
             (html5-parser:node-name content)
             (html5-parser:node-namespace content))))
    (html5-parser:element-map-attributes*
      (lambda (name namespace value)
        (setf (html5-parser:element-attribute element name namespace)
              value))
      content)
    (html5-parser:node-append-child parent element)
    element))
(defmethod build-content-dom ((protocol html-text-score-protocol)
                              (content html5-parser::text-node)
                              (parent html5-parser::node)
                              document)
  (let* ((element
           (html5-parser:make-text-node 
             document
             (html5-parser:node-value content))))
    (html5-parser:node-append-child parent element)
    element))

(defun html-extract-main-content (data score-protocol textify-protocol)
  (let* ((cache (make-hash-table :test 'equal))
         (argmax nil)
         (max-score 0)
         (target nil))
    (html-score-content score-protocol data cache)
    (maphash (lambda (k v)
               (if (> (first v) max-score)
                 (setf argmax k max-score (first v))))
             cache)
    (setf target argmax)
    (setf target (build-content-dom
                   score-protocol
                   (fourth (multiple-value-list
                             (html-score-content score-protocol target cache)))
                   nil nil))
    (if textify-protocol
      (html-element-to-text textify-protocol target) target)))

(defclass html-classname-score-protocol (html-text-score-protocol) ())
(defparameter *removable-classname-regexp*
  "-ad-|ai2html|banner|breadcrumbs|combx|comment|community|cover-wrap|disqus|extra|footer|gdpr|header|legends|menu|related|remark|replies|rss|shoutbox|sidebar|skyscraper|social|sponsor|supplemental|ad-break|agegate|pagination|pager|popup|yom-remote")
(defparameter *no-remove-classname-regexp* "and|article|body|column|content|main|shadow")
(defparameter *boost-classname-regexp* "article|body|content|entry|hentry|h-entry|main|page|pagination|post|text|blog|story")
(defparameter *suppress-classname-regexp* "hidden|^hid$| hid$| hid |^hid |banner|combx|comment|com-|contact|foot|footer|footnote|gdpr|masthead|media|meta|outbrain|promo|related|scroll|share|shoutbox|sidebar|skyscraper|sponsor|shopping|tags|tool|widget")

(with-html-score-content-protocol
  (html-classname-score-protocol) (protocol element cache)
  (def-html-score-content (:element (:form :fieldset :object :embed :footer :link :video
                                           :iframe :input :textarea :select :button))
    (values 0 () "" nil)))

(defmethod html-score-content-dispatch ((protocol html-classname-score-protocol)
                                        (type (eql :element)) tag element cache)
  (let* ((class-attribute (html5-parser:element-attribute element "class"))
         (id (string-downcase
               (html5-parser:element-attribute element "id")))
         (classes (cl-ppcre:split
                    (format nil "[~{~a~}]+" (or *whitespace-list* (list #\Space)))
                    (string-downcase class-attribute)))
         (boost
           (+ (if (find-if (lambda (x)
                             (cl-ppcre:scan *boost-classname-regexp* x))
                           classes)
                +25 0)
              (if (find-if (lambda (x) 
                             (cl-ppcre:scan *suppress-classname-regexp* x))
                           classes)
                -25 0)
              (if (cl-ppcre:scan *boost-classname-regexp* id) +25 0)
              (if (cl-ppcre:scan *suppress-classname-regexp* id) -25 0)))
         (drop (and (not (cl-ppcre:scan *no-remove-classname-regexp* id))
                    (not (find-if (lambda (x)
                                    (cl-ppcre:scan *no-remove-classname-regexp* x))
                                  classes))
                    (or (cl-ppcre:scan *removable-classname-regexp* id)
                        (find-if (lambda (x)
                                   (cl-ppcre:scan *removable-classname-regexp* x))
                                 classes)))))
    (cond (drop (values 0 () "" nil))
          (t (multiple-value-bind (score subscores pass-text content) (call-next-method)
               (let* ((boosted (+ score boost))
                      (core-score (or (car subscores) 0))
                      (pass-boosted (+ core-score (if (> boost 0) 0 (/ boost 2)))))
                 (values boosted (cons pass-boosted (cdr subscores)) pass-text content)))))))
