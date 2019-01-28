(in-package :thoughtful-theridion)

(defun select-form (data &optional (selector "form"))
  (cond
    ((stringp data)
     (select-form (html5-parser:parse-html5 data)))
    ((and (equal (html5-parser:node-type data) :element)
          (equal (html5-parser:node-name data) "form"))
     data)
    (t (loop for e := (css-selectors:query1 selector data)
             then (html5-parser:node-parent e)
             while e
             when (equal (html5-parser:node-type e) :element)
             when (equal (html5-parser:node-name e) "form")
             return e))))

(defun input-entry (data assignments)
  (cond
    ((stringp data)
     (input-entry 
       (html5-parser:node-last-child (html5-parser:parse-html5-fragment data))
       assignments))
    (t (let* ((name (html5-parser:element-attribute data "name"))
              (tag (html5-parser:node-name data))
              (type (html5-parser:element-attribute data "type"))
              (value (html5-parser:element-attribute data "value"))
              (checked (html5-parser:element-attribute data "checked"))
              (checkedp (> (length checked) 0))
              (assigned (assoc name assignments :test 'equal)))
         (cond (assigned nil)
               ((null name) nil)
               ((equal type "checkbox")
                (when checkedp (cons name "1")))
               ((equal type "radio")
                (when checkedp (cons name value)))
               ((equal tag "select")
                (let* ((selected-option
                         (first
                           (remove-if
                             (lambda (e)
                               (= 0 (length
                                      (html5-parser:element-attribute
                                        e "selected"))))
                             (css-selectors:query "option[selected]" data))))
                       (selected-value
                         (and selected-option
                              (html5-parser:element-attribute
                                selected-option "value"))))
                  (when selected-option (list name selected-value))))
               (t (when value (cons name value))))))))

(defun form-entries (data assignments)
  (cond
    ((stringp data)
     (form-entries
       (html5-parser:node-last-child (html5-parser:parse-html5-fragment data))
       assignments))
    (t (let* ((inputs (css-selectors:query "input" data))
              (selects (css-selectors:query "select" data))
              (textareas (css-selectors:query "textarea" data))
              (buttons (css-selectors:query "button" data))
              (elements (append inputs selects textareas buttons))
              (names (loop for e in elements collect
                           (html5-parser:element-attribute e "name")))
              (name-ht (make-hash-table :test 'equal))
              (names-list nil)
              (base-entries (append assignments
                                    (loop for e in elements collect
                                          (input-entry e assignments)))))
         (loop for n in names when n do (incf (gethash n name-ht 0)))
         (maphash (lambda (k v) v (push k names-list)) name-ht)
         (setf names-list (sort names-list 'string<))
         (values
           (loop for e in base-entries
                 for n := (car e)
                 for k := (gethash n name-ht 0)
                 collect e
                 when (> k 1) collect
                 (cons (format nil "~a[]" (car e)) (cdr e)))
           names-list)))))

(defun form-parameters (form &key
                             assignments (url *base-url*) drakma-args
                             fetcher (form-selector "form"))
  (let* ((form (or form
                   (and fetcher
                        (css-selectors:query1 form-selector
                                              (parsed-content fetcher)))))
         (url (or url (and fetcher
                           (current-url fetcher)
                           (format nil "~a" (current-url fetcher)))))
         (form-entries-values
           (multiple-value-list
             (form-entries form assignments)))
         (form-entry-names (second form-entries-values))
         (form-entries (first form-entries-values))
         (method (or (html5-parser:element-attribute form "method")
                     "get"))
         (method (intern (string-upcase method) :keyword))
         (action (or (html5-parser:element-attribute form "action")
                     url))
         (action (if (equal action "") url action))
         (action (let ((*base-url* url)) (real-url action)))
         (encoding (or (html5-parser:element-attribute form "enctype")
                       "application/x-www-form-urlencoded")))
    (append
      (list action)
      (list :method method :content-type encoding :parameters form-entries)
      (list
        :known-parameters (loop for x in form-entry-names collect `(,x . ""))
        :allow-other-keys t)
      drakma-args
      (list :external-format-out :utf-8))))

(defun submit-form (fetcher assignments &key
                            (form-selector "form")
                            chosen-form
                            (drakma-args nil))
  (let* ((document (parsed-content fetcher))
         (form (or chosen-form (select-form document form-selector)))
         (url (format nil "~a" (current-url fetcher)))
         (parameters (form-parameters
                       form :assignments assignments
                       :url (or url *base-url*)
                       :drakma-args drakma-args)))
    (apply 'navigate fetcher parameters)))
