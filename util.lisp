(in-package :thoughtful-theridion)

(defun save-web-page (url target basename &key 
                          fetcher fetcher-parameters
                          drakma-args html-textifier-protocol
                          html-content-score-protocol
                          skip-extract-main-content skip-cookies skip-forms
                          main-content-selector
                          referrer
                          )
  (ensure-directories-exist (format nil "~a/" target))
  (let* ((f (or fetcher
                (apply 'make-instance 'http-fetcher fetcher-parameters)))
         (url (or url (and fetcher (current-url fetcher)))))
    (unless
      (equal (current-url f) url)
      (apply 'navigate f url drakma-args))
    (with-open-file (out (format nil "~a/~a.url" target basename)
                         :direction :output)
      (format out "~a~%~a~%~a~%" url (current-url f) referrer))
    (with-open-file (out (format nil "~a/~a.html" target basename)
                         :direction :output)
      (format out "~a~%" (current-content f)))
    (with-open-file (out (format nil "~a/~a.html.txt" target basename)
                         :direction :output)
      (format out "→[ ~a ]~%→[ ~a ]~%→[ ~a ]~%~a~%~a~%~{~%(~{~s~%~})~%~%~}~{~S~%#.~S~%~}~% ### ### ### ~%~a~%"
              url (current-url f) (current-decoded-url f)
              (or
                (ignore-errors
                  (html-element-to-text
                    (or 
                      html-textifier-protocol
                      (make-instance 'html-textifier-protocol-inspector))
                    (css-selectors:query1 "head title" (parsed-content f))))
                "")
              (or
                (ignore-errors
                  (unless 
                    (or
                      skip-extract-main-content
                      (> (length (current-content f)) 1048576)
                      )
                    (cl-ppcre:regex-replace-all
                      " *\\n *\\n"
                      (let* ((*base-url* (current-url f))
                             (textifier
                               (or 
                                 html-textifier-protocol
                                 (make-instance
                                   'html-textifier-protocol-formatting-inspector))))
                        (if main-content-selector
                          (html-element-to-text 
                            textifier
                            (css-selectors:query1 main-content-selector
                                                  (parsed-content f)))
                          (html-extract-main-content 
                            (parsed-content f)
                            (or html-content-score-protocol
                                (make-instance 'html-classname-score-protocol))
                            textifier)))
                      (coerce (list #\Newline #\Newline) 'string))))
                "")
              (when (and (not skip-forms) (parsed-content f))
                (loop for form in (ignore-errors
                                    (css-selectors:query
                                    "form" (parsed-content f)))
                      collect (form-parameters form :fetcher f)))
              (unless (or skip-cookies (null (cookie-jar f)))
                (list
                  :cookie-jar
                  `(make-instance
                     'drakma:cookie-jar
                     :cookies
                     (list
                       ,@(loop for c in (drakma:cookie-jar-cookies (cookie-jar f))
                               collect
                               `(make-instance
                                  'drakma:cookie
                                  :name
                                  ,(drakma:cookie-name c)
                                  :value
                                  ,(drakma:cookie-value c)
                                  :domain
                                  ,(drakma:cookie-domain c)
                                  :path
                                  ,(drakma:cookie-path c)
                                  :expires
                                  ,(drakma:cookie-expires c)))))))
              (cond
                ((parsed-content f)
                 (or
                   (ignore-errors
                     (html-element-to-text
                       (or html-textifier-protocol
                           (make-instance 'html-textifier-protocol-inspector)) f))
                   (parsed-content f)))
                ((parser-warnings f)
                 (parser-warnings f))
                ((cl-ppcre:scan 
                   "^text/"
                   (cdr (assoc :content-type (current-headers fetcher))))
                 (current-content fetcher))
                (t "."))))))

(defun save-web-form (form-parameters target basename &key
                                      fetcher-parameters drakma-args 
                                      html-textifier-protocol)
  (save-web-page (first form-parameters) target basename
                 :fetcher-parameters fetcher-parameters
                 :drakma-args (append (rest form-parameters) drakma-args)
                 :html-textifier-protocol html-textifier-protocol))
