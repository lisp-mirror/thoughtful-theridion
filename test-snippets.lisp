(asdf:make :thoughtful-theridion)

(in-package :thoughtful-theridion)

(defparameter *fetcher*
  (make-instance
    'http-fetcher
    :redirect-policy
    (lambda (&key content new)
      (when (= 0 (length content)) new))))

(documentation #'drakma:http-request t)

(make-instance 'drakma:cookie-jar)

(apply 'values 1 '(2 3))

(navigate (make-instance 'http-fetcher) "http://ya.ru/")

(puri:parse-uri "http://ya.ru/")

(navigate *fetcher* "http://twitter.com/snowden")

(navigate *fetcher* "http://m.twitter.com/snowden")

(current-headers *fetcher*)

(assoc :content-type (current-headers *fetcher*))

(html5-parser:parse-html5 "<a>")

(cl-json:decode-json-from-string "{\"aB\": [1]}")

(cl-json:decode-json-from-string "[[\"aB\", 1]]")

(parsed-content *fetcher*)

(let ((res nil))
  (html5-parser:element-map-children (lambda (x) (push x res)) (parsed-content *fetcher*))
  (reverse res))

(html5-parser:node-type (parsed-content *fetcher*))

(html5-parser:node-type (html5-parser:node-first-child (html5-parser:parse-html5-fragment "asdf")))

(html5-parser:node-value (html5-parser:node-first-child (html5-parser:parse-html5-fragment "asdf")))

(html5-parser:node-value (html5-parser:parse-html5-fragment "asdf"))

(html5-parser:node-type (html5-parser:node-first-child (parsed-content *fetcher*)))

(html5-parser:node-type (html5-parser:node-next-sibling (html5-parser:node-first-child (parsed-content *fetcher*))))

(html5-parser:node-type (html5-parser:node-last-child (parsed-content *fetcher*)))

(html5-parser:node-name (html5-parser:node-last-child (parsed-content *fetcher*)))

(html5-parser:node-next-sibling (html5-parser:node-last-child (parsed-content *fetcher*)))

(html-element-to-text (make-instance 'html-textifier-protocol)
                      (html5-parser:parse-html5 "<a href=\"qwe\">Some link text</a> continue<br>and newline"))

(html-element-to-text (make-instance 'html-textifier-protocol)
                      (html5-parser:parse-html5 (format nil "Source ~% newline")))

(html-element-to-text (make-instance 'html-textifier-protocol) (parsed-content *fetcher*))

(html5-parser:element-attribute (html5-parser:node-first-child (html5-parser:parse-html5-fragment "<a x=1 y=2>44</a>")) "z")

(html-element-to-text (make-instance 'html-textifier-protocol-inspector) (parsed-content *fetcher*))

(html-element-to-text (make-instance 'html-textifier-protocol-inspector) *fetcher*)

(navigate *fetcher* "http://lemonde.fr")

(navigate *fetcher* "http://mccme.ru")

(navigate *fetcher* "http://mccme.ru" :redirect t)

(save-web-page "http://mccme.ru"
               (format nil "/tmp/test-save-~a" (get-universal-time))
               "test"
               :drakma-args (list :redirect t))

(html5-parser:parse-html5 (current-content *fetcher*))

(current-headers *fetcher*)

(current-content *fetcher*)

(babel:octets-to-string (current-content-bytes *fetcher*) :encoding :utf-8 :errorp nil)

(html5-parser:parse-html5 (current-content-bytes *fetcher*))

(html5-parser:element-attribute (html-meta-encoding (current-content-bytes *fetcher*)) "content")

(navigate *fetcher* "http://jwz.org/blog/" :redirect t)

*fetcher*

(current-content *fetcher*)

(current-headers *fetcher*)

(css-selectors:query1 
  "form"
  (html5-parser:node-last-child
    (html5-parser:parse-html5 "<form action='' ></form>")))

(html5-parser:node-content
  (html5-parser:node-next-sibling
    (html5-parser:node-first-child
      (html5-parser:node-first-child
        (html5-parser:node-last-child
          (parsed-content *fetcher*))))))

(navigate *fetcher* "https://www.facebook.com/521916472/posts/10156492050021473")

(html5-parser:node-value
  (html5-parser:node-first-child
    (html5-parser:node-first-child
      (html5-parser:parse-html5-fragment "<noscript><i>x</i></noscript>"))))

(subseq
  (html-element-to-text
    (make-instance 'html-textifier-protocol-inspector)
    (parsed-content *fetcher*)
    :base-url "https://facebook.com/")
  0 1000)

(html5-parser:element-map-attributes
  (lambda (&rest args) (format t "~s~%" args))
  (html5-parser:node-last-child (parsed-content *fetcher*)))

*fetcher*

(navigate *fetcher* "https://github.com/login/")

(form-entries (select-form (parsed-content *fetcher*) "form[action='/session']")
              `(("login" . "test-login-no-such-user-exists")))

(submit-form *fetcher* 
             `(("login" . "test-login-no-such-user-exists"))
             :form-selector "form[action='/session']")

(form-parameters 
  nil
  :fetcher *fetcher* :form-selector "form[action='/session']" 
  :assignments `(("login" . "test-login-no-such-user-exists"))
  )

(navigate *fetcher* "http://kremlin.enterprises/post/142778524020/can-you-explain-to-some-degree-why-sparc-chips-run")

(navigate *fetcher* "http://kremlin.enterprises/post/142778524020/can-you-explain-to-some-degree-why-sparc-chips-run" :redirect t)

(current-content *fetcher*)

(parsed-content *fetcher*)

(html-element-to-text (make-instance 'html-textifier-protocol-inspector) (parsed-content *fetcher*))

(save-web-page "http://kremlin.enterprises/post/142778524020/can-you-explain-to-some-degree-why-sparc-chips-run" "/tmp/test-tt/" (format nil "test-~a" (get-universal-time)))

(urlencode "qwe:/" t)

(urldecode "http%3A%2F%2F" t)

(navigate *fetcher* "http://мцнмо.рф/")

(navigate *fetcher* "https://www.livejournal.com/users/r-ray/" :redirect t)

(html-element-to-text (make-instance 'html-textifier-protocol-inspector) (parsed-content *fetcher*))

(submit-form *fetcher* `(("adult_check" . ,(html5-parser:element-attribute (css-selectors:query1 "button[name='adult_check']" (parsed-content *fetcher*)) "value"))) :form-selector "div.b-msgsystem-warningbox form")

(submit-form *fetcher* nil 
             :form-selector "div.b-msgsystem-warningbox form"
             :drakma-args '(:redirect t))

(form-parameters nil
                 :fetcher *fetcher*
                 :form-selector "div.b-msgsystem-warningbox form")

(html5-parser:element-attribute (css-selectors:query1 "button[name='adult_check']" (parsed-content *fetcher*)) "value")

(html-element-to-text (make-instance 'html-textifier-protocol-inspector) (css-selectors:query1 "div.b-msgsystem-warningbox form" (parsed-content *fetcher*)))

(form-entries
  (css-selectors:query1
    "div.b-msgsystem-warningbox form" (parsed-content *fetcher*))
  `(("adult_check" . ,(html5-parser:element-attribute (css-selectors:query1 "button[name='adult_check']" (parsed-content *fetcher*)) "value"))))

(let* ((form (css-selectors:query1 "div.b-msgsystem-warningbox form"
                                   (parsed-content *fetcher*)))
       (button (css-selectors:query1 "button" form))
       (name (html5-parser:element-attribute button "name"))
       (value (html5-parser:element-attribute button "value"))
       (value-plus (cl-ppcre:regex-replace-all " " value "+")))
  (submit-form *fetcher* `((,name . ,value-plus))
               :chosen-form form))


(drakma:cookie-jar-cookies (make-instance 'drakma:cookie-jar))

(first (drakma:cookie-jar-cookies (cookie-jar *fetcher*)))

(drakma:cookie-name (first (drakma:cookie-jar-cookies (cookie-jar *fetcher*))))
(drakma:cookie-value (first (drakma:cookie-jar-cookies (cookie-jar *fetcher*))))
(drakma:cookie-domain (first (drakma:cookie-jar-cookies (cookie-jar *fetcher*))))
(drakma:cookie-path (first (drakma:cookie-jar-cookies (cookie-jar *fetcher*))))
(drakma:cookie-expires (first (drakma:cookie-jar-cookies (cookie-jar *fetcher*))))

(make-instance 
  'drakma:cookie
  :name
  (drakma:cookie-name (first (drakma:cookie-jar-cookies (cookie-jar *fetcher*))))
  :value
  (drakma:cookie-value (first (drakma:cookie-jar-cookies (cookie-jar *fetcher*))))
  :domain
  (drakma:cookie-domain (first (drakma:cookie-jar-cookies (cookie-jar *fetcher*))))
  :path
  (drakma:cookie-path (first (drakma:cookie-jar-cookies (cookie-jar *fetcher*))))
  :expires
  (drakma:cookie-expires (first (drakma:cookie-jar-cookies (cookie-jar *fetcher*)))))

(make-instance
  'drakma:cookie-jar
  :cookies
  (list
    (make-instance 
      'drakma:cookie
      :name
      (drakma:cookie-name (first (drakma:cookie-jar-cookies (cookie-jar *fetcher*))))
      :value
      (drakma:cookie-value (first (drakma:cookie-jar-cookies (cookie-jar *fetcher*))))
      :domain
      (drakma:cookie-domain (first (drakma:cookie-jar-cookies (cookie-jar *fetcher*))))
      :path
      (drakma:cookie-path (first (drakma:cookie-jar-cookies (cookie-jar *fetcher*))))
      :expires
      (drakma:cookie-expires (first (drakma:cookie-jar-cookies (cookie-jar *fetcher*)))))
    ))

(navigate *fetcher* "https://lobste.rs/newest/page/2")

(navigate *fetcher* "https://lobste.rs/newest/")

(html-element-to-text 
  (make-instance 'html-textifier-protocol-inspector)
  (css-selectors:query1 "div.morelink > a:nth-last-child(2)" (parsed-content *fetcher*)))
