(defpackage :thoughtful-theridion
  (:use :common-lisp)
  (:export
    #:*default-user-agent*
    #:http-fetcher
    #:navigate
    #:submit-form
    #:html-textifier-protocol
    #:html-textifier-protocol-inspector
    #:html-element-to-text
    #:html-inner-text
    #:save-web-page
    #:save-web-form
    #:parsed-content
    #:current-url
    #:current-content
    #:current-content-bytes
    #:current-headers
    #:current-status-code
    #:parse-obtained-content
    #:*base-url*
    #:real-url
    #:page-walk
    #:page-walk-each
    #:page-walk-each-of
    #:page-walk-maybe-each-of
    ))

