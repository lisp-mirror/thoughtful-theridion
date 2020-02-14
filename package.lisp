(defpackage :thoughtful-theridion
  (:use :common-lisp)
  (:export
    ;; Fetching
    #:*default-user-agent*
    #:http-fetcher
    #:navigate
    #:submit-form
    ;; Additional functionality for fetching
    #:parsed-content
    #:current-url
    #:current-content
    #:current-content-bytes
    #:current-headers
    #:current-status-code
    #:parse-obtained-content
    #:*base-url*
    #:real-url
    ;; Converting HTML to text
    #:html-textifier-protocol
    #:html-textifier-protocol-inspector
    #:html-element-to-text
    #:html-inner-text
    ;; Generic page walking/web crawling macros
    #:page-walk
    #:page-walk-each
    #:page-walk-each-of
    #:page-walk-maybe-each-of
    #:page-walk-each-of-nonempty
    ;; Main content extraction
    #:html-score-content
    #:html-score-content-dispatch
    #:with-html-score-content-protocol
    #:def-html-score-content
    #:html-text-score-protocol
    #:html-classname-score-protocol
    #:html-score-text
    #:html-content-score-summarize
    #:collect-child-scoring-data
    #:build-content-dom
    #:html-extract-main-content
    ;; Utility functions
    #:save-web-page
    #:save-web-form
    ))

