(asdf:defsystem
  ; Theridiidae are also known as cobweb spiders
  :thoughtful-theridion

  :license "GPLv3+"

  :description
  "A tool for browsing-like web use"

  :author "Michael Raskin <38a938c2@rambler.ru>"

  :depends-on (:drakma :cl+ssl
                       :cl-html5-parser :cl-json
                       :babel
                       :css-selectors
                       :css-selectors-simple-tree
                       :cl-ppcre :cl-unicode
                       :parenscript)
  
  :components
  ((:file "package")
   (:file "charsets" :depends-on ("package"))
   (:file "urlencode" :depends-on ("package" "charsets"))
   (:file "date-parsing-experiments" :depends-on ("package"))
   (:file "basic-retrieve" :depends-on ("package" "charsets" "urlencode"
                                        "date-parsing-experiments"))
   (:file "html-textify" :depends-on ("basic-retrieve"))
   (:file "html-forms" :depends-on ("package"))
   (:file "html-main-text" :depends-on ("package" "html-textify"))
   (:file "util" :depends-on ("basic-retrieve" "html-textify" "html-main-text"))
   (:file "walker-dsl" :depends-on ("package" "basic-retrieve"))))
