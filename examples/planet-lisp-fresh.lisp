(use-package :thoughtful-theridion)

(print
  (page-walk (x "https://planet.lisp.org/")
    "div#content > p > a" 
    ("href" x) 
    (page-walk (x x :recur recur) 
               (let prev "tr:first-child > td > a" 
                 ("href" x)) 
               (page-walk-each
                 (page-walk (x x :fetch nil :keep-brancher t)
                            (page-walk-maybe-each-of "li > a")
                            ("href" x)
                            (if (cl-ppcre:scan "/201[0-8]/" x)
                              (page-walk-each) x))
                 (recur prev)))))
