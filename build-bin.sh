#! /usr/bin/env bash

cd "$(dirname "$0")"

NO_RLWRAP=1 ./nix-load-lisp.sh --non-interactive --eval '
(progn
  (setf cffi:*foreign-library-directories*
        (remove
           ""
           (uiop:split-string (uiop:getenv "LD_LIBRARY_PATH") 
                              :separator ":")
                              :test (function equalp)))
  (loop
    with libpath :=
    (uiop:split-string
      (uiop:getenv "LD_LIBRARY_PATH")
      :separator ":")
    for l in sb-alien::*shared-objects*
    for ns := (sb-alien::shared-object-namestring l)
    do (and (> (length ns) 0) (not (equal (elt ns 0) "/"))
            (let*
              ((prefix (find-if
                         (lambda (s)
                           (probe-file (format nil "~a/~a" s ns)))
                         libpath))
               (fullpath (and prefix (format nil "~a/~a" prefix ns))))
              (when fullpath
                (setf
                  (sb-alien::shared-object-namestring l) fullpath
                  (sb-alien::shared-object-pathname l)
                  (probe-file fullpath))))))
  (sb-ext:save-lisp-and-die "./thoughtful-theridion.bin" :executable t))
'
