(in-package :thoughtful-theridion)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (unless (ignore-errors (drakma:parse-cookie-date "Mon 01 Jan 2000 00:00:00"))
    (let* ((default-drakma-parse-cookie-date
             (symbol-function 'drakma:parse-cookie-date)))
      (defun drakma:parse-cookie-date (s)
        (if (let ((tz (first (last (cl-ppcre:split " " s)))))
              (or (cl-ppcre:scan "^[A-Z]{3}" tz)
                  (cl-ppcre:scan "^-[0-9]{4}" tz)))
          (funcall default-drakma-parse-cookie-date s)
          (funcall default-drakma-parse-cookie-date
                   (concatenate 'string s " UTC")))))))
