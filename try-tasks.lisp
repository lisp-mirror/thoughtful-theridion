(defpackage :thoughtful-theridion/try
  (:use :common-lisp)
  (:export
    #:ok
    #:spawnable-lisp
    #:*default-spawnable-lisp*
    #:every-in-instance 
    #:every-in-instances
    #:define-task
    #:undefine-all-tasks
    #:select-tasks
    #:soft-check))

(in-package :thoughtful-theridion/try)

(defparameter *ok-var* :unset)

(defmacro ok (&body body)
  "
  A macro to track success of at least one of different attempts.

  (ok [forms]) returns whether (ok) was ever called during the evaluation.

  (ok) must only be called within dynamic extent of (ok ...)
  "
  (cond
    ((and body
          (not (member-if 'listp body))
          (getf body :soft-check))
     (when (equal *ok-var* nil)
       (setf *ok-var* t)))
    (body
      `(let ((*ok-var* nil))
         ,@ body
         *ok-var*))
    (t
      `(progn
         (unless (member *ok-var* (list t nil))
           (error 
             "(ok) must be called during the dynamic extent of (ok ...)"))
         (setf *ok-var* t)))))

;; Fetching stuff from network can end up with LDB, so play safer
;;
;; Strings are used to allow things that don't print readably
;; This does add a bit of complexity
(defclass spawnable-lisp ()
  ((base-command :type list :initarg :base-command
                 :accessor spawnable-lisp-base-command
                 :initform (list (first (uiop:raw-command-line-arguments)) 
                                 #+(or abcl clasp sbcl) "--noinform" 
                                 #+(or clasp sbcl) "--non-interactive"
                                 #+(or ccl clisp mkcl) "--quiet"
                                 #+(or abcl ccl) "--batch")
                 :documentation "The basic command and arguments")
   (eval-argument :type string :initarg :eval-argument
                  :accessor spawnable-lisp-eval-argument
                  :initform (or
                              #+clisp "-x"
                              #+mkcl "-eval"
                              "--eval")
                  :documentation "The lisp CLI argument to eval a form")
   (initial-forms :type list :initarg :initial-forms
                  :accessor spawnable-lisp-initial-forms
                  :initform `((with-output-to-string
                                (*standard-output*)
                                (require :asdf))
                              (with-output-to-string
                                (*standard-output*)
                                (asdf:load-system 
                                  :thoughtful-theridion)
                                (asdf:load-system 
                                  :thoughtful-theridion/content-series)))
                  :documentation 
                  "Extra printable forms or strings to pass to eval"))
  (:documentation 
    "A class representing how to spawn a Common Lisp implementation"))

(defvar *default-spawnable-lisp* (make-instance 'spawnable-lisp))

(defun stringify-form (f)
  (if (stringp f) f
    (let* ((*package* (find-package :common-lisp)))
      (format nil "~s" f))))

(defmethod
  spawnable-lisp-command-for-forms ((command spawnable-lisp)
                              (forms list)
                              &key (exit-value-code 0))
  (append
    (spawnable-lisp-base-command command)
    (loop for f in
          (append (spawnable-lisp-initial-forms command)
                  forms
                  (list
                    (if (stringp exit-value-code)
                      (format nil "(uiop:quit ~a)" exit-value-code)
                      `(uiop:quit ,exit-value-code))))
          for f-string := (stringify-form f)
          collect (spawnable-lisp-eval-argument command)
          collect f-string)))

(defparameter *spawnable-trial-backtrace-output* *standard-output*)

(defmacro form-testcase (f)
  `(lambda ()
     (try:with-test (nil :name ',f)
       (try:is
         (block
           try-block
           (handler-case
             ,f
             (serious-condition 
               (e)
               (format *spawnable-trial-backtrace-output*
                       "Error in form:~%~s~%" ',f)
               (trivial-backtrace:print-backtrace 
                 e :output *spawnable-trial-backtrace-output*)
               (finish-output *spawnable-trial-backtrace-output*)
               (return-from try-block nil))))
         :msg ,(stringify-form f)))))

(defmacro spawnable-trial ((report-file backtrace-file) &body forms)
  `(with-open-file (*spawnable-trial-backtrace-output* 
                     ,backtrace-file 
                     :direction :output 
                     :if-exists :overwrite)
     (let* ((trial
              (try:try
                (list
                  ,@(loop for f in (alexandria:shuffle forms)
                          collect `(form-testcase ,f))))
                :stream *error-output* :collect t))
            (report 
              (with-output-to-string (s) 
                (try:replay-events trial :stream s)))
            (passedp (try:passedp trial))
            (code (if passedp 0 1)))
       (alexandria:write-string-into-file
         report ,report-file :if-exists :supersede)
       (uiop:quit code))))

(defmethod every-in-instance ((spawner spawnable-lisp) (forms list))
  (let* ((report-file
           (uiop:with-temporary-file (:keep t :pathname pathname)
                                     (namestring pathname)))
         (backtrace-file
           (uiop:with-temporary-file (:keep t :pathname pathname)
                                     (namestring pathname))))
    (multiple-value-bind
      (output error-output code)
      (with-input-from-string (empty-stream "")
        (uiop:run-program
          (spawnable-lisp-command-for-forms
            spawner
            (list
              (format 
                nil
                "(thoughtful-theridion/try::spawnable-trial (~s ~s)~{ ~a~})"
                report-file backtrace-file
                (mapcar 'stringify-form forms))))
          :output :string :error-output :string
          :input empty-stream
          :ignore-error-status t))
      (let* ((report (alexandria:read-file-into-string report-file))
             (backtrace (when (probe-file backtrace-file)
                          (alexandria:read-file-into-string backtrace-file))))
        (ignore-errors (delete-file report-file))
        (ignore-errors (delete-file backtrace-file))
        (values
          (= code 0)
          report
          backtrace
          error-output
          output
          code)))))

(defmethod every-in-instances ((spawner spawnable-lisp) (form-lists list))
  (let* ((threads nil))
    (loop for form-list in form-lists
          do (push (bordeaux-threads-2:make-thread
                     (let ((form-list form-list))
                       (lambda () (every-in-instance spawner form-list))))
                   threads))
    (let* ((results
             (loop for thread in threads
                   collect
                   (multiple-value-list
                     (bordeaux-threads-2:join-thread thread)))))
      (apply 
        'values
        (every 'first results)
        (loop for k from 0 to (1- (apply 'max (mapcar 'length results)))
              collect (loop for r in results collect (nth k r)))))))

(defmethod every-in-instance ((spawner (eql t)) (l t))
  (every-in-instance *default-spawnable-lisp* l))
(defmethod every-in-instances ((spawner (eql t)) (l t))
  (every-in-instances *default-spawnable-lisp* l))

(defvar *task-list* nil
  "A list of defined tasks")

(defun undefine-all-tasks ()
  (setf *task-list* nil))

(defmacro define-task (tags form)
  "
  Define a task; a form to evaluate but with tags prepended
  Tags can be keywords or lists beginning with keywords
  "
  `(progn
     (push (list ',form
                 ,@ (loop for p in tags
                          collect
                          ; values inside can be evaluated
                          (cond
                            ((listp p)
                             `(list ,@p))
                            (t `(list ,p)))))
           *task-list*)))

(defun taglist-matches-tag (taglist tag &rest values)
  "
  Checks whether a task taglist has the tag (optionally with specific values)
  Returns the remaining values (or T if none)
  "
  (loop for entry in taglist
        when (alexandria:starts-with-subseq
               (cons tag values)
               (alexandria:ensure-list entry)
               :test 'equal)
        return 
        (or (subseq (alexandria:ensure-list entry)
                    (1+ (length values)))
            t)))

(defvar *current-searched-task-in-list* nil)

(defun tagp (&rest template)
  "Magic alias for taglist-matches-tag to use in iteration"
  (apply 'taglist-matches-tag 
         (rest *current-searched-task-in-list*)
         template))

(defmacro select-tasks (entry predicate &key group-by (package *package*))
  "
  Provide a list of lists of tasks matching predicate grouped by group-by
  Predicate and group-by are forms to be evaluated with entry set to a task
  TAGP is injected into the current package so one can write succinct queries
  No promises about the order of the tasks
  "
  (let* ((package (find-package package)))
  `(macrolet ((,(intern "TAGP" package) (&rest spec) `(tagp ,@ spec)))
     (let* ((grouping-ht (make-hash-table :test 'equal))
            (res nil))
     (loop for ,entry in *task-list*
           do
           (let ((*current-searched-task-in-list* ,entry))
             (when ,predicate
               (push (first entry) (gethash ,group-by grouping-ht)))))
     (maphash (lambda (k v) (push v res)) grouping-ht)
     res))))

(defmacro soft-check (&body body)
  `(when (ignore-errors (try:current-trial))
     ,@ body))
