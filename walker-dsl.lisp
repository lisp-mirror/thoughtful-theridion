(defclass page-walker-brancher ()
  ((branches :accessor page-walker-branches 
             :initform nil :initarg :branches)))

(defvar *page-walker-terminator* (make-instance 'page-walker-brancher :branches ()))

(defmethod page-walker-terminator-p ((x page-walker-brancher))
  (null (page-walker-branches x)))

(defmethod page-walker-terminator-p ((x t)) nil)

(defun page-walker-brancher (l)
  (let* ((l (remove-if 'page-walker-terminator-p l)))
    (if (null l) *page-walker-terminator*
      (make-instance 'page-walker-brancher :branches l))))

(defun page-walker-brancher-content (x)
  (cond ((typep x 'page-walker-brancher)
         (loop for y in (page-walker-branches x)
               append (if (typep y 'page-walker-brancher)
                        (page-walker-brancher-content y)
                        (list y))))
        (t x)))

(defmacro page-walker-brancher-progn (&rest forms)
  (let ((var (gensym)))
    `(page-walker-brancher
       ,(if (null forms) nil
          `(let ((,var ,(first forms)))
             (unless (page-walker-terminator-p ,var)
               (list ,var
                     (page-walker-brancher-progn ,@(rest forms)))))))))

(defun page-walker-clause-to-form (clause var env &key
                                          (extra-functions nil)
                                          (extra-macros nil)
                                          (aggressive t)
                                          (body nil))
  (cond ((and aggressive
              (stringp clause))
         `(page-walker-brancher (css-selectors:query ,clause ,var)))
        ((and (listp clause)
              (stringp (first clause)))
         `(html5-parser:element-attribute ,(second clause) ,(first clause)))
        ((and (listp clause)
              (eq (first clause) 'let)
              (symbolp (second clause)))
         (values
           (let ((main-form
                   `(with-page (,var ,var :fetch nil
                                     :keep-brancher t)
                               ,@body))
                 (result-var (second clause))
                 (rest-result-var (gensym))
                 (descend (gensym "let-clause-convertor")))
             `(let ((,result-var
                      (with-page (,var ,var :fetch nil :keep-brancher t)
                                 ,@(rest (rest clause)))))
                (labels ((,descend (,result-var)
                                   (if (typep ,result-var
                                              'page-walker-brancher)
                                     (page-walker-brancher
                                       (loop for ,var in
                                             (page-walker-branches
                                               ,result-var)
                                             for ,rest-result-var :=
                                             (,descend ,var)
                                             until
                                             (page-walker-terminator-p
                                                 ,rest-result-var)
                                             collect ,rest-result-var))
                                     (let ((,result-var ,result-var)
                                           (,var ,var))
                                       ,main-form))))
                  (,descend ,result-var))))
           t))
        ((and (listp clause)
              (eq (first clause) 'vector))
         `(coerce
            (with-page (,var ,var :fetch nil :keep-brancher t)
                       ,@(rest clause))
            'vector))
        ((and (listp clause)
              (consp clause)
              (symbolp (first clause))
              (or (find (first clause) extra-functions)
                  (and
                    (null (member (first clause) extra-macros))
                    (null (macro-function (first clause) env)))))
         (cons (first clause)
               (loop for arg in (rest clause)
                     collect (page-walker-clause-to-form
                               arg var env
                               :extra-functions extra-functions
                               :extra-macros extra-macros
                               :aggressive nil))))
        (t clause)))

(defun html-to-text (element)
  (html-element-to-text (make-instance 'html-textifier-protocol)
                        element))

(defmacro with-page ((var value &key
                          (recurse nil)
                          (recur nil)
                          (fetcher '(make-instance 'http-fetcher))
                          (fetch t)
                          (use-fetcher nil)
                          (keep-brancher nil))
                     &body body
                     &environment env)
  (assert (not (and recur recurse)))
  (let* ((recur (or recur recurse))
         (fetcher-var (gensym))
         (top-wrapper (if fetch
                        `(let* ((,fetcher-var ,fetcher)
                                (*base-url* ,var)
                                (,var (progn
                                        (navigate ,fetcher-var ,var)
                                        ,(if use-fetcher
                                           fetcher-var
                                           `(parsed-content ,fetcher-var))))))
                        `(let* ((,var ,var)))))
         (first-clause (first body))
         (main-body
           (if (null body)
             var
             (multiple-value-bind (form full)
               (page-walker-clause-to-form
                 first-clause var env
                 :extra-functions (list recur)
                 :extra-macros nil
                 :aggressive t
                 :body (rest body))
               (if full form
                 (let ((result-var (gensym))
                       (rest-form
                         `(with-page (,var ,var :fetch nil :keep-brancher t)
                                     ,@(rest body)))
                       (rest-result-var (gensym))
                       (descend (gensym "with-page")))
                   `(let ((,result-var ,form))
                      (labels ((,descend (,result-var)
                                         (if (typep ,result-var
                                                    'page-walker-brancher)
                                           (page-walker-brancher
                                             (loop for ,var in
                                                   (page-walker-branches
                                                     ,result-var)
                                                   for ,rest-result-var :=
                                                   (,descend ,var)
                                                   until
                                                   (page-walker-terminator-p
                                                       ,rest-result-var)
                                                   collect ,rest-result-var))
                                           (let ((,var ,result-var))
                                             ,rest-form))))
                        (,descend ,result-var))))))))
         (main-wrapped (append top-wrapper (list main-body)))
         (full-computation
           (if recur `(labels ((,recur (,var) ,main-wrapped))
                          (,recur ,var))
             main-wrapped))
         (full-computation-wrapped `(let ((,var ,value)) ,full-computation))
         (unpacked-result
           (if keep-brancher
             full-computation-wrapped
             `(page-walker-brancher-content ,full-computation-wrapped))))
    unpacked-result))
