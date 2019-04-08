(in-package :thoughtful-theridion)

(defclass page-walker-brancher ()
  ((branches :accessor page-walker-branches 
             :initform nil :initarg :branches :type list)))

(defclass page-walker-brancher-skippable (page-walker-brancher) ())

(defvar *page-walker-terminator* (make-instance 'page-walker-brancher :branches ()))

(defmethod page-walker-terminator-p ((x page-walker-brancher))
  (null (page-walker-branches x)))

(defmethod page-walker-terminator-p ((x t)) nil)
(defmethod page-walker-terminator-p ((x page-walker-brancher-skippable)) nil)

(defmethod page-walk-each-of ((l list))
  (let* ((l (remove-if 'page-walker-terminator-p l)))
    (if (null l) *page-walker-terminator*
      (make-instance 'page-walker-brancher :branches l))))

(defmethod page-walk-each-of ((l page-walker-brancher))
  (page-walk-each-of (page-walker-branches l)))

(defmethod page-walk-maybe-each-of ((l list))
  (make-instance 'page-walker-brancher-skippable :branches l))

(defmethod page-walk-maybe-each-of ((l page-walker-brancher))
  (page-walk-maybe-each-of (page-walker-branches l)))

(defun page-walker-brancher-content (x)
  (cond ((typep x 'page-walker-brancher)
         (loop for y in (page-walker-branches x)
               append (if (typep y 'page-walker-brancher)
                        (page-walker-brancher-content y)
                        (list y))))
        (t x)))

(defmacro page-walk-each (&rest forms)
  (let ((var (gensym)))
    `(page-walk-each-of
       ,(if (null forms) nil
          `(let ((,var ,(first forms)))
             (unless (page-walker-terminator-p ,var)
               (list ,var
                     (page-walk-each ,@(rest forms)))))))))

(defun page-walker-clause-to-form (clause var env &key
                                          (extra-functions nil)
                                          (extra-macros nil)
                                          (aggressive t)
                                          (fetcher-var nil)
                                          (body nil))
  (cond ((and aggressive
              (stringp clause))
         `(page-walk-each-of (css-selectors:query ,clause ,var)))
        ((and (listp clause)
              (stringp (first clause)))
         `(html5-parser:element-attribute ,(second clause) ,(first clause)))
        ((and (listp clause)
              (eq (first clause) 'let)
              (symbolp (second clause)))
         (values
           (let ((main-form
                   `(page-walk (,var ,var :fetch nil
                                     :keep-brancher t
                                     :fetcher-var fetcher-var)
                               ,@body))
                 (result-var (second clause))
                 (rest-result-var (gensym))
                 (descend (gensym "let-clause-convertor")))
             `(let ((,result-var
                      (page-walk (,var ,var :fetch nil :keep-brancher t
                                       :fetcher-var fetcher-var)
                                 ,@(rest (rest clause)))))
                (labels ((,descend (,result-var)
                                   (if (typep ,result-var
                                              'page-walker-brancher)
                                     (if (null (page-walker-branches ,result-var))
                                       ,result-var
                                       (page-walk-each-of
                                         (loop for ,var in
                                               (page-walker-branches
                                                 ,result-var)
                                               for ,rest-result-var :=
                                               (,descend ,var)
                                               until
                                               (page-walker-terminator-p
                                                 ,rest-result-var)
                                               collect ,rest-result-var)))
                                     (let ((,result-var ,result-var)
                                           (,var ,var))
                                       ,main-form))))
                  (,descend ,result-var))))
           t))
        ((and (listp clause)
              (eq (first clause) 'vector))
         `(coerce
            (page-walk (,var ,var :fetch nil :keep-brancher t
                             :fetcher-var fetcher-var)
                       ,@(rest clause))
            'vector))
        ((and (listp clause)
              (eq (first clause) 'assert))
         (let ((subform (page-walker-clause-to-form
                          (second clause) var env
                          :extra-functions extra-functions
                          :extra-macros extra-macros
                          :fetcher-var fetcher-var
                          :aggressive t
                          :body
                          `((progn (error "unsupported kind of assertion")))))
               (subvar (gensym)))
           `(let ((,subvar ,subform))
              (assert ,subvar nil ,@(cdddr clause))
              ,(or (third clause) subvar))))
        ((and (listp clause)
              (eq (first clause) 'page-walk-maybe-each-of))
         (multiple-value-bind
           (subform full)
           (page-walker-clause-to-form
             (second clause) var env
             :extra-functions extra-functions
             :extra-macros :extra-macros
             :fetcher-var fetcher-var
             :aggressive t
             :body body)
           (values `(page-walk-maybe-each-of ,subform) full)))
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
                               :fetcher-var fetcher-var
                               :aggressive nil))))
        (t clause)))

(defmacro page-walk ((var value &key
                          (recurse nil)
                          (recur nil)
                          (fetcher '(make-instance 'http-fetcher))
                          (fetcher-var (gensym))
                          (fetch t)
                          (use-fetcher nil)
                          (keep-brancher nil))
                     &body body
                     &environment env)
  (assert (not (and recur recurse)))
  (let* ((recur (or recur recurse))
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
                 :fetcher-var fetcher-var
                 :aggressive t
                 :body (rest body))
               (if full form
                 (let ((result-var (gensym))
                       (rest-form
                         `(page-walk (,var ,var :fetch nil :keep-brancher t
                                           :fetcher-var fetcher-var)
                                     ,@(rest body)))
                       (rest-result-var (gensym))
                       (descend (gensym "page-walk")))
                   `(let ((,result-var ,form))
                      (labels ((,descend (,result-var)
                                         (if (typep ,result-var
                                                    'page-walker-brancher)
                                           (if (null (page-walker-branches
                                                       ,result-var))
                                             ,result-var
                                             (page-walk-each-of
                                               (loop for ,var in
                                                     (page-walker-branches
                                                       ,result-var)
                                                     for ,rest-result-var :=
                                                     (,descend ,var)
                                                     until
                                                     (page-walker-terminator-p
                                                       ,rest-result-var)
                                                     collect ,rest-result-var)))
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
