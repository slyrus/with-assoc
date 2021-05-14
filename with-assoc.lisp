
(cl:defpackage #:with-assoc
  (:use :cl)
  (:export #:with-assoc*
           #:with-assoc
           #:with-assoc-equal))

(cl:in-package #:with-assoc)

(defmacro with-assoc* ((associations &key key test) alist &body body)
  "Takes a list containing a list of keyspec pairs (where keyspec is
either 1) an atom key where key is a string or a keyword, or 2) a list
of the form (key var &optional default-value)) and (optional) keyword
args key and test, an association list, and a body of code for which
the variables will be bound to (cdr (assoc <key> alist)) or
default-value if (assoc <key> alist) returns nil."
  (let ((%alist (gensym "ALIST"))
        (pairvars (loop repeat (length associations)
                     collect (gensym "PAIR"))))
    `(let ((,%alist ,alist))
       (let (,@(loop for value in associations
                  for pvar in pairvars
                  collect
                    (let ((value
                           (if (and value (listp value))
                               (car value)
                               value)))
                      `(,pvar (assoc ,(if (and (symbolp value)
                                               (not (keywordp value)))
                                          (intern (symbol-name value) :keyword)
                                          value)
                                     ,%alist
                                     ,@(when key `(:key ,key))
                                     ,@(when test `(:test ,test)))))))
         (declare (ignorable ,@pairvars))
         (symbol-macrolet
             (,@(loop for var-spec in associations
                   for pvar in pairvars
                   collect
                     (let ((var
                            (if (and var-spec (listp var-spec))
                                (cadr var-spec)
                                (intern (if (symbolp var-spec)
                                            (symbol-name var-spec)
                                            var-spec)))))
                       (if (listp var-spec)
                           (destructuring-bind (var-key var-var &optional
                                                        (var-default nil var-default-supplied-p))
                               var-spec
                             (declare (ignore var-key var-var))
                             `(,var ,(if var-default-supplied-p
                                         `(if ,pvar
                                              (cdr ,pvar)
                                              ,var-default)
                                         `(cdr ,pvar))))
                           `(,var (cdr ,pvar))))))
           ,@body)))))

(defmacro with-assoc (associations alist &body body)
  "Takes a list of keyspec pairs (where keyspec is either 1) an atom
key where key is a string or a keyword, or 2) a list of the form (key
var)), an association list, and a body of code for which the variables
will be bound to (cdr (assoc <key> alist))."
  `(with-assoc* (,associations)
       ,alist
     ,@body))

(defmacro with-assoc-equal (associations alist &body body)
  "Takes a list of keyspec pairs (where keyspec is either 1) an atom
key where key is a string or a keyword, or 2) a list of the form (key
var)), an association list, and a body of code for which the variables
will be bound to (cdr (assoc <key> alist :test 'equal))."
  `(with-assoc* (,associations :test 'equal)
       ,alist
     ,@body))

