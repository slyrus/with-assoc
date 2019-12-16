
(cl:defpackage #:with-assoc
  (:use :cl)
  (:export #:with-assoc*
           #:with-assoc
           #:with-assoc-equal))

(cl:in-package #:with-assoc)

(defmacro with-assoc* ((associations &key key test) alist &body body)
  "Takes a list containing a list of keyspec pairs (where keyspec is
either 1) an atom key where key is a string or a keyword, or 2) a list
of the form (key var)) and (optional) keyword args key and test, an
association list, and a body of code for which the variables will be
bound to (cdr (assoc <key> alist))."
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
             (,@(loop for var in associations
                      for pvar in pairvars
                   collect
                     (let ((var
                            (if (and var (listp var))
                                (cadr var)
                                (intern (if (symbolp var)
                                            (symbol-name var)
                                            var)))))
                       `(,var (cdr ,pvar)))))
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

