
(cl:defpackage #:with-assoc
  (:use :cl)
  (:export #:with-assoc))

(cl:in-package #:with-assoc)

(defmacro with-assoc (associations alist &body body)
  "Takes a list of (key variable) pairs, an association list, and a
body of code for which the variables will be bound to (cdr (assoc
<key> alist))."
  (let ((%alist (gensym "ALIST"))
        (pairvars (loop repeat (length associations)
                        collect (gensym "PAIR"))))
    `(let ((,%alist ,alist))
       (let (,@(loop for (value ignore) in associations
                     for pvar in pairvars
                     collect `(,pvar (assoc ,value ,%alist))))
         (declare (ignorable ,@pairvars))
         (symbol-macrolet
             (,@(loop for (ignore var) in associations
                      for pvar in pairvars
                      collect `(,var (cdr ,pvar))))
           ,@body)))))
