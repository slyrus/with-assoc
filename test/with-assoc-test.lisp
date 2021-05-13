
(cl:in-package #:with-assoc-test)

(fiveam:in-suite :with-assoc)

(fiveam:test with-assoc/2
  (is (equal
       (with-assoc:with-assoc (:foo)
           '((:foo . "bar")
             (:baz . "wizard"))
         foo)
       "bar")))

(fiveam:test with-assoc/3
  (is (equalp
       (with-assoc:with-assoc (:foo :baz)
           '((:foo . "bar")
             (:baz . "wizard"))
         (list foo baz))
       '("bar" "wizard"))))

(fiveam:test with-assoc/4
  (is (equalp
       (with-assoc:with-assoc (foo baz)
           '((:foo . "bar")
             (:baz . "wizard"))
         (list foo baz))
       '("bar" "wizard"))))

(fiveam:test with-assoc/4
  (is (equalp
       (with-assoc:with-assoc (:foo :baz)
           '(("FOO" . "bar")
             (:baz . "wizard"))
         (list foo baz))
       '(nil "wizard"))))

;;
;; foo may be either "bar" or nil, depending on whether (eq "FOO"
;; "FOO"), which depends on a bunch of things. Ignore this test for
;; now.
#+nil
(fiveam:test with-assoc/5
  (is (equalp
       (with-assoc:with-assoc ("FOO" :baz)
           '(("FOO" . "bar")
             (:baz . "wizard"))
         (list foo baz))
       '("bar" "wizard"))))

(fiveam:test with-assoc/default-value
  (is (equalp
       (with-assoc:with-assoc (:foo :baz (:blort blort "splat"))
           '((:foo . "bar")
             (:baz . "wizard"))
         (list foo baz blort))
       '("bar" "wizard" "splat"))))


(fiveam:test with-assoc-equal/1
  (is (equalp
       (with-assoc:with-assoc-equal ("FOO" :baz)
           '(("FOO" . "bar")
             (:baz . "wizard"))
         (list foo baz))
       '("bar" "wizard"))))

(fiveam:test with-assoc-equal/default-value
  (is (equalp
       (with-assoc:with-assoc-equal ("FOO" :baz (:blort blort "splat"))
           '(("FOO" . "bar")
             (:baz . "wizard"))
         (list foo baz blort))
       '("bar" "wizard" "splat"))))

(fiveam:test with-assoc*/1
  (is (equalp
       (with-assoc:with-assoc* ((:foo :baz))
           '(("FOO" . "bar")
             (:baz . "wizard"))
         (list foo baz))
       '(nil "wizard"))))

(fiveam:test with-assoc*/2
  (is (equalp
       (with-assoc:with-assoc* (("FOO" :baz))
           '(("FOO" . "bar")
             (:baz . "wizard"))
         (list foo baz))
       '("bar" "wizard"))))

(fiveam:test with-assoc*/3
  (is (equalp
       (with-assoc:with-assoc* (("FOO" :baz) :test 'equal)
           '(("FOO" . "bar")
             (:baz . "wizard"))
         (list foo baz))
       '("bar" "wizard"))))

(fiveam:test with-assoc*/4
  (is (equalp
       (with-assoc:with-assoc* (("FOO" baz) :test 'equal)
           '(("FOO" . "bar")
             (:baz . "wizard"))
         (list foo baz))
       '("bar" "wizard"))))

(fiveam:test with-assoc*/5
  (is (equalp
       (with-assoc:with-assoc* ((("FOO" foo) :baz) :test 'equal)
           '(("FOO" . "bar")
             (:baz . "wizard"))
         (list foo baz))
       '("bar" "wizard"))))

(fiveam:test with-assoc*/6
  (is (equalp
       (with-assoc:with-assoc* ((("FOO" foo) (:baz bazzy)) :test 'equal)
           '(("FOO" . "bar")
             (:baz . "wizard"))
         (list foo bazzy))
       '("bar" "wizard"))))

(fiveam:test with-assoc*/default-value
  (is (equalp
       (with-assoc:with-assoc* ((:foo :baz (:blort blort "splort")))
           '((:foo . "bar")
             (:baz . "wizard"))
         (list foo baz blort))
       '("bar" "wizard" "splort"))))

(fiveam:test with-assoc*/unused-default-value
  (is (equalp
       (with-assoc:with-assoc* ((:foo :baz (:blort blort "splort")))
           '((:foo . "bar")
             (:baz . "wizard")
             (:blort . "quux"))
         (list foo baz blort))
       '("bar" "wizard" "quux"))))

(fiveam:test with-assoc*/test-equal-default-value
  (is (equalp
       (with-assoc:with-assoc* ((("FOO" foo) (:baz bazzy) ("BLORT" blort "splort")) :test 'equal)
           '(("FOO" . "bar")
             (:baz . "wizard"))
         (list foo bazzy blort))
       '("bar" "wizard" "splort"))))

(fiveam:test with-assoc*/test-equal-unused-default-value
  (is (equalp
       (with-assoc:with-assoc* ((("FOO" foo) (:baz bazzy) ("BLORT" blort "splort")) :test 'equal)
           '(("FOO" . "bar")
             (:baz . "wizard")
             ("BLORT" . "quux"))
         (list foo bazzy blort))
       '("bar" "wizard" "quux"))))


;; TODO: test key and test-not args to with-assoc*


