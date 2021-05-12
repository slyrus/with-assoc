
(asdf:defsystem "with-assoc"
  :description "A macro for accessing data in association lists."
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :serial t
  :components ((:file "with-assoc"))
  :in-order-to ((test-op (test-op :with-assoc/test))))

(asdf:defsystem :with-assoc/test
  :depends-on (:with-assoc :fiveam)
  :serial t
  :components
  ((:module :test
            :components ((:file "package")
                         (:file "with-assoc-test"))))
  :perform (test-op (o c)
                    (uiop:symbol-call :fiveam '#:run! :with-assoc)))
