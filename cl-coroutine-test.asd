#|
  This file is a part of cl-coroutine project.
  Copyright (c) 2014 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-coroutine-test-asd
  (:use :cl :asdf))
(in-package :cl-coroutine-test-asd)

(defsystem cl-coroutine-test
  :author "Masayuki Takagi"
  :license ""
  :depends-on (:cl-coroutine
               :cl-test-more)
  :components ((:module "t"
                :components
                ((:file "cl-coroutine"))))

  :defsystem-depends-on (:cl-test-more)
  :perform (test-op :after (op c)
                    (funcall (intern #. (string :run-test-system) :cl-test-more)
                             c)
                    (asdf:clear-system c)))
