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
  :license "LLGPL"
  :depends-on (:cl-coroutine
               :cl-test-more)
  :components ((:module "t"
                :components
                ((:file "cl-coroutine"))))

  :perform (load-op :after (op c) (asdf:clear-system c)))
