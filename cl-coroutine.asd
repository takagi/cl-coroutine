#|
  This file is a part of cl-coroutine project.
  Copyright (c) 2014 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-coroutine-asd
  (:use :cl :asdf))
(in-package :cl-coroutine-asd)

(defsystem cl-coroutine
  :version "0.1"
  :author "Masayuki Takagi"
  :license "LLGPL"
  :depends-on (:cl-cont :alexandria)
  :components ((:module "src"
                :components
                ((:file "cl-coroutine"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (load-op cl-coroutine-test))))
