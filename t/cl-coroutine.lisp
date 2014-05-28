#|
  This file is a part of cl-coroutine project.
  Copyright (c) 2014 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-coroutine-test
  (:use :cl
        :cl-coroutine
        :cl-test-more))
(in-package :cl-coroutine-test)

(plan nil)


;;;
;;; Test basic case
;;;

(diag "Test basic case")

(defcoroutine test-case1 ()
  (yield 1)
  (yield 2)
  (coexit 3)
  (yield 4))

(let ((coroutine (make-coroutine 'test-case1)))
  (is (funcall coroutine) 1 "basic case 1")
  (is (funcall coroutine) 2 "basic case 2")
  (is (funcall coroutine) 3 "basic case 3")
  (is (funcall coroutine) nil "basic case 4"))

(with-coroutine (test-case1)
  (is (test-case1) 1 "basic case 5"))

(is-error (make-coroutine 'coroutine-not-exists) simple-error
          "basic case 6")


;;;
;;; Test coroutine which yields multiple values
;;;

(diag "Test coroutine which yields multiple values")

(defcoroutine test-case2 ()
  (yield (values 1 2))
  (yield (values 3 4)))

(let ((coroutine (make-coroutine 'test-case2)))
  (multiple-value-bind (x y) (funcall coroutine)
    (is x 1 "basic case 1")
    (is y 2 "basic case 2"))
  (multiple-value-bind (x y) (funcall coroutine)
    (is x 3 "basic case 3")
    (is y 4 "basic case 4")))


;;;
;;; Test coroutine which has parameters
;;;

(diag "Test coroutine which has parameters")

(defcoroutine test-case3 (foo)
  (is foo 1 "basic case 1")
  (yield)
  (is foo 2 "basic case 2"))

(let ((coroutine (make-coroutine 'test-case3)))
  (funcall coroutine 1)
  (funcall coroutine 2))


;;;
;;; Test coroutine with let binding
;;;

(diag "Test coroutine with let binding")

(defcoroutine test-case4 (foo)
  (is foo 1 "basic case 1")
  (let ((foo 2))
    (yield)
    ;; FOO is 3, not 2
    (is foo 3 "basic case 2")))

(let ((coroutine (make-coroutine 'test-case4)))
  (funcall coroutine 1)
  (funcall coroutine 3))


;;;
;;; Test coroutine which uses UNWIND-PROTECT
;;;

(diag "Test coroutine which uses UNWIND-PROTECT")

(defcoroutine test-case5 ()
  (unwind-protect
       (yield 1)
    (yield 2)))

(let ((coroutine (make-coroutine 'test-case5)))
  (is-error (funcall coroutine) simple-error "basic case 1"))


;;;
;;; Test coroutine which uses CATCH/THROW
;;;
  
(diag "Test coroutine which uses CATCH/THROW")

(defcoroutine test-case6 ()
  (catch 'result
    (yield 1)
    (throw 'result 1)
    (yield 2))
  (yield 3))

(let ((coroutine (make-coroutine 'test-case6)))
  (is (funcall coroutine) 1 "basic case 1")
  (is-error (funcall coroutine) error "basic case 2"))


(finalize)
