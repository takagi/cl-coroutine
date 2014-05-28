#|
  This file is a part of cl-coroutine project.
  Copyright (c) 2014 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-coroutine
  (:use :cl :cl-cont)
  (:export :defcoroutine
           :yield
           :coexit
           :make-coroutine
           :with-coroutine)
  (:import-from :alexandria
                :with-gensyms))
(in-package :cl-coroutine)


;;;
;;; DEFCOROUTINE macro
;;;

(defmacro defcoroutine (name (&optional arg) &body body)
  (if arg
      (defcoroutine/arg name arg body)
      (defcoroutine/no-arg name body)))

(defun defcoroutine/arg (name arg body)
  (alexandria:with-gensyms (cont)
    `(setf (get ',name 'make-coroutine)
           #'(lambda ()
               (let (,cont)
                 #'(lambda (,arg)
                     (declare (ignorable ,arg))
                     (if ,cont
                       (funcall ,cont ,arg)
                       (cl-cont:with-call/cc
                         (macrolet ((yield (&optional result)
                                      (let ((cc (gensym)))
                                        `(setf ,',arg
                                               (cl-cont:let/cc ,cc
                                                 (setf ,',cont ,cc)
                                                 ,result))))
                                    (coexit (&optional result)
                                      `(cl-cont:let/cc _
                                         (declare (ignorable _))
                                         (setf ,',cont
                                               #'(lambda (_)
                                                   (declare (ignorable _))
                                                   (values)))
                                         ,result)))
                           ,@body
                           (coexit nil))))))))))

(defun defcoroutine/no-arg (name body)
  (alexandria:with-gensyms (cont)
    `(setf (get ',name 'make-coroutine)
           #'(lambda ()
               (let (,cont)
                 #'(lambda ()
                     (if ,cont
                       (funcall ,cont)
                       (cl-cont:with-call/cc
                         (macrolet ((yield (&optional result)
                                      (let ((cc (gensym)))
                                        `(cl-cont:let/cc ,cc
                                           (setf ,',cont ,cc)
                                           ,result)))
                                    (coexit (&optional result)
                                      `(cl-cont:let/cc _
                                         (declare (ignorable _))
                                         (setf ,',cont
                                               #'(lambda ()
                                                   (values)))
                                         ,result)))
                           ,@body
                           (coexit nil))))))))))


;;;
;;; MAKE-COROUTINE function
;;;

(defun make-coroutine (name)
  (let ((func (get name 'make-coroutine)))
    (unless func
      (error "The coroutine ~S is undefined." name))
    (funcall func)))


;;;
;;; WITH-COROUTINE macro
;;;

(defmacro with-coroutine ((name) &body body)
  (with-gensyms (coroutine)
    `(let ((,coroutine (make-coroutine ',name)))
       (macrolet ((,name (&rest args)
                     `(funcall ,',coroutine ,@args)))
         ,@body))))