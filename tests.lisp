(defpackage #:cffi-objects.tests
  (:use #:cl #:cffi-objects #:hu.dwim.stefil))

(in-package #:cffi-objects.tests)

(defsuite* (test-suite
            :in root-suite
            :documentation "Testing CFFI-objects"))

(defcstruct* test
  (x :int))

(deftest test.carray ()
  (is (= (let ((obj (make-instance 'test)))
           (setf (x obj) 1)
           (x obj)) 1))
  (let ((obj (make-array 10)))
    (dotimes (i 10)
      (setf (aref obj i) 
            (let ((struct (make-instance 'test)))
              (setf (x struct) i)
              struct)))
    (is (every (lambda (a b) (= (x a) (x b)))
               obj
               (convert-from-foreign 
                (convert-to-foreign obj '(carray (struct test)))
                '(carray (struct test)))))))