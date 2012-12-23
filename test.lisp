(defpackage cffi-objects.test
  (:use cl cffi-objects))
(in-package cffi-objects.test)

(defcallback test-double :double ((x :double))
  (+ x 2))

(defun call-test-double ()
  (= 4 (foreign-funcall-pointer (callback test-double) () :double 2 :double)))

(defcstruct* tstruct
  (a :int)
  (b :int))

(defcallback test-struct :int ((x :pointer))
  (setf (mem-aref x :int 0) 10)
  (mem-aref x :int 1))

(defun call-test-struct ()
  (let ((s (make-instance 'tstruct)))
    (setf (a s) 1 (b s) 2)
    (prog1
        (foreign-funcall-pointer 
         (callback test-struct) () (struct tstruct :out t) s :int)
      (assert (= (a s) 10)))))

(assert (call-test-double))

(assert (= (call-test-struct) 2))

