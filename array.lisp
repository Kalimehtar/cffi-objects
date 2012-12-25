;;;
;;; array.lisp --- array
;;;
;;; Copyright (C) 2012, Roman Klochkov <monk@slavsoft.surgut.ru>
;;;

(in-package #:cffi-objects)

(defvar *array-length* (foreign-alloc :uint))

;; TODO: add with-pointer-to-vector-data optimization
(define-foreign-type cffi-array (freeable)
  ((element-type :initarg :type :accessor element-type))
  (:actual-type :pointer))

(define-parse-method carray (type &rest rest)
  (apply #'make-instance 'cffi-array :type type rest))

(defmethod translate-to-foreign (value (cffi-array cffi-array))
  (if (pointerp value)
      value
      (let* ((length (length value))
             (type (element-type cffi-array))
             (res (foreign-alloc type :count length)))
        (if (struct-p type)
            (dotimes (i length (values res t))
              (clos->struct (second type) (elt value i) (mem-aptr res type i)))
            (dotimes (i length (values res t))
              (setf (mem-aref res type i) (elt value i)))))))

(defmethod translate-from-foreign (ptr (cffi-array cffi-array))
  (let ((array-length (mem-ref *array-length* :uint)))
    (let* ((res (make-array array-length))
           (type (element-type cffi-array)))
      (if (struct-p type)
          (dotimes (i array-length res)
            (setf (aref res i) (convert-from-foreign 
                                (mem-aptr ptr (list :struct (second type)) i)
                                type)))
          (dotimes (i array-length res)
            (setf (aref res i) (mem-aref ptr type i)))))))

(define-foreign-type cffi-null-array (freeable)
  ((element-type :initarg :type :accessor element-type))
  (:actual-type :pointer))

(define-parse-method null-array (type &rest rest)
  (apply #'make-instance 'cffi-null-array :type type rest))

(defmethod translate-to-foreign (value (cffi-null-array cffi-null-array))
  (if (pointerp value)
      value
      (let* ((length (length value))
             (type (element-type cffi-null-array))
             (res (foreign-alloc type :count (+ 1 length))))
        (dotimes (i length (values res t))
          (setf (mem-aref res type i) (elt value i)))
        (setf (mem-aref res :pointer length) (null-pointer))
        res)))

(defmethod translate-from-foreign (ptr (cffi-null-array cffi-null-array))
  (let* ((res nil)
         (el-type (element-type cffi-null-array)))
    (do ((i 0 (+ i 1))) ((null-pointer-p (mem-aref ptr :pointer i)))
      (push (mem-aref ptr el-type i) res))
    (coerce (nreverse res) 'array)))

(defctype string-array (null-array :string) "Zero-terminated string array")

