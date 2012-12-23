;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; object.lisp --- CFFI type PFUNCTION
;;;
;;; Copyright (C) 2007, Roman Klochkov <kalimehtar@mail.ru>
;;;

(in-package :cffi-objects)

(define-foreign-type pfunction ()
  ()
  (:actual-type :pointer)
  (:simple-parser pfunction)
  (:documentation "Takes a foreign pointer, keyword or a string.
Keyword or a string = name of C function, substituting #\- to #\_"))

(defmethod translate-to-foreign (value (type pfunction))
  (labels ((to-ptr (str)
             (declare (type string str))
             (foreign-symbol-pointer (substitute #\_ #\- str))))
    (etypecase value
      (string (to-ptr value))
      (keyword (to-ptr (string-downcase value)))
      (foreign-pointer value)
      (null (null-pointer)))))
