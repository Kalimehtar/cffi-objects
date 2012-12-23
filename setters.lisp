;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; object.lisp --- Auto setters for foreign slots
;;;
;;; Copyright (C) 2007, Roman Klochkov <monk@slavsoft.surgut.ru>
;;;

(in-package #:cffi-objects)

(defmacro save-setter (class name)
  "Use this to register setters for SETF-INIT and INIT-SLOTS macro"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (pushnew ',name (get ',class 'slots))))

(defmacro remove-setter (class name)
  "Use this to unregister setters for SETF-INIT and INIT-SLOTS macro"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (setf (get ',class 'slots)
          (delete ',name (get ',class 'slots)))))

(defmacro clear-setters (class)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (setf (get ',class 'slots) nil)))

(defmacro setf-init (object &rest fields)
  "Should be used in constructors"
  `(progn
     ,@(mapcar (lambda (field-all)
                 (let ((field (if (consp field-all) 
                                  (first field-all) field-all))
                       (field-p (if (consp field-all)
                                    (third field-all) field-all)))
                   `(when ,field-p
                      (unless (initialized ,object ,field)
                        (setf (,field ,object) ,field)
                        (initialize ,object ',field)))))
               fields)))

(defun initialized (obj field)
  (find field (slot-value obj 'initialized)))

(defun initialize (obj fields)
  "Used when you need to mark, that FIELDS already initialized"
  (etypecase fields
    (list (dolist (field fields)
            (initialize obj field)))
    (symbol (push fields (slot-value obj 'initialized)))))

(defun name-p (name)
  (intern (format nil "~a-P" name) (symbol-package name)))

(defmacro init-slots (class &optional add-keys &body body)
  "For SETF-INIT auto-constructor"
  (let ((slots (mapcar (lambda (x) (list x nil (name-p x)))
                       (get class 'slots))))
    `(defmethod shared-initialize :after ((,class ,class) slot-names
                                          &key ,@slots ,@add-keys
                                          &allow-other-keys)
       (declare (ignore slot-names))
       (setf-init ,class ,@slots)
       ,@body)))
