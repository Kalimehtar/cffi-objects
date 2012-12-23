;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; struct.lisp --- CFFI wrapper for structs. We need to save on lisp
;;;                 side only values of struct field, not pointer on
;;;                 the struct to be able to garbage collect it
;;;
;;; Copyright (C) 2011, Roman Klochkov <kalimehtar@mail.ru>
;;;

(in-package :cffi-objects)

(defclass struct (object)
  ((value :documentation "plist ({field-name field-value}*)"))
  (:documentation "If value bound, use it, else use pointer.
Struct may be used in OBJECT cffi-type or STRUCT cffi-type"))

(defgeneric new-struct (class)
  (:method (class)
    (foreign-alloc class)))        

(defgeneric free-struct (class value)
  (:method (class value)
    (declare (ignore class))
 ;   (break)
    ;(format t "Free ~a ~a~%" class value)
    (foreign-free value)))

(defmethod gconstructor ((struct struct) &key new-struct &allow-other-keys)
  (if new-struct 
      (new-struct (class-name (class-of struct)))
      (progn 
        (setf (slot-value struct 'value) nil)
        (setf (slot-value struct 'free-after) nil)
        (null-pointer))))

(defun pair (maybe-pair)
  (if (consp maybe-pair) maybe-pair (cons maybe-pair maybe-pair)))

(defmacro defcstruct-accessors (class)
  "CLASS may be symbol = class-name = struct name,
or may be cons (class-name . struct-name)"
  (destructuring-bind (class-name . struct-name) (pair class)
    `(progn
       (clear-setters ,class-name)
       ,@(mapcar
          (lambda (x) 
           `(progn
              (unless (fboundp ',x)
                (defgeneric ,x (,class-name)))
              (defmethod ,x ((,class-name ,class-name))
                (if (slot-boundp ,class-name 'value)
                    (getf (slot-value ,class-name 'value) ',x)
                    (foreign-slot-value (pointer ,class-name)
                                        '(:struct ,struct-name) ',x)))
              (unless (fboundp '(setf ,x))
                (defgeneric (setf ,x) (val ,class-name)))
              (defmethod (setf ,x) (val (,class-name ,class-name))
                (if (slot-boundp ,class-name 'value)
                    (setf (getf (slot-value ,class-name 'value) ',x) val)
                    (setf (foreign-slot-value (pointer ,class-name) 
                                              '(:struct ,struct-name) ',x) 
                          val)))
              (save-setter ,class-name ,x)))
          (foreign-slot-names `(:struct ,struct-name))))))

(defmacro defbitaccessors (class slot &rest fields)
  (let ((pos 0))
    (flet ((build-field (field)
             (destructuring-bind (name type size) field
               (prog1 
                   `(progn
                      (unless (fboundp ',name)
                        (defgeneric ,name (,class)))
                      (defmethod ,name ((,class ,class))
                        (convert-from-foreign 
                         (ldb (byte ,size ,pos) (slot-value ,class ',slot))
                         ,type))
                      (unless (fboundp '(setf ,name))
                        (defgeneric (setf ,name) (value ,class)))
                      (defmethod (setf ,name) (value (,class ,class))
                        (setf (ldb (byte ,size ,pos) (slot-value ,class ',slot))
                              (convert-to-foreign value ,type))))
                 (incf pos size)))))
      (cons 'progn (mapcar #'build-field fields)))))


(defmacro defcstruct* (class &body body)
  `(progn
     (defclass ,class (struct) ())
     (defcstruct ,class ,@body)
     (defcstruct-accessors ,class)
     (init-slots ,class)))

(defun clos->struct (class object struct)
  (let ((default (gensym)))
    (mapc (lambda (slot) 
            (let ((val (getf (slot-value object 'value) slot default)))
              (unless (eq val default)
                (setf (foreign-slot-value struct (list :struct class) slot) 
                      val))))
          (foreign-slot-names (list :struct class)))))

(defun clos->new-struct (class object)
  (if (slot-boundp object 'value)
      (let ((res (new-struct class)))
        (clos->struct class object res)
        res)
      (pointer object)))

(defun struct->clos (class struct &optional object)
  "Translates pointer STRUCT to object OBJECT (if not supplied, then to new 
object).
  I suppose, that by default it should convert data from pointer to struct.
Only exception is the presence of OBJECT with not boundp value"
  (let ((%object (or object
                     (unless (null-pointer-p struct)
                       (make-instance class)))))
    (when %object
      (if (slot-boundp %object 'value)
          (progn
            (setf (slot-value %object 'value) nil)
            (unless (null-pointer-p struct)
              (dolist (slot (foreign-slot-names (list :struct class)))
                (setf (getf (slot-value %object 'value) slot) 
                      (foreign-slot-value struct (list :struct class) slot)))))
          (setf (pointer %object) struct))
      %object)))

(define-foreign-type cffi-struct (cffi-object freeable-out)
  ()
  (:actual-type :pointer))

(defmethod free-sent-ptr ((type cffi-struct) ptr place)
  (when (and (not (null-pointer-p ptr)) (slot-boundp place 'value))
    (free-struct (object-class type) ptr)))

(defmethod free-returned-ptr ((type cffi-struct) ptr)
  (unless (null-pointer-p ptr)
    (free-struct (object-class type) ptr)))


(defmethod foreign-type-size ((type cffi-struct))
  "Return the size in bytes of a foreign typedef."
  (foreign-type-size (list :struct (object-class type))))

(define-parse-method struct (class &rest rest)
  (apply #'make-instance 'cffi-struct :class class rest))

(defun %class (type value)
  (or (object-class type) (class-name (class-of value))))

(defmethod copy-from-foreign ((type cffi-object) ptr place)
  (when (slot-boundp place 'value)
    (struct->clos (%class type place) ptr place)))

;; cffi-object is not tyoo. It is for use struct with object designator
(defmethod translate-to-foreign ((value struct) (type cffi-object))
  (values (clos->new-struct (%class type value) value) value))

(defmethod translate-from-foreign (value (type cffi-struct))
  (struct->clos (object-class type) value))

;;; Allowed use with object designator
;; object == (struct nil)


;; to allow using array of structs
;; (eval-when (:compile-toplevel :load-toplevel :execute) 
;;    (unless (get 'mem-ref 'struct)
;;      (let ((old (fdefinition 'mem-ref)))
;;        (fmakunbound 'mem-ref)
;;        (defun mem-ref (ptr type &optional (offset 0))
;;          (let ((ptype (cffi::parse-type type)))
;;            (if (subtypep (type-of ptype) 'cffi-struct)
;;                (translate-from-foreign (inc-pointer ptr offset) ptype)
;;                (funcall old ptr type offset)))))
;;      (setf (get 'mem-ref 'struct) t)))

(defun struct-p (type)
  (and (consp type) (eq (car type) 'struct)))

(defun from-foreign (var type count)
  "VAR - symbol; type - symbol or list -- CFFI type; count -- integer"
  (if count
      (let ((res (make-array count)))
        (if (struct-p type)
            (dotimes (i count)
              (setf (aref res i)
                    (convert-from-foreign (mem-aptr var type i) type)))
            (dotimes (i count)
              (setf (aref res i)
                    (mem-aref var type i))))
        res)
      (mem-ref var type)))


(defmacro with-foreign-out ((var type &optional count) return-result &body body)
  "The same as WITH-FOREIGN-OBJECT, but returns value of object"
  (let ((value `(from-foreign ,var ,type ,count)))
  `(with-foreign-object (,var ,type ,@(when count (list count)))
     ,(if (eq return-result :ignore)
          `(progn ,@body ,value)
          `(let ((res ,@body))
             ,(ecase return-result
                     (:if-success `(when res ,value))
                     (:return `(values res ,value))))))))

(flet 
    ((make-with-foreign-outs (res-fun bindings return-result body)
       (let ((values-form (mapcar (lambda (x)
                                    (destructuring-bind 
                                          (var type &optional count) x
                                      `(from-foreign ,var ,type ,count)))
                                  bindings)))
         `(with-foreign-objects ,bindings
            ,(if (eq return-result :ignore)
                 `(progn ,@body (,res-fun ,@values-form))
                 `(let ((res ,@body))
                    ,(ecase return-result
                            (:if-success
                             `(when res (,res-fun ,@values-form)))
                            (:return
                              `(,res-fun res ,@values-form)))))))))
  
  (defmacro with-foreign-outs (bindings return-result &body body)
    "The same as WITH-FOREIGN-OBJECTS, but returns (values ...) 
of result and binded vars, RETURN-RESULT may be 
:RETURN - return result and values
:IF-SUCCESS - return values if result t
:IGNORE - discard result"
    (make-with-foreign-outs 'values bindings return-result body))

  (defmacro with-foreign-outs-list (bindings return-result &body body)
    "The same as WITH-FOREIGN-OBJECTS, but returns list"
    (make-with-foreign-outs 'list bindings return-result body)))
