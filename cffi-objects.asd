;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; cffi-object.asd --- ASDF system definition for cffi-objects
;;;
;;; Copyright (C) 2007, Roman Klochkov <kalimehtar@mail.ru>
;;;

(defpackage #:cffi-object-system
  (:use #:cl #:asdf))
(in-package #:cffi-object-system)

(defsystem cffi-objects
  :description "CFFI in-place replacement with object wrappers, structs and arrays"
  :author "Roman Klochkov <monk@slavsoft.surgut.ru>"
  :version "0.9"
  :license "BSD"
  :depends-on (cffi trivial-garbage)
  :components
  ((:file package)
   (:file redefines :depends-on (package freeable))
   (:file freeable :depends-on (package))
   (:file object :depends-on (freeable))
   (:file pfunction :depends-on (package))
   (:file setters :depends-on (package))
   (:file array :depends-on (struct))
   (:file struct :depends-on (object setters))))
