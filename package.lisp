;;;;<title> CFFI-Objects</title>
;;;;<author>Roman Klochkov, monk@slavsoft.surgut.ru</author>
;;;;<date>2012</date>

;;;; Package definition for cffi-objects, 
;;;; that is a CFFI add-on, supporting GLib/GObject/GDK/GTK and similar objects

;;;<insert name="introduction"/>

;;;[ [[* Package definition *]]

(in-package #:cl-user)

#|<doc>
We unexport all symbols before [[defpackage]], because
CFFI-objects will be a drop-in replacemant for CFFI and I don't
want to export by hand all symbols exported by CFFI.
|#

(eval-when (:compile-toplevel :load-toplevel)
  (let ((p (find-package "CFFI-OBJECTS")))
    (when p
      (do-external-symbols (v p)
        (unexport (list v) p)))))

(defpackage #:cffi-objects
  (:use #:common-lisp #:cffi #+message-oo #:message-oo)
  (:export
   #:freeable-base
   ;; slots
   #:free-from-foreign
   #:free-to-foreign
   ;; freeable-base API
   #:free-sent-if-needed
   #:free-returned-if-needed
   #:free-ptr
   #:free-sent-ptr
   #:free-returned-ptr

   #:freeable
   #:freeable-out
   #:copy-from-foreign

   #:gconstructor

   #:object
   #:free-after
   #:find-object
   #:object-by-id
   #:initialize
   #:*objects*
   #:*objects-ids*
   #:object-class
   #:volatile
   ;; slots
   #:pointer
   ;; methods
   #:free

   #:*array-length*
   ;; types
   #:pstring
   #:pfunction
   #:cffi-object
   #:cffi-array
   #:cffi-null-array
   #:carray
   #:null-array
   #:string-array

   #:cffi-keyword
   #:cffi-pathname
   #:cffi-string

   #:struct
   #:cffi-struct
   #:new-struct
   #:free-struct

   #:defcstruct-accessors
   #:defcstruct*
   #:defbitaccessors

   ;; not for objects, but useful with cffi
   #:with-foreign-out
   #:with-foreign-outs
   #:with-foreign-outs-list

   ;; for creating object models on top of C objects
   #:pair
   #:setf-init
   #:init-slots
   #:save-setter
   #:remove-setter
   #:clear-setters))

;;;<doc> Now simply reexport all CFFI symbols.
(eval-when (:compile-toplevel :load-toplevel)
  (let ((cffi (find-package "CFFI")) 
        (cffi-objects (find-package "CFFI-OBJECTS")))
    (do-external-symbols (v cffi)
      (export (list v) cffi-objects))))

;;; <define name="introduction">
#|<doc>
[[* Introduction *]]

This document describes CFFI-objects: library, that extends CFFI to support
structures, objects and reference parameters.

Other alternatives are Virgil and FSBV/cffi-libffi. Virgil tend to marshall all
data back and forth. There are no support for structures as pointers.
FSBV is obsoleted by cffi-libffi. Libffi I dislike, because it gives another
layer of indirection (so make it slower) without new features (no bit fields
in structures).

So I made my own library. It gives the opportunity for programmer to
say which structures should be return values and how to save every
particular structure -- as pointer or as a lisp value.

Example:
\begin{alltt}
 (defcstruct* foo (bar :int) (baz :int))
 (defvar foo-as-ptr (make-instance 'foo :new-struct t))
 (defvar foo-as-value (make-instance 'foo))

 (defcfun foo-maker (struct foo))
 (defcfun proceed-foo :void (param (struct foo :out t)))
 (defcfun print-foo :void (param (struct foo)))
\end{alltt}

Here you can use either [[foo-as-ptr]] or [[foo-as-value]] in all functions.
[[Foo-as-ptr]] is faster, because it shouldn't convert values from Lisp to C
and back, but if foreign pointer is not considered stable (may be freed
by another c-function) or you don't want to control, when you need
to free foreign pointer, you should use [[foo-as-value]].

\include{redefines}
\include{freeable}
|#
;;; </define>



