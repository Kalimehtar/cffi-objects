;;;;<author>Roman Klochkov, monk@slavsoft.surgut.ru</author>
;;;; Base classes for freeable and changeable CFFI types

(in-package #:cffi-objects)    

;;;[ [[* Memory freeing automation *]]

#|<doc>
Most of new CFFI types introduced in my library will live in the dynamic
memory. There are different policies of memory control in different languages
and libraries. Sometimes caller should clean memory (like in GTK), sometimes 
callee. 

In any case programmer should have possibility to say, if he would
like to free memory after function call. For example, in GTK it is common
for callback to return a newly-allocated string or structure, but in
parameters responsibility to clean memory remains to caller.

Another common option for any type is a flag, that it is out-paramter, 
so value of it should be translated back before freeing,

For uniformity with CFFI :string I chose :free-from-foreign and 
:free-to-foreign boolean flags to show, when we want to free memory. By default
"caller frees" model is used.
|#

;;;[ <class freeable-base>

#|<doc> I divided freeable functional to two classes: 
\begin{itemize}
\item [[freeable-base]] introduces all necessary fields and handlers
\item [[freeable]] have ready cffi-translator methods
|#

(define-foreign-type freeable-base ()
  ;; Should we free after translating from foreign?
  ((free-from-foreign :initarg :free-from-foreign
                      :reader fst-free-from-foreign-p
                      :initform nil :type boolean)
   ;; Should we free after translating to foreign?
   (free-to-foreign :initarg :free-to-foreign
                    :reader fst-free-to-foreign-p
                    :initform t :type boolean)))

#|<doc> 
Interface to [[freeable-base]] consists of three generics for describing,
how to free particular type: [[free-ptr]], [[free-sent-ptr]] and 
[[free-returned-ptr]], and two functions to use in CFFI translators:
[[free-returned-if-needed]] and [[free-sent-if-needed]].
|#

;;;[ <generic free-ptr (type ptr)>

#|<doc>
This generic describes, how to free an object with CFFI type [[type]] and
pointer [[ptr]]. As [[type]] should be a symbol, you should specialize
this generic with EQL specifier if your objects shouldn't be freed with
[[foreign-free].

One can ask, why normal specializer by type of object and [[object] as
a first parameter is not used. Such strange API is developed, 
because [[free-ptr]] is used in [[trivial-garbage:finalize]] and in some 
implementation (for example, SBCL) finalizer shouldn't have reference
to finalized object.

If you dislike it and you will not use finalizers, simply specialize or
redefine [[free-sent-ptr]] and [[free-returned-ptr]]
|#

(defgeneric free-ptr (type ptr)
  (:documentation "Called to free ptr, unless overriden free-sent-ptr 
or free-returned-ptr. TYPE should be specialized with EQL")
  (:method (type ptr) (foreign-free ptr)))

;;;[ <generic free-sent-ptr>

(defgeneric free-sent-ptr (cffi-type ptr param)
  (:method ((cffi-type freeable-base) ptr param)
    (unless (null-pointer-p ptr)
      (free-ptr (type-of cffi-type) ptr))))

;;;[ <generic free-returned-ptr>

(defgeneric free-returned-ptr (cffi-type ptr)
  (:method ((cffi-type freeable-base) ptr)
    (unless (null-pointer-p ptr)
      (free-ptr (type-of cffi-type) ptr))))

;;;[ <function free-sent-if-needed

(defun free-sent-if-needed (cffi-type ptr param)
  (when (fst-free-to-foreign-p cffi-type)
    (free-sent-ptr cffi-type ptr param)))

;;;[ <function free-returned-if-needed

(defun free-returned-if-needed (cffi-type ptr)
  (when (fst-free-from-foreign-p cffi-type)
    (free-returned-ptr cffi-type ptr)))

;;;[ <class freeable>

(defclass freeable (freeable-base) ()
  (:documentation "Mixing to auto-set translators"))



(defmethod free-translated-object :after (ptr (type freeable) param)
  (free-sent-if-needed type ptr param))

(defmethod translate-from-foreign :after (ptr (type freeable))
  (free-returned-if-needed type ptr))

;;;[ <class freeable-out>

(define-foreign-type freeable-out (freeable)
  ((out :accessor object-out :initarg :out :initform nil
        :documentation "This is out param (for fill in foreign side)"))
  (:documentation "For returning data in out params.
If OUT is t, then translate-to-foreign MUST return (values ptr place)"))

(defgeneric copy-from-foreign (type ptr place)
  (:documentation "Transfers data from pointer PTR to PLACE"))

(defmethod free-translated-object :before (ptr (type freeable-out) place)
  (when (object-out type)
    (copy-from-foreign type ptr place)))
