;;;;<author>Roman Klochkov, monk@slavsoft.surgut.ru</author>
;;;; Several ad-hoc CFFI types for real numbers, keywords and pathnames

(in-package #:cffi-objects)    

;;;[ [[* Float numbers, keywords, pathnames *]]

;;;[ <method expand-to-foreign-dyn>

#|<doc>
With plain CFFI language become slightly bondage. In lisp i have number,
real and integer, but in CFFI only floats and ints. So, for example,
this code is wrong
\begin{alltt}
 (defcfun sin :double (x :double))
 (sin 0)
should be
 (sin 0.0d0)
\end{alltt}

I think, that this is unnnecessary. So here is my hack (it is hack, because
it uses not exported symbols). It makes :double and :float to work, as if
corresponding parameters coerced to the needed type.
|#

(defmethod expand-to-foreign-dyn (value var body 
                                  (type cffi::foreign-built-in-type))
  `(let ((,var 
          ,(case (cffi::type-keyword type)
                 (:double `(coerce ,value 'double-float))
                 (:float `(coerce ,value 'single-float))
                 (t value))
           )) 
     ,@body))

;;;[ <class cffi-keyword>

#|<doc>
Constant-like strings often used in C, particulary in GTK. 
It is good to use lisp symbols in this case. 
So [[cffi-keyword]] type use symbol name as a string for C parameter. 
The name is downcased, because there are more string in downcase,
than in upcase (for not downcased string you still may put string as is).
Typical case for this type is using lisp keyword. So the name.
|#

(define-foreign-type cffi-keyword (freeable)
  ()
  (:simple-parser cffi-keyword)
  (:actual-type :pointer))

(defmethod translate-to-foreign ((value symbol) (type cffi-keyword))
  (foreign-string-alloc (string-downcase value)))

(defmethod translate-to-foreign ((value string) (type cffi-keyword))
  (foreign-string-alloc value))

(defmethod free-ptr ((type (eql 'cffi-keyword)) ptr)
  (foreign-string-free ptr))

;;;[ <class cffi-pathname>

#|<doc>
The same case for pathnames. If C function expect path to file, 
you may send it as a string or as a lisp pathname.
|#

(define-foreign-type cffi-pathname (freeable)
  ()
  (:simple-parser cffi-pathname)
  (:actual-type :string))

(defmethod translate-to-foreign ((value pathname) (type cffi-pathname))
  (convert-to-foreign (namestring value) :string))

(defmethod translate-to-foreign ((value string) (type cffi-pathname))
  (convert-to-foreign value :string))



