(in-package :method-combination-types)

;; load this file to initialize and populate the new method combination system
;; Replaces all existing method combinations with combination types
;; also initalizes the standard-standard-method-combination

;; :cl src/org/armedbear/lisp/combination-types-init.lisp
;; ---------------------------
;; Standard method combination
;; ---------------------------

(let* ((class (find-class 'standard-standard-method-combination))
       (instance (make-instance class)))
  (setf (slot-value class 'type-name) 'standard)
  (setf (slot-value class '%constructor)
        (lambda (options)
          (when options
            (method-combination-error
             "The standard method combination accepts no options."))
          instance))
  (setf (gethash nil (method-combination-type-%cache class)) instance)
  (setf (gethash 'standard **method-combination-types**) class)
  
  ;;(substitute-method-combination instance *standard-method-combination*)
  ;;nil is not of type hash-table
  
  (setq *standard-method-combination* instance))


;; ------------------------------------
;; Built-in (short) method combinations
;; ------------------------------------

;;; The built-in method combination types as taken from page 1-31 of 88-002R.

(define-method-combination +      :identity-with-one-argument t)
(define-method-combination and    :identity-with-one-argument t)
(define-method-combination append :identity-with-one-argument nil)
(define-method-combination list   :identity-with-one-argument nil)
(define-method-combination max    :identity-with-one-argument t)
(define-method-combination min    :identity-with-one-argument t)
(define-method-combination nconc  :identity-with-one-argument t)
(define-method-combination progn  :identity-with-one-argument t)
(define-method-combination or     :identity-with-one-argument t)


#|
(let* ((or-class (find-method-combination-type 'or))
       (or-instance (when or-class
		      (funcall (method-combination-%constructor or-class)
			       '(:most-specific-first)))))
  (when (and or-class or-instance)
    (setf (gethash '(:most-specific-first)
                   (method-combination-type-%cache or-class))
          or-instance))
  (substitute-method-combination or-instance *or-method-combination*)
  (setq *or-method-combination* or-instance)
  )
|#
