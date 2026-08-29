(in-package #:json-patch)

(define-condition json-patch-error (error)
  ((message :initarg :message :initform "json patch error"
            :reader json-patch-error-message)
   (operation :initarg :operation :initform nil
              :reader json-patch-error-operation))
  (:report (lambda (c s) (write-string (json-patch-error-message c) s))))

(define-condition json-pointer-error (json-patch-error) ())

(define-condition json-patch-test-failed (json-patch-error) ()
  (:documentation "A `test` operation did not match. RFC 6902 makes a failed
   test an error that aborts the whole patch, not a boolean result."))

(defun %fail (type control &rest args)
  (error type :message (apply #'format nil control args)))

;;; RFC 6901 pointers. `~1` is a literal `/` and `~0` a literal `~`; the order
;;; matters -- unescaping `~0` first would turn `~01` into `/`.

(defun %unescape-token (token)
  (let ((out (make-string-output-stream))
        (i 0)
        (n (length token)))
    (loop while (< i n)
          do (let ((ch (char token i)))
               (cond
                 ((and (char= ch #\~) (< (1+ i) n) (char= (char token (1+ i)) #\1))
                  (write-char #\/ out) (incf i 2))
                 ((and (char= ch #\~) (< (1+ i) n) (char= (char token (1+ i)) #\0))
                  (write-char #\~ out) (incf i 2))
                 ((char= ch #\~)
                  (%fail 'json-pointer-error "dangling ~~ in pointer token ~s" token))
                 (t (write-char ch out) (incf i)))))
    (get-output-stream-string out)))

(defun %escape-token (token)
  (with-output-to-string (out)
    (loop for ch across token
          do (case ch
               (#\~ (write-string "~0" out))
               (#\/ (write-string "~1" out))
               (t (write-char ch out))))))

(defun parse-pointer (pointer)
  "RFC 6901 POINTER as a list of decoded tokens. \"\" is the whole document."
  (cond
    ((null pointer) nil)
    ((listp pointer) pointer)
    ((string= pointer "") nil)
    ((char/= (char pointer 0) #\/)
     (%fail 'json-pointer-error "pointer must be empty or start with /: ~s" pointer))
    (t
     (let ((tokens '())
           (start 1))
       (loop for i = (position #\/ pointer :start start)
             do (push (%unescape-token (subseq pointer start i)) tokens)
                (if i (setf start (1+ i)) (return)))
       (nreverse tokens)))))

(defun unparse-pointer (tokens)
  (if (null tokens)
      ""
      (format nil "~{/~a~}" (mapcar #'%escape-token tokens))))

;;; Node access

(defun %object-p (node) (hash-table-p node))

(defun %array-p (node)
  (and (vectorp node) (not (stringp node))))

(defun %array-index (token length &key allow-end)
  "TOKEN as an array index. `-` is the position past the last element, which is
   only meaningful for `add`."
  (cond
    ((string= token "-")
     (if allow-end
         length
         (%fail 'json-pointer-error "array index - is only valid for add")))
    ((and (plusp (length token))
          (every #'digit-char-p token)
          ;; RFC 6901: no leading zeros, so "01" is not an index.
          (or (string= token "0") (char/= (char token 0) #\0)))
     (let ((i (parse-integer token)))
       (if (<= 0 i (if allow-end length (1- length)))
           i
           (%fail 'json-pointer-error "array index ~a out of range" token))))
    (t (%fail 'json-pointer-error "not an array index: ~s" token))))

(defun %child (node token)
  (cond
    ((%object-p node)
     (multiple-value-bind (value found) (gethash token node)
       (unless found
         (%fail 'json-pointer-error "no such key: ~s" token))
       value))
    ((%array-p node)
     (aref node (%array-index token (length node))))
    (t (%fail 'json-pointer-error "cannot descend into ~a at ~s"
              (type-of node) token))))

(defun pointer-get (document pointer)
  "Value at POINTER. Signals JSON-POINTER-ERROR when the path does not exist."
  (let ((node document))
    (dolist (token (parse-pointer pointer) node)
      (setf node (%child node token)))))

(defun pointer-exists-p (document pointer)
  (handler-case (progn (pointer-get document pointer) t)
    (json-pointer-error () nil)))

;;; Structural comparison. Needed by `test`, and by any reducer wanting to know
;;; whether a patch actually changed anything.

(defun json-equal (a b)
  (cond
    ((and (%object-p a) (%object-p b))
     (and (= (hash-table-count a) (hash-table-count b))
          (block compare
            (maphash (lambda (k v)
                       (multiple-value-bind (other found) (gethash k b)
                         (unless (and found (json-equal v other))
                           (return-from compare nil))))
                     a)
            t)))
    ((and (%array-p a) (%array-p b))
     (and (= (length a) (length b))
          (every #'json-equal a b)))
    ((and (stringp a) (stringp b)) (string= a b))
    ((and (numberp a) (numberp b)) (= a b))
    (t (eql a b))))
