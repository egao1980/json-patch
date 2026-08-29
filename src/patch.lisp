(in-package #:json-patch)

;;; APPLY-PATCH is non-destructive: it rebuilds the nodes along each touched
;;; path and shares everything else. A reducer holding the previous state can
;;; therefore keep using it, which is what makes snapshot/delta state safe to
;;; hand to a UI that may still be rendering the old value.

(defun %copy-object (table)
  (let ((new (make-hash-table :test #'equal :size (max 1 (hash-table-count table)))))
    (maphash (lambda (k v) (setf (gethash k new) v)) table)
    new))

(defun %copy-array (vector)
  (make-array (length vector) :adjustable t :fill-pointer (length vector)
                              :initial-contents (coerce vector 'list)))

(defun %put (node token value)
  "Copy of NODE with TOKEN set to VALUE."
  (cond
    ((%object-p node)
     (let ((new (%copy-object node)))
       (setf (gethash token new) value)
       new))
    ((%array-p node)
     (let ((new (%copy-array node)))
       (setf (aref new (%array-index token (length node))) value)
       new))
    (t (%fail 'json-pointer-error "cannot set ~s on ~a" token (type-of node)))))

(defun %insert (node token value)
  "Copy of NODE with VALUE added at TOKEN: set for objects, insert for arrays."
  (cond
    ((%object-p node)
     (let ((new (%copy-object node)))
       (setf (gethash token new) value)
       new))
    ((%array-p node)
     (let* ((index (%array-index token (length node) :allow-end t))
            (old (coerce node 'list))
            (merged (append (subseq old 0 index) (list value) (subseq old index))))
       (make-array (length merged) :adjustable t :fill-pointer (length merged)
                                   :initial-contents merged)))
    (t (%fail 'json-pointer-error "cannot add ~s to ~a" token (type-of node)))))

(defun %delete (node token)
  "Copy of NODE with TOKEN removed."
  (cond
    ((%object-p node)
     (unless (nth-value 1 (gethash token node))
       (%fail 'json-pointer-error "cannot remove missing key ~s" token))
     (let ((new (%copy-object node)))
       (remhash token new)
       new))
    ((%array-p node)
     (let* ((index (%array-index token (length node)))
            (old (coerce node 'list))
            (merged (append (subseq old 0 index) (subseq old (1+ index)))))
       (make-array (length merged) :adjustable t :fill-pointer (length merged)
                                   :initial-contents merged)))
    (t (%fail 'json-pointer-error "cannot remove ~s from ~a" token (type-of node)))))

(defun %alter (node tokens fn)
  "Rebuild NODE along TOKENS, applying FN to the final container and last token."
  (let ((token (first tokens)))
    (if (null (rest tokens))
        (funcall fn node token)
        (%put node token (%alter (%child node token) (rest tokens) fn)))))

(defun %op-field (operation name &key required)
  (multiple-value-bind (value found) (gethash name operation)
    (when (and required (not found))
      (error 'json-patch-error
             :operation operation
             :message (format nil "operation is missing required member ~s" name)))
    (values value found)))

(defun %prefix-p (outer inner)
  "Is OUTER a prefix of INNER? Moving a location into its own child is illegal."
  (and (<= (length outer) (length inner))
       (every #'string= outer (subseq inner 0 (length outer)))))

(defun apply-operation (document operation)
  "Apply one RFC 6902 operation, returning the new document."
  (unless (%object-p operation)
    (error 'json-patch-error :operation operation
                             :message "each patch operation must be an object"))
  (let* ((op (%op-field operation "op" :required t))
         (path (parse-pointer (%op-field operation "path" :required t))))
    (flet ((value ()
             (multiple-value-bind (v found) (%op-field operation "value")
               ;; An explicit null is a legitimate value, so presence of the
               ;; member is what counts, not truthiness.
               (unless found
                 (error 'json-patch-error :operation operation
                                          :message (format nil "~a requires a value" op)))
               v))
           (from ()
             (parse-pointer (%op-field operation "from" :required t))))
      (cond
        ((equal op "add")
         (let ((new (value)))
           (if (null path) new (%alter document path (lambda (n tok) (%insert n tok new))))))

        ((equal op "replace")
         (let ((new (value)))
           (cond
             ((null path) new)
             (t
              ;; replace requires the target to exist, unlike add.
              (pointer-get document path)
              (%alter document path (lambda (n tok) (%put n tok new)))))))

        ((equal op "remove")
         (when (null path)
           (error 'json-patch-error :operation operation
                                    :message "cannot remove the whole document"))
         (%alter document path #'%delete))

        ((equal op "test")
         (let ((actual (handler-case (pointer-get document path)
                         (json-pointer-error ()
                           (error 'json-patch-test-failed
                                  :operation operation
                                  :message (format nil "test failed: ~a does not exist"
                                                   (unparse-pointer path)))))))
           (unless (json-equal actual (value))
             (error 'json-patch-test-failed
                    :operation operation
                    :message (format nil "test failed at ~a" (unparse-pointer path))))
           document))

        ((equal op "move")
         (let ((source (from)))
           (when (%prefix-p source path)
             (error 'json-patch-error :operation operation
                                      :message "cannot move a location into itself"))
           (let ((moved (pointer-get document source)))
             (let ((without (if (null source)
                                (error 'json-patch-error
                                       :operation operation
                                       :message "cannot move the whole document")
                                (%alter document source #'%delete))))
               (if (null path)
                   moved
                   (%alter without path (lambda (n tok) (%insert n tok moved))))))))

        ((equal op "copy")
         (let ((copied (pointer-get document (from))))
           (if (null path)
               copied
               (%alter document path (lambda (n tok) (%insert n tok copied))))))

        (t (error 'json-patch-error :operation operation
                                    :message (format nil "unknown op ~s" op)))))))

(defun apply-patch (document operations)
  "Apply RFC 6902 OPERATIONS in order, returning the new document.

   All or nothing: any failing operation -- including a `test` that does not
   match -- signals, and DOCUMENT is left untouched because nothing is mutated
   in place."
  (let ((result document))
    (map nil
         (lambda (operation) (setf result (apply-operation result operation)))
         (if (listp operations) operations (coerce operations 'list)))
    result))
