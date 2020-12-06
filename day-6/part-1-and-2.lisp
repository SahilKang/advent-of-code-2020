;;; part-1

(defun parse-groups (file-path)
  (with-open-file (stream file-path)
    (loop
      with group = nil
      and groups = nil

      for line = (read-line stream nil nil)
      while line

      if (zerop (length line)) do
        (when group
          (push group groups)
          (setf group nil))
      else do
        (push line group)

      finally
         (when group
           (push group groups))
         (return groups))))

(defun count-yes-answers (group)
  (let ((yes-answers (make-hash-table)))
    (labels ((put-char (char)
               (setf (gethash char yes-answers) t))
             (put-string (string)
               (map nil #'put-char string)))
      (map nil #'put-string group))
    (hash-table-count yes-answers)))

(defun part-1 ()
  (let ((groups (parse-groups "./input")))
    (apply #'+ (mapcar #'count-yes-answers groups))))

;;; part-2

(defun count-yes-answers (group)
  (loop
    with candidates = (make-hash-table)

      initially
         (flet ((put-char (char)
                  (setf (gethash char candidates) t)))
           (map nil #'put-char (elt group 0)))

    for person in (cdr group)
    until (zerop (hash-table-count candidates))
    do (loop
         for candidate being the hash-keys of candidates
         unless (find candidate person :test #'char=)
           do (remhash candidate candidates))

    finally
       (return (hash-table-count candidates))))

(defun part-2 ()
  (let ((groups (parse-groups "./input")))
    (apply #'+ (mapcar #'count-yes-answers groups))))
