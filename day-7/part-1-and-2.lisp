;;; part-1

(defun parse-records (file-path)
  (with-open-file (stream file-path)
    (loop
      for line = (read-line stream nil nil)
      while line
      collect (parse-record line))))

(defun parse-record (line)
  (let ((parent (parse-parent line))
        (children (parse-children line)))
    (cons parent children)))

(defun parse-parent (line)
  (let ((bags-position (find-substring " bags" line)))
    (subseq line 0 bags-position)))

(defun parse-children (line)
  (let ((contain-position (find-substring "contain " line t)))
    (%parse-children (subseq line contain-position))))

(defun %parse-children (line)
  (unless (string= line "no other bags.")
    (let ((comma-position (position #\, line :test #'char=)))
      (if comma-position
          (cons (parse-child line) (%parse-children (subseq line (1+ comma-position))))
          (cons (parse-child line) nil)))))

(defun parse-child (line)
  (multiple-value-bind (number position)
      (parse-integer line :junk-allowed t)
    (let ((bag-position (find-substring " bag" line)))
      (cons (subseq line (1+ position) bag-position) number))))

(defun find-substring (substring string &optional endingp)
  (let ((position (search substring string :test #'string=)))
    (if endingp
        (+ position (length substring))
        position)))

(defun child->parents (records)
  (loop
    with child->parents = (make-hash-table :test #'equal)

    for (parent . children) in records
    do (loop
         for (child . count) in children
         do (push parent (gethash child child->parents)))

    finally
       (return child->parents)))

(defun determine-all-parents (child child->parents)
  (let ((parents (gethash child child->parents)))
    (when parents
      (reduce (lambda (parents child)
                (append parents (determine-all-parents child child->parents)))
              parents
              :initial-value parents))))

(defun part-1 ()
  (let* ((records (parse-records "./input"))
         (child->parents (child->parents records)))
    (length
     (remove-duplicates
      (determine-all-parents "shiny gold" child->parents)))))

;;; part-2

(defun part-2 ()
  (let ((records (parse-records "./input")))
    (1- (count-bags "shiny gold" records))))

(defun count-bags (bag records)
  (let ((children (cdr (assoc bag records :test #'string=))))
    (if (null children)
        1
        (reduce (lambda (count child-pair)
                  (+ count
                     (*
                      (cdr child-pair)
                      (count-bags (car child-pair) records))))
                children
                :initial-value 1))))
