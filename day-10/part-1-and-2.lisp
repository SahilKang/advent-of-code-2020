;;; part-1

(defun parse-adapters (file-path)
  (with-open-file (stream file-path)
    (loop
      for line = (read-line stream nil nil)
      while line
      collect (parse-integer line))))

(defun part-1 ()
  (loop
    with ones = 0
    and threes = 1
    and ordered = (sort (parse-adapters "./input") #'<)

    for i below (length ordered)
    for prev = 0 then (elt ordered (1- i))
    for cur = (elt ordered i)
    for diff = (- cur prev)

    if (= diff 1) do
      (incf ones)
    else if (= diff 3) do
      (incf threes)

    finally
       (return (* ones threes))))

;;; part-2

(defun get-candidates (remaining last)
  (loop
    for x being the hash-keys of remaining
    for diff = (- x last)

    when (member diff '(1 2 3))
      collect x))

(defun backtrack (remaining last target seen)
  (cond
    ((= target last) 1)
    ((zerop (hash-table-count remaining)) 0)
    (t (loop
         with count = 0

         for c in (get-candidates remaining last)
         for memo = (gethash c seen)

         if memo do
           (incf count memo)
         else do
           (remhash c remaining)
           (let ((more-count (backtrack remaining c target seen)))
             (setf (gethash c remaining) t
                   (gethash c seen) more-count
                   count (+ count more-count)))

         finally
            (return count)))))

(defun part-2 ()
  (let* ((adapters (parse-adapters "./input"))
         (target (apply #'max adapters))
         (remaining (make-hash-table)))
    (map nil
         (lambda (x)
           (setf (gethash x remaining) t))
         adapters)
    (backtrack remaining 0 target (make-hash-table))))
