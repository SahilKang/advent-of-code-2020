;;; part-1

(defun parse-numbers (file-path)
  (with-open-file (stream file-path)
    (loop
      for line = (read-line stream nil nil)
      while line
      collect (parse-integer line))))

(defun first-bad (numbers window-length)
  (loop
    with window = (subseq numbers 0 window-length)

    for n in (nthcdr window-length numbers)

    unless (can-sum-p window n)
      return n

    do (setf window (nconc (cdr window) (list n)))))

(defun can-sum-p (window n)
  (loop
    with hash-table = (make-hash-table)

    for x in window

    when (gethash x hash-table)
      return t

    do (setf (gethash (- n x) hash-table) t)

    finally
       (return nil)))

(defun part-1 ()
  (let ((numbers (parse-numbers "./input")))
    (first-bad numbers 25)))

;;; part-2

(defun find-run (numbers target)
  (loop
    with left = 0
    and sum = (elt numbers 0)

    for right from 1 below (length numbers)

    do
       (incf sum (elt numbers right))
       (loop
         while (and (> sum target)
                    (< left right))
         do (decf sum (elt numbers left))
            (incf left))

    when (= sum target)
      return (subseq numbers left (1+ right))))

(defun part-2 ()
  (let* ((numbers (parse-numbers "./input"))
         (bad (first-bad numbers 25))
         (run (find-run numbers bad))
         (min (apply #'min run))
         (max (apply #'max run)))
    (+ min max)))
