(defun main ()
  (let ((expenses (parse-expenses "./input"))
        (target 2020))
    (part-2 expenses target)))

(defun parse-expenses (file-path)
  (with-open-file (stream file-path)
    (loop
      with vector = (make-array 0 :adjustable t :fill-pointer t)

      for line = (read-line stream nil nil)
      while line
      do (vector-push-extend (parse-integer line) vector)

      finally (return vector))))

(defun part-2 (expenses target)
  (loop
    with sorted = (sort expenses #'<)

    for i below (length expenses)

    when (find-product sorted i target)
      return it))

(defun find-product (sorted-expenses start target)
  (loop
    with i = (1+ start)
    and j = (1- (length sorted-expenses))
    and start-value = (elt sorted-expenses start)
    with sub-target = (- target start-value)

    while (< i j)
    for left = (elt sorted-expenses i)
    for right = (elt sorted-expenses j)
    for sum = (+ left right)

    when (= sum sub-target)
      return (* start-value left right)

    if (< sum sub-target)
      do (incf i)
    else do
      (decf j)))
