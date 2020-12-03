(defun main ()
  (let ((expenses (parse-expenses "./input"))
        (target 2020))
    (part-1 expenses target)))

(defun parse-expenses (file-path)
  (with-open-file (stream file-path)
    (loop
      for line = (read-line stream nil nil)
      while line
      collect (parse-integer line))))

(defun part-1 (expenses target)
  (loop
    with complement->expense = (make-hash-table)

    for expense in expenses
    for complement = (- target expense)
    for complement-expense = (gethash expense complement->expense)

    if complement-expense
      return (* expense complement-expense)
    else do
      (setf (gethash complement complement->expense) expense)))
