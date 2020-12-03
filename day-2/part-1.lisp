(defun main ()
  (let ((passwords (parse-passwords "./input")))
    (part-1 passwords)))

(defun part-1 (passwords)
  (flet ((validp (args)
           (apply #'validp args)))
    (count-if #'validp passwords)))

(defun validp (min max character password)
  (loop
    with count = 0

    for c across password
    when (char= c character)
      do
         (incf count)
         (when (> count max)
           (return))

    finally (return (>= count min))))

(defun parse-passwords (file-path)
  (with-open-file (stream file-path)
    (loop
      for line = (read-line stream nil nil)
      while line
      collect (parse-line line))))

(defun parse-line (line)
  (let ((start 0))
    (flet ((read-upto (&optional char)
             (if (not char)
                 (subseq line start)
                 (let ((end (position char line :test #'char= :start start)))
                   (prog1 (subseq line start end)
                     (setf start (1+ end)))))))
      (let ((min (parse-integer (read-upto #\-)))
            (max (parse-integer (read-upto #\space)))
            (character (character (read-upto #\:)))
            (password (progn (incf start) (read-upto))))
        (list min max character password)))))
