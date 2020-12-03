(defun main ()
  (let ((passwords (parse-passwords "./input")))
    (part-2 passwords)))

(defun part-2 (passwords)
  (flet ((validp (args)
           (apply #'validp args)))
    (count-if #'validp passwords)))

(defun validp (i j character password)
  (flet ((validp (i)
           (%validp (1- i) character password)))
    (let ((first (validp i))
          (second (validp j)))
      (when (= 1 (logxor first second))
        t))))

(defun %validp (i character password)
  (if (and (< i (length password))
           (char= (char password i) character))
      1
      0))

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
