;;; part-1

(defun partition (from to upperp)
  (let ((midpoint (floor (+ to from) 2)))
    (if upperp
        (cons (1+ midpoint) to)
        (cons from midpoint))))

(defun partition-sequence (from to booleans)
  (car
   (reduce (lambda (range upperp)
             (destructuring-bind (from . to) range
               (partition from to upperp)))
           booleans
           :initial-value (cons from to))))

(defun chars->booleans (chars upper-char start &optional end)
  (flet ((upperp (char)
           (char= char upper-char)))
    (map 'vector #'upperp (subseq chars start end))))

(defun decode-row (boarding-pass)
  (let ((booleans (chars->booleans boarding-pass #\B 0 7)))
    (partition-sequence 0 127 booleans)))

(defun decode-column (boarding-pass)
  (let ((booleans (chars->booleans boarding-pass #\R 7)))
    (partition-sequence 0 7 booleans)))

(defun seat-id (boarding-pass)
  (let ((row (decode-row boarding-pass))
        (column (decode-column boarding-pass)))
    (+ column (* row 8))))

(defun read-boarding-passes (file-path)
  (with-open-file (stream file-path)
    (loop
      for line = (read-line stream nil nil)
      while line
      collect line)))

(defun part-1 ()
  (let ((boarding-passes (read-boarding-passes "./input")))
    (apply #'max (mapcar #'seat-id boarding-passes))))

;;; part-2

(defun part-2 ()
  (let ((boarding-passes (without-first-and-last-rows
                             (read-boarding-passes "./input"))))
    (find-missing-seat-id boarding-passes)))

(defun without-first-and-last-rows (boarding-passes)
  (flet ((not-first-or-last-row (boarding-pass)
           (let ((row (decode-row boarding-pass)))
             (unless (or (= row 0) (= row 127))
               (list boarding-pass)))))
    (mapcan #'not-first-or-last-row boarding-passes)))

(defun find-missing-seat-id (boarding-passes)
  (loop
    with seat-ids = (sort (map 'vector #'seat-id boarding-passes) #'<)

    for i from 1 below (length seat-ids)
    for seat-id = (elt seat-ids i)
    for previous-seat-id = (elt seat-ids (1- i))

    unless (= 1 (- seat-id previous-seat-id))
      return (1- seat-id)))
