;;; part-1

(defun parse-instructions (file-path)
  (with-open-file (stream file-path)
    (loop
      with instructions = (make-array 0 :adjustable t :fill-pointer t)

      for line = (read-line stream nil nil)
      while line

      for instruction = (parse-instruction line)
      do (vector-push-extend instruction instructions)

      finally
         (return instructions))))

(defun parse-instruction (line)
  (let ((op (parse-op line))
        (arg (parse-arg line)))
    (cons op arg)))

(defun parse-op (line)
  (let ((op (subseq line 0 3)))
    (cond
      ((string= op "nop") 'nop)
      ((string= op "acc") 'acc)
      ((string= op "jmp") 'jmp))))

(defun parse-arg (line)
  (parse-integer line :start 4))

(defun part-1 ()
  (loop
    with instructions = (parse-instructions "./input")
    and accumulator = 0
    and i = 0

    for (op . arg) = (elt instructions i)
    while op

    for delta-i = (ecase op
                   (nop 1)
                   (acc (prog1 1
                          (incf accumulator arg)))
                   (jmp arg))
    do
       (setf (elt instructions i) nil)
       (incf i delta-i)

    finally
       (return accumulator)))

;;; part-2

(defun get-answer (instructions)
  (loop
    with accumulator = 0
    and i = 0

    while (< i (length instructions))
    for (op . arg) = (elt instructions i)

    unless op
      return nil

    do
       (setf (elt instructions i) nil
             i (ecase op
                 (nop (1+ i))
                 (acc (prog1 (1+ i)
                        (incf accumulator arg)))
                 (jmp (+ i arg))))

    finally
       (return accumulator)))

(defun swap-op (instructions from)
  (loop
    with copy = (copy-seq instructions)

    for i from from below (length copy)
    for (op . arg) = (elt copy i)

    when (member op '(nop jmp)) do
      (let ((complement-op (ecase op
                             (nop 'jmp)
                             (jmp 'nop))))
        (setf (elt copy i) (cons complement-op arg))
        (return (cons copy (1+ i))))

    finally
       (return (cons copy i))))

(defun part-2 ()
  (loop
    with instructions = (parse-instructions "./input")

    for (modified . from) = (swap-op instructions 0) then (swap-op instructions from)

    when (get-answer modified)
      return it

    while (< from (length instructions))))
