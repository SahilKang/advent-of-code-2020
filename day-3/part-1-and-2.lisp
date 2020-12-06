;;; part-1

(defun main ()
  (let ((lines (read-lines "./input"))
        (x-displacement 3)
        (y-displacement 1))
    (part-1 lines x-displacement y-displacement)))

(defun part-1 (lines x-displacement y-displacement)
  (let ((path (travel-by-toboggan lines x-displacement y-displacement)))
    (count-if #'treep path)))

(defun travel-by-toboggan (lines x-displacement y-displacement)
  (loop
    for y from y-displacement below (length lines) by y-displacement

    for line = (elt lines y)
    for x = x-displacement then (mod (+ x x-displacement) (length line))

    collect (char line x)))

(defun treep (cell)
  (char= cell #\#))

(defun read-lines (file-path)
  (with-open-file (stream file-path)
    (loop
      with vector = (make-array 0 :adjustable t :fill-pointer t)

      for line = (read-line stream nil nil)
      while line
      do (vector-push-extend line vector)

      finally (return vector))))

;;; part-2

(defun part-2 ()
  (loop
    with lines = (read-lines "./input")

    for (x y) in '((1 1) (3 1) (5 1) (7 1) (1 2))
    collect (part-1 lines x y) into tree-counts

    finally (return (apply #'* tree-counts))))
