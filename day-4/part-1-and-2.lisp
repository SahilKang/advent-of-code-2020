;;; part-1

(defun part-1 ()
  (let ((passports (parse-passports "./input")))
    (count-if #'validp passports)))

(defun validp (passport)
  (flet ((containsp (key)
           (getf passport key)))
    (every #'containsp #(:byr :iyr :eyr :hgt :hcl :ecl :pid))))

(defun parse-passports (file-path)
  (with-open-file (stream file-path)
    (loop
      with passport-lines = nil
      and passports = (make-array 0 :adjustable t :fill-pointer t)

      for line = (read-line stream nil nil)
      while line

      if (zerop (length line)) do
        (when passport-lines
          (vector-push-extend (parse-passport passport-lines) passports)
          (setf passport-lines nil))
      else do
        (push line passport-lines)

      finally
         (when passport-lines
           (vector-push-extend (parse-passport passport-lines) passports))
         (return passports))))

(defun parse-passport (lines)
  (parse-fields (join-lines lines)))

(defun join-lines (lines)
  (format nil "~{~A~^ ~}" lines))

(defun parse-fields (line)
  (flet ((parse-field (start)
           (parse-field line start)))
    (loop
      for (key value next-start) = (parse-field 0) then (parse-field next-start)
      nconc (list key value)
      while next-start)))

(defun parse-field (line start)
  (let* ((colon-position (position #\: line :test #'char= :start start))
         (space-position (position #\space line :test #'char= :start start))
         (key (make-keyword (subseq line start colon-position)))
         (value (subseq line (1+ colon-position) space-position)))
    (list key value (when space-position (1+ space-position)))))

(defun make-keyword (string)
  (intern (string-upcase string) 'keyword))

;;; part-2

(eval-when (:compile-toplevel :execute)
  (defparameter *predicates* nil))

(defmacro defpred (field &body body)
  (declare (keyword field)
           (cons body))
  (let ((function-name (intern (format nil "VALID-~A-P" field)))
        (field-value (intern (symbol-name field)))
        (passport (gensym)))
    (pushnew function-name *predicates*)
    `(defun ,function-name (,passport)
       (let ((,field-value (getf ,passport ,field)))
         (when ,field-value
           (handler-case
               ,@body
             (condition ()
               nil)))))))

(defpred :byr
  (multiple-value-bind (birth-year num-digits)
      (parse-integer byr)
    (and (= num-digits 4)
         (>= birth-year 1920)
         (<= birth-year 2002))))

(defpred :iyr
  (multiple-value-bind (issue-year num-digits)
      (parse-integer iyr)
    (and (= num-digits 4)
         (>= issue-year 2010)
         (<= issue-year 2020))))

(defpred :eyr
  (multiple-value-bind (expiration-year num-digits)
      (parse-integer eyr)
    (and (= num-digits 4)
         (>= expiration-year 2020)
         (<= expiration-year 2030))))

(defpred :hgt
  (multiple-value-bind (height start)
      (parse-integer hgt :junk-allowed t)
    (let ((suffix (subseq hgt start)))
      (cond
        ((string= suffix "cm")
         (and (>= height 150) (<= height 193)))
        ((string= suffix "in")
         (and (>= height 59) (<= height 76)))
        (t nil)))))

(defpred :hcl
  (when (and (= (length hcl) 7)
             (char= (char hcl 0) #\#))
    (flet ((validp (char)
             (or (digit-char-p char)
                 (let ((code (char-code char)))
                   (and (>= code (char-code #\a))
                        (<= code (char-code #\f)))))))
      (every #'validp (subseq hcl 1)))))

(defpred :ecl
  (find ecl #("amb" "blu" "brn" "gry" "grn" "hzl" "oth") :test #'string=))

(defpred :pid
  (= 9 (nth-value 1 (parse-integer pid))))

(macrolet
    ((defun-validp ()
       `(defun validp (passport)
          (and ,@(mapcar (lambda (predicate)
                           `(,predicate passport))
                         *predicates*)))))
  (defun-validp))

(defun part-2 ()
  (let ((passports (parse-passports "./input")))
    (count-if #'validp passports)))
