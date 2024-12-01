(defun split-sequence (delimiter sequence)
  "Splits the sequence into a list of strings based on the delimiter, skipping empty strings."
  (let ((start 0)
        (result '()))
    (loop for i from 0 to (1- (length sequence))
          do (if (find (elt sequence i) (list delimiter #\Space))
                 (progn
                   (let ((segment (subseq sequence start i)))
                     (when (not (string= segment ""))
                       (push segment result)))
                   (setq start (1+ i)))))
    (let ((last-segment (subseq sequence start (length sequence))))
      (when (not (string= last-segment ""))
        (push last-segment result)))
    (nreverse result)))

(defun read-data (file-name)
  "Reads data from the file and returns a list of pairs of integers."
  (with-open-file (stream file-name)
    (let ((result '()))
      (loop for line = (read-line stream nil)
            while line
            do (let ((pair (map 'list 'parse-integer (split-sequence #\Tab line))))
                 (when (and pair (length pair) (= (length pair) 2))
                   (push pair result))))
      (nreverse result))))

(defun calculate-distance (list1 list2)
  "Calculates the total distance between two sorted lists."
  (reduce #'+ (mapcar #'(lambda (a b) (abs (- a b))) list1 list2)))

(defun main ()
  (let ((data (read-data "data.txt")))
    (let* ((left-list (mapcar #'first data))
           (right-list (mapcar #'second data))
           (sorted-left (sort left-list #'<))
           (sorted-right (sort right-list #'<)))
      (calculate-distance sorted-left sorted-right))))

(print (main))
