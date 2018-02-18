(defparameter *user-info* (make-hash-table :test 'equal))
(setf (gethash (list "raskin" :owner) *user-info*) t)
