
#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
 |
 | experiment module data structure
 |
 ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
 
(defstruct experiment-module
    next-stimulus)
    
#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
 |
 | experiment module functionality
 |
 ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
    
(defun next-stimulus()
    "returns the sequence number of the current/next stimulus"
    (let ((exp (get-module experiment)))
        (experiment-module-next-stimulus exp)))

(defun set-next-stimulus(value)
    "sets the sequence number of the next stimulus to the given value"
    (let ((exp (get-module experiment)))
        (setf (experiment-module-next-stimulus exp) value)
        value))
        
(defun inc-next-stimulus()
    "returns the sequence number of the current/next stimulus"
    (let ((exp (get-module experiment)))
        (incf (experiment-module-next-stimulus exp) 1)))

#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
 |
 | experiment module framework
 |
 ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#

(defun create-experiment-module (model-name)
    (declare (ignore model-name))
    (let ((exp (make-experiment-module)))
        (setf (experiment-module-next-stimulus exp) 0)
        exp))

(defun reset-experiment-module (exp)
    (declare (ignore exp)))

(defun delete-experiment-module (exp)
    (declare (ignore exp)))

(defun experiment-module-params (exp param)
    #| No parameters defined |#
    (declare (ignore exp param)))

(define-module-fct 'experiment
    '() #| no buffers |#
    '() #| no parameters |#
    :version "0.1"
    :documentation "A framework for experiments"
    :creation 'create-experiment-module
    :reset 'reset-experiment-module 
    :delete 'delete-experiment-module
    :params 'experiment-module-params
)