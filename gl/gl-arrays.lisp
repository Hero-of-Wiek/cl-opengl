(in-package :gl)

(define-gl-array-format vertex
  (vertex :type :float :components (x y z)))

(defmacro with-gl-array-values ((var type &optional components count) values
                                &body body)
  ;; Might want to make it so component types can nest:
  ;; (x y z (w v x)) or whatever
  (nutils:once-only (values)
    `(with-gl-array (,var ,type :count (or ,count (length ,values)))
       (setf (get-arrays ,var ,components) ,values)
       ;; Might want to have something that lets us bind stuff
       ;;automatically in teh arglists, but not a top priority.
       ;;(bind-gl-vertex-array ,var)
       ,@body)))

(defun get-array (gl-vector &optional (index 0) components)
  (if components
      (apply #'vector
             (mapcar (lambda (component)
                       (glaref gl-vector index component))
                     components))
      (glaref gl-vector index)))

(defun (setf get-array) (item gl-vector &optional (index 0) components)
  (if components
      (loop for element across item
         for component in components
         do (setf (glaref gl-vector index component) element))
      (setf (glaref gl-vector index) item)))

(defun get-arrays (gl-array &optional components (count (gl::gl-array-size gl-array)))
  (loop for i from 0 to (1- count)
       collect (get-array gl-array i components)))

(defun (setf get-arrays) (vectors gl-array
                          &optional components (count (gl-array-size gl-array)))
    (assert (<= (length vectors) count) () "More vectors then ~D
Vectors are: ~A" count vectors)
  (loop for vector in vectors
     for i from 0
     do (setf (get-array gl-array i components) vector)))