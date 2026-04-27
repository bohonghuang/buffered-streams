(in-package #:buffered-streams)

(defstruct character-index-buffer
  (stream (make-string-input-stream "") :type stream)
  (index 0 :type non-negative-fixnum)
  (buffer (make-array 32 :element-type '(cons non-negative-fixnum non-negative-fixnum) :initial-element '(0 . 0))
   :type (simple-array (cons non-negative-fixnum non-negative-fixnum) (*))))

(define-constant +character-index-buffer-start+ '(0 . 0) :test #'equal)
(define-constant +character-index-buffer-end+ '(#.most-positive-fixnum . #.most-positive-fixnum) :test #'equal)

(declaim (inline copy-cons))
(defun copy-cons (src &optional (dest (cons nil nil)))
  (setf (car dest) (car src)
        (cdr dest) (cdr src))
  dest)

(defun character-index-buffer-compact (character-index-buffer)
  (loop :with buffer :of-type (simple-array cons (*)) := (character-index-buffer-buffer character-index-buffer)
        :for i :of-type non-negative-fixnum :from 2 :below (length buffer)
        :for index[i-2] :of-type non-negative-fixnum := (car (aref buffer (- i 2)))
        :and index[i-1] :of-type non-negative-fixnum := (car (aref buffer (- i 1)))
        :and index[i] :of-type non-negative-fixnum := (car (aref buffer i))
        :for diff[i] :of-type non-negative-fixnum := (- index[i] index[i-1])
        :and diff[i-1] :of-type non-negative-fixnum := (- index[i-1] index[i-2])
        :when (>= diff[i] diff[i-1])
          :do (loop-finish)
        :finally
           (loop :for j :of-type non-negative-fixnum :from (min i (1- (length buffer))) :below (length buffer)
                 :do (copy-cons (aref buffer j) (aref buffer (1- j)))
                 :finally (copy-cons +character-index-buffer-end+ (aref buffer (1- (length buffer)))))))

(defun character-index-buffer-update (character-index-buffer)
  (let ((stream (character-index-buffer-stream character-index-buffer))
        (index (character-index-buffer-index character-index-buffer)))
    (let ((cons (loop :with buffer :of-type (simple-array cons (*)) := (character-index-buffer-buffer character-index-buffer)
                      :for i :of-type non-negative-fixnum :from (1- (length buffer)) :downto 1
                      :for index[i-1] :of-type non-negative-fixnum := (car (aref buffer (1- i)))
                      :and index[i] :of-type non-negative-fixnum := (car (aref buffer i))
                      :if (= index[i] most-positive-fixnum)
                        :when (/= index[i-1] most-positive-fixnum)
                          :return (aref buffer i) :end
                      :else
                        :if (= index[i] index[i-1])
                          :return (aref buffer i)
                      :finally
                         (character-index-buffer-compact character-index-buffer)
                         (return (aref buffer (1- (length buffer)))))))
      (setf (car cons) index
            (cdr cons) (file-position stream)))))

(defun character-index-buffer-seek (character-index-buffer)
  (loop :with buffer :of-type (simple-array cons (*)) := (character-index-buffer-buffer character-index-buffer)
        :and stream :of-type stream := (character-index-buffer-stream character-index-buffer)
        :and index :of-type non-negative-fixnum := (character-index-buffer-index character-index-buffer)
        :for i :of-type non-negative-fixnum :from 1 :below (length buffer)
        :for (index[i-1] . position[i-1]) :of-type (non-negative-fixnum . non-negative-fixnum) := (aref buffer (1- i))
        :and (index[i] . nil) :of-type (non-negative-fixnum . non-negative-fixnum) := (aref buffer i)
        :when (<= index[i-1] index (1- index[i]))
          :do (loop-finish)
        :finally
           (let* ((i (min i (1- (length buffer))))
                  (length (- index index[i-1]))
                  (sequence (make-array length)))
             (declare (type non-negative-fixnum i length)
                      (dynamic-extent sequence))
             (file-position stream position[i-1])
             (read-sequence sequence stream)
             (setf (car (aref buffer i)) index
                   (cdr (aref buffer i)) (file-position stream)))
           (loop :for j :of-type non-negative-fixnum :from (1+ i) :below (length buffer)
                 :do (copy-cons +character-index-buffer-end+ (aref buffer j)))))

(defclass character-indexed-input-stream (fundamental-character-input-stream)
  ((buffer :reader stream-buffer :type character-index-buffer)))

(defmethod initialize-instance :after ((indexed-stream character-indexed-input-stream)
                                       &key (stream nil) (size 32) &allow-other-keys)
  (assert (input-stream-p stream))
  (assert (subtypep (stream-element-type stream) 'character))
  (setf (slot-value indexed-stream 'buffer)
        (make-character-index-buffer
         :stream stream
         :buffer (loop :with buffer :of-type (simple-array cons (*))
                         := (make-array size :element-type '(cons non-negative-fixnum non-negative-fixnum)
                                             :initial-element +character-index-buffer-start+)
                       :for i :of-type non-negative-fixnum :from 1 :below (length buffer)
                       :do (setf (aref buffer i) (copy-cons +character-index-buffer-end+))
                       :finally (return buffer)))))

(defmethod stream-read-char ((stream character-indexed-input-stream))
  (let* ((index-buffer (stream-buffer stream))
         (source (character-index-buffer-stream index-buffer))
         (character (read-char source nil :eof)))
    (if (eq character :eof)
        :eof
        (progn
          (incf (character-index-buffer-index index-buffer))
          (character-index-buffer-update index-buffer)))
    character))

(defmethod stream-read-sequence ((stream character-indexed-input-stream) sequence start end &key)
  (let* ((index-buffer (stream-buffer stream))
         (source (character-index-buffer-stream index-buffer))
         (next-index (read-sequence sequence source :start start :end end))
         (read-count (- next-index start)))
    (when (plusp read-count)
      (incf (character-index-buffer-index index-buffer) read-count)
      (character-index-buffer-update index-buffer))
    next-index))

(defmethod stream-file-position ((stream character-indexed-input-stream))
  (character-index-buffer-index (stream-buffer stream)))

(defmethod (setf stream-file-position) (new-position (stream character-indexed-input-stream))
  (let ((index-buffer (stream-buffer stream)))
    (setf (character-index-buffer-index index-buffer) new-position)
    (character-index-buffer-seek index-buffer)
    new-position))

(defmethod stream-unread-char ((stream character-indexed-input-stream) char)
  (declare (ignore char))
  (file-position stream (1- (file-position stream))))

(defmethod stream-element-type ((stream character-indexed-input-stream))
  (stream-element-type (character-index-buffer-stream (stream-buffer stream))))

(defmethod open-stream-p ((stream character-indexed-input-stream))
  (open-stream-p (character-index-buffer-stream (stream-buffer stream))))

(defmethod close ((stream character-indexed-input-stream) &key abort)
  (close (character-index-buffer-stream (stream-buffer stream)) :abort abort))
