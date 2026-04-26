(in-package #:buffered-streams)

(defstruct character-index-buffer
  (stream (make-string-input-stream "") :type stream)
  (index 0 :type non-negative-fixnum)
  (interval 1 :type non-negative-fixnum)
  (buffer (make-array 32 :element-type '(cons non-negative-fixnum non-negative-fixnum) :initial-element '(0 . 0))
   :type (simple-array (cons non-negative-fixnum non-negative-fixnum) (*))))

(declaim (ftype (function (character-index-buffer) (values non-negative-fixnum)) character-index-buffer-ratio))
(defun character-index-buffer-ratio (buffer)
  (let* ((buffer (character-index-buffer-buffer buffer))
         (index[0] (car (aref buffer 0)))
         (index[1] (car (aref buffer 1)))
         (index[2] (car (aref buffer 2)))
         (a (- index[1] index[0]))
         (b (- index[2] index[1])))
    (declare (type non-negative-fixnum index[0] index[1] index[2] a b))
    (if (= a b) 1 (floor a b))))

(defun copy-cons (src &optional (dest (cons nil nil)))
  (setf (car dest) (car src)
        (cdr dest) (cdr src))
  dest)

(defconstant +character-index-buffer-compact-ratio+ 2)

(defun character-index-buffer-compact (buffer)
  (let* ((default-interval (character-index-buffer-interval buffer))
         (ratio +character-index-buffer-compact-ratio+)
         (buffer (character-index-buffer-buffer buffer))
         (buffer-length (length buffer)))
    (declare (type non-negative-fixnum ratio buffer-length))
    (flet ((compact (start interval ratio)
             (loop :for j := start
                     :then (or (loop :for k :from j :below buffer-length
                                     :for index[k] :of-type non-negative-fixnum := (car (aref buffer k))
                                     :and index[k-1] :of-type non-negative-fixnum := (car (aref buffer (1- k)))
                                     :sum (- index[k] index[k-1]) :into current-interval :of-type non-negative-fixnum
                                     :when (<= expected-interval current-interval)
                                       :do (copy-cons (aref buffer k) (aref buffer i)) :and :return (1+ k))
                               (loop-finish))
                   :for i :from start :below buffer-length
                   :for expected-interval :of-type non-negative-fixnum := interval :then (max (floor expected-interval ratio) default-interval)
                   :finally
                      (loop :for k :from i :below buffer-length
                            :for l :from j
                            :do (copy-cons (aref buffer (min l (1- buffer-length))) (aref buffer k))))))
      (declare (ftype (function (non-negative-fixnum non-negative-fixnum non-negative-fixnum)) compact))
      (multiple-value-bind (start current-interval)
          (loop :for i :of-type non-negative-fixnum :from 2 :below buffer-length
                :for index[i-2] :of-type non-negative-fixnum := (car (aref buffer (- i 2)))
                :and index[i-1] :of-type non-negative-fixnum := (car (aref buffer (1- i)))
                :and index[i] :of-type non-negative-fixnum := (car (aref buffer i))
                :for current-interval :of-type non-negative-fixnum := (floor (- index[i-1] index[i-2]) ratio)
                :when (< (- index[i] index[i-1]) current-interval)
                  :return (values i current-interval)
                :when (> (- index[i] index[i-1]) current-interval)
                  :return (values (1- i) (* current-interval ratio ratio))
                :finally (return (values buffer-length current-interval)))
        (declare (type non-negative-fixnum start current-interval))
        (assert (< start buffer-length))
        (compact start current-interval ratio)))))

(defun character-index-buffer-update (character-index-buffer)
  (let ((stream (character-index-buffer-stream character-index-buffer))
        (interval (character-index-buffer-interval character-index-buffer))
        (index (character-index-buffer-index character-index-buffer))
        (buffer (character-index-buffer-buffer character-index-buffer)))
    (let ((cons (loop :for i :of-type non-negative-fixnum :from 1 :below (length buffer)
                      :for index[i-1] :of-type non-negative-fixnum := (car (aref buffer (1- i)))
                      :and index[i] :of-type non-negative-fixnum := (car (aref buffer i))
                      :for current-interval :of-type fixnum := (- index[i] index[i-1])
                      :when (or (< current-interval interval) (zerop index[i]))
                        :return (aref buffer i)
                      :finally
                         (character-index-buffer-compact character-index-buffer)
                         (return-from character-index-buffer-update (character-index-buffer-update character-index-buffer)))))
      (setf (car cons) index
            (cdr cons) (file-position stream)))))

(defun character-index-buffer-seek (character-index-buffer)
  (let ((stream (character-index-buffer-stream character-index-buffer))
        (index (character-index-buffer-index character-index-buffer))
        (buffer (character-index-buffer-buffer character-index-buffer)))
    (loop :for i :of-type non-negative-fixnum :from 1 :below (length buffer)
          :for (index[i-1] . position[i-1]) :of-type (non-negative-fixnum . non-negative-fixnum) := (aref buffer (1- i))
          :and (index[i] . nil) :of-type (non-negative-fixnum . non-negative-fixnum) := (aref buffer i)
          :when (and (>= index index[i-1]) (or (< index index[i]) (zerop index[i])))
            :return (loop :initially (let* ((length (- index index[i-1]))
                                            (sequence (make-array length)))
                                       (declare (dynamic-extent sequence))
                                       (file-position stream position[i-1])
                                       (read-sequence sequence stream)
                                       (setf (car (aref buffer i)) index
                                             (cdr (aref buffer i)) (file-position stream)))
                          :for j :from (1+ i) :below (length buffer)
                          :do (copy-cons '(0 . 0) (aref buffer j))))))

(defclass character-indexed-input-stream (fundamental-character-input-stream)
  ((buffer :reader stream-buffer :type character-index-buffer)))

(defmethod initialize-instance :after ((indexed-stream character-indexed-input-stream)
                                       &key (stream nil) (interval 1024) (size 32) &allow-other-keys)
  (assert (input-stream-p stream))
  (assert (subtypep (stream-element-type stream) 'character))
  (setf (slot-value indexed-stream 'buffer)
        (make-character-index-buffer
         :stream stream
         :interval interval
         :buffer (loop :with buffer := (make-array size :element-type '(cons non-negative-fixnum non-negative-fixnum) :initial-element '(0 . 0))
                       :for i :below (length buffer)
                       :do (setf (aref buffer i) (cons 0 0))
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
