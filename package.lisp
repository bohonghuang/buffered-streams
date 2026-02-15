(defpackage buffered-streams
  (:use #:cl #:alexandria #:trivial-gray-streams)
  (:export
   #:buffered-character-input-stream
   #:buffered-binary-input-stream))

(in-package #:buffered-streams)

(defstruct stream-ring-buffer
  (stream (make-string-input-stream "") :type stream)
  (stream-position 0 :type non-negative-fixnum)
  (stream-start 0 :type non-negative-fixnum)
  (stream-end 0 :type non-negative-fixnum)
  (buffer (make-array 0) :type (simple-array * (*)))
  (buffer-offset 0 :type non-negative-fixnum))

(declaim (ftype (function (stream-ring-buffer &optional non-negative-fixnum) (values non-negative-fixnum)) stream-ring-buffer-fill-buffer))
(defun stream-ring-buffer-fill-buffer (stream-ring-buffer &optional (length (length (stream-ring-buffer-buffer stream-ring-buffer))))
  (with-accessors ((stream stream-ring-buffer-stream)
                   (stream-position stream-ring-buffer-stream-position)
                   (stream-start stream-ring-buffer-stream-start)
                   (stream-end stream-ring-buffer-stream-end)
                   (buffer stream-ring-buffer-buffer)
                   (buffer-offset stream-ring-buffer-buffer-offset))
      stream-ring-buffer
    (assert (<= length (length buffer)))
    (if (plusp length)
        (let ((write-index (mod (- stream-end buffer-offset) (length buffer))))
          (check-type write-index non-negative-fixnum)
          (let* ((write-length (min (- (length buffer) write-index) length))
                 (written (- (read-sequence buffer stream :start write-index :end (+ write-index write-length)) write-index)))
            (incf stream-end written)
            (let ((offset-start (- (- stream-end stream-start) (length buffer))))
              (when (plusp offset-start)
                (incf stream-start offset-start)))
            (if (< written write-length)
                written
                (+ written (stream-ring-buffer-fill-buffer stream-ring-buffer (- length write-length))))))
        (progn
          (assert (<= stream-start stream-position (1- stream-end)))
          0))))

(declaim (ftype (function (stream-ring-buffer &optional t) (values t)) stream-ring-buffer-read-one))
(defun stream-ring-buffer-read-one (stream-ring-buffer &optional (eof :eof))
  (with-accessors ((stream stream-ring-buffer-stream)
                   (stream-position stream-ring-buffer-stream-position)
                   (stream-start stream-ring-buffer-stream-start)
                   (stream-end stream-ring-buffer-stream-end)
                   (buffer stream-ring-buffer-buffer)
                   (buffer-offset stream-ring-buffer-buffer-offset))
      stream-ring-buffer
    (let ((buffer-length (length buffer)))
      (when (>= stream-position stream-end)
        (when (zerop (stream-ring-buffer-fill-buffer stream-ring-buffer (floor buffer-length 2)))
          (return-from stream-ring-buffer-read-one eof)))
      (prog1 (aref buffer (mod (- stream-position buffer-offset) (length buffer)))
        (incf stream-position)))))

(declaim (ftype (function (stream-ring-buffer non-negative-fixnum) t) stream-ring-buffer-seek))
(defun stream-ring-buffer-seek (stream-ring-buffer position)
  (with-accessors ((stream stream-ring-buffer-stream)
                   (stream-position stream-ring-buffer-stream-position)
                   (stream-start stream-ring-buffer-stream-start)
                   (stream-end stream-ring-buffer-stream-end)
                   (buffer stream-ring-buffer-buffer)
                   (buffer-offset stream-ring-buffer-buffer-offset))
      stream-ring-buffer
    (let ((buffer-length (length buffer)))
      (if (<= stream-start position (1- stream-end))
          (setf stream-position position)
          (let ((backtrack-length (min position (floor buffer-length 2))))
            (file-position stream (setf stream-start (setf stream-end (- position backtrack-length))))
            (setf stream-position position
                  buffer-offset stream-start)
            (stream-ring-buffer-fill-buffer stream-ring-buffer buffer-length))))))

(defclass buffered-input-stream (fundamental-input-stream)
  ((buffer :reader stream-buffer :type stream-ring-buffer)))

(defmethod stream-file-position ((stream buffered-input-stream))
  (stream-ring-buffer-stream-position (stream-buffer stream)))

(defmethod (setf stream-file-position) (new-position (stream buffered-input-stream))
  (stream-ring-buffer-seek (stream-buffer stream) new-position)
  new-position)

(defmethod open-stream-p ((stream buffered-input-stream))
  (open-stream-p (stream-ring-buffer-stream (stream-buffer stream))))

(defmethod close ((stream buffered-input-stream) &key abort)
  (close (stream-ring-buffer-stream (stream-buffer stream)) :abort abort))

(defclass buffered-character-input-stream (buffered-input-stream fundamental-character-input-stream)
  ())

(defmethod initialize-instance :after ((buffered-stream buffered-character-input-stream) &key (stream nil) (size 4096) &allow-other-keys)
  (assert (input-stream-p stream))
  (setf (slot-value buffered-stream 'buffer) (make-stream-ring-buffer
                                              :stream stream
                                              :buffer (make-array size :element-type 'character))))

(defmethod stream-read-char ((stream buffered-character-input-stream))
  (stream-ring-buffer-read-one (stream-buffer stream)))

(defmethod stream-unread-char ((stream buffered-character-input-stream) char)
  (declare (ignore char))
  (stream-ring-buffer-seek (stream-buffer stream) (1- (file-position stream))))

(defmethod stream-element-type ((stream buffered-character-input-stream))
  'character)

(defclass buffered-binary-input-stream (buffered-input-stream fundamental-binary-input-stream)
  ())

(defmethod initialize-instance :after ((buffered-stream buffered-binary-input-stream) &key (stream nil) (size 4096) &allow-other-keys)
  (assert (input-stream-p stream))
  (setf (slot-value buffered-stream 'buffer) (make-stream-ring-buffer
                                              :stream stream
                                              :buffer (make-array size :element-type '(unsigned-byte 8)))))

(defmethod stream-read-byte ((stream buffered-binary-input-stream))
  (stream-ring-buffer-read-one (stream-buffer stream)))

(defmethod stream-element-type ((stream buffered-binary-input-stream))
  '(unsigned-byte 8))
