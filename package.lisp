(defpackage buffered-streams
  (:use #:cl #:alexandria #:trivial-gray-streams)
  (:export
   #:character-indexed-input-stream
   #:buffered-character-input-stream
   #:buffered-binary-input-stream))

(in-package #:buffered-streams)
