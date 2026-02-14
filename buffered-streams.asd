(defsystem buffered-streams
  :version "0.1.0"
  :author "Bohong Huang <bohonghuang@qq.com>"
  :maintainer "Bohong Huang <bohonghuang@qq.com>"
  :license "Apache-2.0"
  :description "Buffered streams for efficient seeking and backtracking."
  :homepage "https://github.com/bohonghuang/buffered-streams"
  :bug-tracker "https://github.com/bohonghuang/buffered-streams/issues"
  :source-control (:git "https://github.com/bohonghuang/buffered-streams.git")
  :depends-on (#:alexandria #:trivial-gray-streams)
  :components ((:file "package"))
  :in-order-to ((test-op (test-op #:buffered-streams/test))))

(defsystem buffered-streams/test
  :depends-on (#:buffered-streams #:flexi-streams #:parachute)
  :pathname "test/"
  :components ((:file "package"))
  :perform (test-op (op c) (symbol-call '#:parachute '#:test (find-symbol (symbol-name '#:suite) '#:buffered-streams.test))))
