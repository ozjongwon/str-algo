(defsystem "str-algo"
  :long-name "str-algo"
  :version "0.0.1"
  :author "Jongwon Choi"
  :maintainer "Jongwon Choi"
  :mailto "oz.jongwon.choi@gmail.com"
  :license "MIT"
  :homepage "https://github.com/ozjongwon/str-algo"
  :bug-tracker "https://github.com/ozjongwon/str-algo/issues"
  :source-control "https://github.com/ozjongwon/str-algo.git"
  :depends-on ("alexandria")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "str-algo"
  :long-description "str-algo"
  :in-order-to ((test-op (test-op "str-algo/tests"))))

(defsystem "str-algo/tests"
  :author "Jongwon Choi"
  :license "MIT"
  :depends-on ("str-algo"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for str-algo"
  :perform (test-op (op c) (symbol-call :rove :run c)))
