;;;; mblogic-clog.asd

(asdf:defsystem #:mblogic-clog
  :description "PLC Compiler/Interpreter ported from MBLogic Python system"
  :author "Gregory Brooks"
  :license "GPL-3.0"
  :version "0.1.0"
  :serial t
  :depends-on (#:cl-ppcre
               #:alexandria
               #:parse-number)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "data-table")
                             (:file "instructions")
                             (:file "parser")
                             (:file "math-lib")        ; Before compiler (for compile-time math parsing)
                             (:file "timer-counter")
                             (:file "table-ops")
                             (:file "compiler")        ; After libraries (uses them at compile-time)
                             (:file "interpreter"))))
  :in-order-to ((test-op (test-op #:mblogic-clog/test))))

(asdf:defsystem #:mblogic-clog/web
  :description "CLOG-based ladder diagram visualization for MBLogic-CLOG"
  :author "Gregory Brooks"
  :license "GPL-3.0"
  :depends-on (#:mblogic-clog
               #:clog
               #:bordeaux-threads)
  :serial t
  :components ((:module "src/web"
                :serial t
                :components ((:file "package")
                             (:file "ladder-render")
                             (:file "plc-data")
                             (:file "svg-render")
                             (:file "server")))))

(asdf:defsystem #:mblogic-clog/test
  :description "Test suite for mblogic-clog"
  :author "Gregory Brooks"
  :license "GPL-3.0"
  :depends-on (#:mblogic-clog
               #:fiveam)
  :serial t
  :components ((:module "test"
                :serial t
                :components ((:file "test-suite")
                             (:file "test-data-table")
                             (:file "test-parser")
                             (:file "test-compiler")
                             (:file "test-interpreter"))))
  :perform (test-op (o c) (symbol-call :fiveam '#:run!
                                       (find-symbol* '#:all-tests
                                                     :mblogic-cl-test))))

(asdf:defsystem #:mblogic-clog/web-test
  :description "Test suite for mblogic-clog web visualization"
  :author "Gregory Brooks"
  :license "GPL-3.0"
  :depends-on (#:mblogic-clog
               #:mblogic-clog/web
               #:mblogic-clog/test
               #:fiveam)
  :serial t
  :components ((:module "test"
                :components ((:file "test-ld-visualization"))))
  :perform (test-op (o c) (symbol-call :fiveam '#:run!
                                       (find-symbol* '#:ld-visualization-tests
                                                     :mblogic-cl-test))))

(asdf:defsystem #:mblogic-clog/builder
  :description "CLOG Builder for MBLogic-CLOG development"
  :author "Gregory Brooks"
  :license "GPL-3.0"
  :depends-on (#:mblogic-clog/web
               #:clog/tools)
  :components ())
