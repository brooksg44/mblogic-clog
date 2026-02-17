;;;; test-suite.lisp
;;;; Phase 7: Main test framework and utilities

(defpackage #:mblogic-cl-test
  (:use #:cl #:mblogic-cl #:fiveam)
  (:export #:all-tests
           #:data-table-tests
           #:parser-tests
           #:compiler-tests
           #:interpreter-tests
           #:library-tests
           #:integration-tests
           #:ld-visualization-tests
           #:run-tests
           #:run-quick-tests
           #:run-web-tests))

(in-package #:mblogic-cl-test)

;;; ============================================================
;;; Test Suites Definition
;;; ============================================================

(def-suite all-tests
  :description "All MBLogic-CL tests")

(def-suite data-table-tests
  :description "Data table tests"
  :in all-tests)

(def-suite parser-tests
  :description "IL parser tests"
  :in all-tests)

(def-suite compiler-tests
  :description "IL compiler tests"
  :in all-tests)

(def-suite interpreter-tests
  :description "PLC interpreter tests"
  :in all-tests)

(def-suite library-tests
  :description "Phase 6 library tests (math, timers, table ops)"
  :in all-tests)

(def-suite integration-tests
  :description "Full system integration tests"
  :in all-tests)

(def-suite performance-tests
  :description "Performance benchmarks"
  :in all-tests)

;;; ============================================================
;;; Test Utilities
;;; ============================================================

(defun make-test-data-table ()
  "Create and initialize a test data table"
  (let ((dt (make-data-table)))
    (init-data-table dt)
    dt))

(defmacro with-test-data-table ((var) &body body)
  "Execute body with a fresh test data table bound to VAR"
  `(let ((,var (make-test-data-table)))
     ,@body))

(defun read-test-file (filename)
  "Read test IL program from test directory"
  (let ((path (merge-pathnames filename
                               (asdf:system-relative-pathname :mblogic-cl "test/"))))
    (with-open-file (in path :direction :input)
      (loop for line = (read-line in nil)
            while line
            collect line))))

(defun compile-test-program (source-string)
  "Compile an IL program from a string"
  (compile-il-string source-string))

(defun run-test-program (source-string &key (max-scans 1) inputs)
  "Compile and run an IL program, returning the interpreter"
  (let ((interp (run-il-string source-string :max-scans max-scans)))
    ;; Set any inputs before first scan
    (when inputs
      (dolist (input inputs)
        (destructuring-bind (addr . value) input
          (cond
            ((bool-addr-p addr)
             (set-bool-value interp addr value))
            ((word-addr-p addr)
             (set-word-value interp addr value))
            ((float-addr-p addr)
             (set-float-value interp addr value))))))
    interp))

(defun assert-bool-equal (interp addr expected &optional message)
  "Assert boolean value at address equals expected"
  (let ((actual (get-bool-value interp addr)))
    (is (eq actual expected)
        (or message
            (format nil "~A: expected ~A, got ~A" addr expected actual)))))

(defun assert-word-equal (interp addr expected &optional message)
  "Assert word value at address equals expected"
  (let ((actual (get-word-value interp addr)))
    (is (= actual expected)
        (or message
            (format nil "~A: expected ~A, got ~A" addr expected actual)))))

(defun assert-float-near (interp addr expected &optional (tolerance 0.001) message)
  "Assert float value at address is near expected"
  (let ((actual (get-float-value interp addr)))
    (is (< (abs (- actual expected)) tolerance)
        (or message
            (format nil "~A: expected ~A, got ~A" addr expected actual)))))

;;; ============================================================
;;; Test Runners
;;; ============================================================

(defun run-tests ()
  "Run all tests"
  (run! 'all-tests))

(defun run-quick-tests ()
  "Run quick subset of tests (excluding performance)"
  (run! 'data-table-tests)
  (run! 'parser-tests)
  (run! 'compiler-tests)
  (run! 'interpreter-tests)
  (run! 'library-tests))

(defun run-integration-tests ()
  "Run integration tests only"
  (run! 'integration-tests))

(defun run-performance-tests ()
  "Run performance benchmarks only"
  (run! 'performance-tests))

(defun run-web-tests ()
  "Run ladder diagram visualization tests only"
  (run! 'ld-visualization-tests))

;;; End of test-suite.lisp
