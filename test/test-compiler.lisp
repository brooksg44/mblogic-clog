;;;; test-compiler.lisp
;;;; Phase 7: IL compiler tests

(in-package #:mblogic-cl-test)

(in-suite compiler-tests)

;;; ============================================================
;;; Basic Compilation Tests
;;; ============================================================

(test compile-simple-program
  "Test compiling simple program"
  (let ((compiled (compile-il-string "NETWORK 1
STR X1
OUT Y1
")))
    (is (not (null compiled)))
    (is (not (null (program-main-function compiled))))))

(test compile-multiple-networks
  "Test compiling multiple networks"
  (let ((compiled (compile-il-string "NETWORK 1
STR X1
OUT Y1

NETWORK 2
STR X2
OUT Y2

NETWORK 3
STR X3
OUT Y3
")))
    (is (not (null compiled)))
    (is (not (null (program-main-function compiled))))))

;;; ============================================================
;;; Boolean Logic Compilation
;;; ============================================================

(test compile-str-and-out
  "Test STR and OUT compilation"
  (let* ((source "NETWORK 1
STR X1
OUT Y1
")
         (interp (run-il-string source :max-scans 1)))
    ;; X1 false -> Y1 false
    (is (null (get-bool-value interp "Y1")))
    ;; Set X1 true and run again
    (set-bool-value interp "X1" t)
    (step-scan interp)
    (is (eq t (get-bool-value interp "Y1")))))

(test compile-strn
  "Test STRN (NOT) compilation"
  (let* ((source "NETWORK 1
STRN X1
OUT Y1
")
         (interp (run-il-string source :max-scans 1)))
    ;; X1 false -> Y1 true (inverted)
    (is (eq t (get-bool-value interp "Y1")))
    ;; Set X1 true
    (set-bool-value interp "X1" t)
    (step-scan interp)
    (is (null (get-bool-value interp "Y1")))))

(test compile-and-or
  "Test AND and OR compilation"
  (let* ((source "NETWORK 1
STR X1
AND X2
OUT Y1

NETWORK 2
STR X3
OR X4
OUT Y2
")
         (interp (run-il-string source :max-scans 1)))
    ;; AND test: both false
    (is (null (get-bool-value interp "Y1")))
    ;; OR test: both false
    (is (null (get-bool-value interp "Y2")))

    ;; Set X1=T, X2=F -> Y1 still false
    (set-bool-value interp "X1" t)
    (step-scan interp)
    (is (null (get-bool-value interp "Y1")))

    ;; Set X1=T, X2=T -> Y1 true
    (set-bool-value interp "X2" t)
    (step-scan interp)
    (is (eq t (get-bool-value interp "Y1")))

    ;; Set X3=T, X4=F -> Y2 true (OR)
    (set-bool-value interp "X3" t)
    (step-scan interp)
    (is (eq t (get-bool-value interp "Y2")))))

(test compile-andstr-orstr
  "Test ANDSTR and ORSTR compilation"
  (let* ((source "NETWORK 1
STR X1
STR X2
ANDSTR
OUT Y1

NETWORK 2
STR X3
STR X4
ORSTR
OUT Y2
")
         (interp (run-il-string source :max-scans 1)))
    ;; ANDSTR: both stacks false -> false
    (is (null (get-bool-value interp "Y1")))
    ;; ORSTR: both stacks false -> false
    (is (null (get-bool-value interp "Y2")))

    ;; Set X1=T, X2=T -> Y1 true
    (set-bool-value interp "X1" t)
    (set-bool-value interp "X2" t)
    (step-scan interp)
    (is (eq t (get-bool-value interp "Y1")))

    ;; Set X3=T -> Y2 true (OR)
    (set-bool-value interp "X3" t)
    (step-scan interp)
    (is (eq t (get-bool-value interp "Y2")))))

;;; ============================================================
;;; Output Instructions Compilation
;;; ============================================================

(test compile-set-rst
  "Test SET and RST compilation"
  (let* ((source "NETWORK 1
STR X1
SET C1

NETWORK 2
STR X2
RST C1
")
         (interp (run-il-string source :max-scans 1)))
    ;; Initially C1 is false
    (is (null (get-bool-value interp "C1")))

    ;; Set X1=T -> C1 latched on
    (set-bool-value interp "X1" t)
    (step-scan interp)
    (is (eq t (get-bool-value interp "C1")))

    ;; Clear X1 -> C1 stays on (latched)
    (set-bool-value interp "X1" nil)
    (step-scan interp)
    (is (eq t (get-bool-value interp "C1")))

    ;; Set X2=T -> C1 reset
    (set-bool-value interp "X2" t)
    (step-scan interp)
    (is (null (get-bool-value interp "C1")))))

(test compile-multiple-outputs
  "Test multiple outputs on single instruction"
  (let* ((source "NETWORK 1
STR X1
OUT Y1 Y2 Y3
")
         (interp (run-il-string source :max-scans 1)))
    (set-bool-value interp "X1" t)
    (step-scan interp)
    (is (eq t (get-bool-value interp "Y1")))
    (is (eq t (get-bool-value interp "Y2")))
    (is (eq t (get-bool-value interp "Y3")))))

;;; ============================================================
;;; Comparison Instructions Compilation
;;; ============================================================

(test compile-stre
  "Test STRE (equal) compilation"
  (let* ((source "NETWORK 1
STR SC1
COPY 100 DS1
COPY 100 DS2

NETWORK 2
STRE DS1 DS2
OUT Y1

NETWORK 3
STRE DS1 50
OUT Y2
")
         (interp (run-il-string source :max-scans 1)))
    (is (eq t (get-bool-value interp "Y1")) "DS1=DS2 should be true")
    (is (null (get-bool-value interp "Y2")) "DS1=50 should be false")))

(test compile-strgt-strlt
  "Test STRGT and STRLT compilation"
  (let* ((source "NETWORK 1
STR SC1
COPY 100 DS1

NETWORK 2
STRGT DS1 50
OUT Y1

NETWORK 3
STRLT DS1 200
OUT Y2

NETWORK 4
STRGT DS1 100
OUT Y3

NETWORK 5
STRLT DS1 50
OUT Y4
")
         (interp (run-il-string source :max-scans 1)))
    (is (eq t (get-bool-value interp "Y1")) "100 > 50 should be true")
    (is (eq t (get-bool-value interp "Y2")) "100 < 200 should be true")
    (is (null (get-bool-value interp "Y3")) "100 > 100 should be false")
    (is (null (get-bool-value interp "Y4")) "100 < 50 should be false")))

;;; ============================================================
;;; Data Movement Compilation
;;; ============================================================

(test compile-copy
  "Test COPY compilation"
  (let* ((source "NETWORK 1
STR SC1
COPY 12345 DS100
COPY DS100 DS101
")
         (interp (run-il-string source :max-scans 1)))
    (is (= 12345 (get-word-value interp "DS100")))
    (is (= 12345 (get-word-value interp "DS101")))))

(test compile-cpyblk
  "Test CPYBLK compilation"
  (let* ((source "NETWORK 1
STR SC1
COPY 10 DS1
COPY 20 DS2
COPY 30 DS3
CPYBLK DS1 DS3 DS100
")
         (interp (run-il-string source :max-scans 1)))
    (is (= 10 (get-word-value interp "DS100")))
    (is (= 20 (get-word-value interp "DS101")))
    (is (= 30 (get-word-value interp "DS102")))))

(test compile-fill
  "Test FILL compilation"
  (let* ((source "NETWORK 1
STR SC1
FILL 42 DS100 DS104
")
         (interp (run-il-string source :max-scans 1)))
    (is (= 42 (get-word-value interp "DS100")))
    (is (= 42 (get-word-value interp "DS101")))
    (is (= 42 (get-word-value interp "DS102")))
    (is (= 42 (get-word-value interp "DS103")))
    (is (= 42 (get-word-value interp "DS104")))))

;;; ============================================================
;;; Math Compilation
;;; ============================================================

(test compile-mathdec
  "Test MATHDEC compilation"
  (let* ((source "NETWORK 1
STR SC1
COPY 10 DS1
COPY 20 DS2
MATHDEC DS100 0 DS1 + DS2
MATHDEC DS101 0 DS1 * DS2
MATHDEC DS102 0 (DS1 + DS2) * 2
")
         (interp (run-il-string source :max-scans 1)))
    (is (= 30 (get-word-value interp "DS100")) "10 + 20 = 30")
    (is (= 200 (get-word-value interp "DS101")) "10 * 20 = 200")
    (is (= 60 (get-word-value interp "DS102")) "(10 + 20) * 2 = 60")))

(test compile-mathhex
  "Test MATHHEX compilation"
  (let* ((source "NETWORK 1
STR SC1
COPY 255 DH1
COPY 15 DH2
MATHHEX DH100 0 DH1 AND DH2
MATHHEX DH101 0 DH1 OR DH2
")
         (interp (run-il-string source :max-scans 1)))
    (is (= 15 (get-word-value interp "DH100")) "255 AND 15 = 15")
    (is (= 255 (get-word-value interp "DH101")) "255 OR 15 = 255")))

;;; ============================================================
;;; Subroutine Compilation
;;; ============================================================

(test compile-subroutine-call
  "Test subroutine CALL and RT compilation"
  (let* ((source "NETWORK 1
STR SC1
CALL TestSub

NETWORK 2
END

SBR TestSub
NETWORK 1
STR SC1
COPY 9999 DS100
RT
")
         (interp (run-il-string source :max-scans 1)))
    (is (= 9999 (get-word-value interp "DS100"))
        "Subroutine should have set DS100")))

(test compile-conditional-return
  "Test RTC (conditional return) compilation"
  (let* ((source "NETWORK 1
STR SC1
CALL TestSub

NETWORK 2
END

SBR TestSub
NETWORK 1
STR SC1
COPY 1 DS100
RTC

NETWORK 2
STR SC1
COPY 2 DS100
RT
")
         (interp (run-il-string source :max-scans 1)))
    ;; SC1 is always true, so RTC returns early
    (is (= 1 (get-word-value interp "DS100"))
        "Should return after first COPY (RTC triggered)")))

;;; ============================================================
;;; Control Flow Compilation
;;; ============================================================

(test compile-end
  "Test END compilation"
  (let* ((source "NETWORK 1
STR SC1
OUT Y1

NETWORK 2
END

NETWORK 3
STR SC1
OUT Y2
")
         (interp (run-il-string source :max-scans 1)))
    (is (eq t (get-bool-value interp "Y1")) "Y1 before END should be set")
    (is (null (get-bool-value interp "Y2")) "Y2 after END should not be set")))

(test compile-endc
  "Test ENDC (conditional end) compilation"
  (let* ((source "NETWORK 1
STR X1
ENDC

NETWORK 2
STR SC1
OUT Y1
")
         (interp (run-il-string source :max-scans 1)))
    ;; X1 is false, so ENDC doesn't trigger
    (is (eq t (get-bool-value interp "Y1")) "Y1 should be set when X1=false")

    ;; Set X1=true, ENDC triggers
    (set-bool-value interp "X1" t)
    (set-bool-value interp "Y1" nil)  ; Reset Y1
    (step-scan interp)
    (is (null (get-bool-value interp "Y1")) "Y1 should not be set when X1=true (ENDC)")))

;;; ============================================================
;;; FOR/NEXT Loop Compilation Tests
;;; ============================================================

(test compile-for-next-basic
  "Test basic FOR/NEXT loop compilation"
  (let* ((source "NETWORK 1
STR SC1
FOR 5
COPY 1 DS1
NEXT
")
         (interp (run-il-string source :max-scans 1)))
    ;; SC1 is true, so loop runs 5 times, but only last iteration
    ;; value is visible (overwrites same register)
    (is (= 1 (get-word-value interp "DS1")))))

(test compile-for-next-accumulator
  "Test FOR/NEXT loop with accumulating counter"
  (let* ((source "NETWORK 1
STR SC1
COPY 0 DS1

NETWORK 2
STR SC1
FOR 10
MATHDEC DS1 0 DS1 + 1
NEXT
")
         (interp (run-il-string source :max-scans 1)))
    (is (= 10 (get-word-value interp "DS1")) "Loop should run 10 times")))

(test compile-for-next-conditional
  "Test FOR/NEXT loop that doesn't execute when condition is false"
  (let* ((source "NETWORK 1
STR SC1
COPY 0 DS1

NETWORK 2
STR SC2
FOR 100
MATHDEC DS1 0 DS1 + 1
NEXT
")
         (interp (run-il-string source :max-scans 1)))
    ;; SC2 is always false, so loop never executes
    (is (= 0 (get-word-value interp "DS1")) "Loop should not run when condition is false")))

;;; ============================================================
;;; Compile plcprog.txt Test
;;; ============================================================

(test compile-plcprog
  "Test compiling the full plcprog.txt sample program"
  (let* ((source (read-test-file "plcprog.txt"))
         (parser (make-il-parser :source source))
         (result (parse-program parser)))
    (is (not (null result)) "Parser should return result")
    (is (null (program-errors result)) "Parser should have no errors")
    ;; Now compile the parsed program
    (let* ((compiler (make-il-compiler))
           (compiled (compile-program compiler result)))
      (is (not (null compiled)) "Compilation should succeed")
      (is (not (null (program-main-function compiled))) "Should have main function")
      (is (>= (hash-table-count (program-compiled-subroutines compiled)) 5)
          "Should compile at least 5 subroutines"))))

;;; End of test-compiler.lisp
