;;;; test-parser.lisp
;;;; Phase 7: IL parser tests

(in-package #:mblogic-cl-test)

(in-suite parser-tests)

;;; ============================================================
;;; Instruction Set Tests
;;; ============================================================

(test instruction-set-loaded
  "Test that instruction set is loaded"
  (is (> (instruction-count) 0))
  (is (>= (instruction-count) 70)))  ; Should have ~80+ instructions

(test find-instruction
  "Test instruction lookup"
  (is (not (null (find-instruction "STR"))))
  (is (not (null (find-instruction "AND"))))
  (is (not (null (find-instruction "OUT"))))
  (is (not (null (find-instruction "TMR"))))
  (is (null (find-instruction "INVALID"))))

(test instruction-properties
  "Test instruction properties"
  (let ((str-instr (find-instruction "STR")))
    (is (string= "STR" (instruction-opcode str-instr)))
    (is (= 1 (instruction-min-params str-instr)))
    (is (= 1 (instruction-max-params str-instr))))
  (let ((out-instr (find-instruction "OUT")))
    (is (= 1 (instruction-min-params out-instr)))
    (is (= 8 (instruction-max-params out-instr)))))

;;; ============================================================
;;; Basic Parsing Tests
;;; ============================================================

(test parse-simple-network
  "Test parsing simple network"
  (let* ((source (list "NETWORK 1"
                       "STR X1"
                       "OUT Y1"))
         (parser (make-il-parser :source source))
         (result (parse-program parser)))
    (is (not (null result)))
    (is (null (program-errors result)))
    (is (= 1 (length (program-main-networks result))))))

(test parse-multiple-networks
  "Test parsing multiple networks"
  (let* ((source (list "NETWORK 1"
                       "STR X1"
                       "OUT Y1"
                       ""
                       "NETWORK 2"
                       "STR X2"
                       "OUT Y2"))
         (parser (make-il-parser :source source))
         (result (parse-program parser)))
    (is (not (null result)))
    (is (null (program-errors result)))
    (is (= 2 (length (program-main-networks result))))))

(test parse-comments
  "Test parsing with comments"
  (let* ((source (list "// This is a comment"
                       "NETWORK 1"
                       "// Another comment"
                       "STR X1"
                       "OUT Y1  // Inline comment"))
         (parser (make-il-parser :source source))
         (result (parse-program parser)))
    (is (not (null result)))
    (is (null (program-errors result)))))

(test parse-blank-lines
  "Test parsing with blank lines"
  (let* ((source (list ""
                       "NETWORK 1"
                       ""
                       "STR X1"
                       ""
                       "OUT Y1"
                       ""))
         (parser (make-il-parser :source source))
         (result (parse-program parser)))
    (is (not (null result)))
    (is (null (program-errors result)))))

;;; ============================================================
;;; Instruction Parsing Tests
;;; ============================================================

(test parse-boolean-instructions
  "Test parsing boolean instructions"
  (let* ((source (list "NETWORK 1"
                       "STR X1"
                       "STRN X2"
                       "AND C1"
                       "ANDN C2"
                       "OR T1"
                       "ORN CT1"
                       "OUT Y1"))
         (parser (make-il-parser :source source))
         (result (parse-program parser)))
    (is (not (null result)))
    (is (null (program-errors result)))))

(test parse-stack-instructions
  "Test parsing stack instructions"
  (let* ((source (list "NETWORK 1"
                       "STR X1"
                       "STR X2"
                       "ANDSTR"
                       "STR X3"
                       "ORSTR"
                       "OUT Y1"))
         (parser (make-il-parser :source source))
         (result (parse-program parser)))
    (is (not (null result)))
    (is (null (program-errors result)))))

(test parse-compare-instructions
  "Test parsing compare instructions"
  (let* ((source (list "NETWORK 1"
                       "STRE DS1 100"
                       "STRNE DS2 0"
                       "STRGT DS3 50"
                       "STRLT DS4 25"
                       "OUT Y1"))
         (parser (make-il-parser :source source))
         (result (parse-program parser)))
    (is (not (null result)))
    (is (null (program-errors result)))))

(test parse-timer-instructions
  "Test parsing timer instructions"
  (let* ((source (list "NETWORK 1"
                       "STR X1"
                       "TMR T1 1000 ms"
                       ""
                       "NETWORK 2"
                       "STR X2"
                       "STR X3"
                       "TMRA T2 5000 sec"))
         (parser (make-il-parser :source source))
         (result (parse-program parser)))
    (is (not (null result)))
    (is (null (program-errors result)))))

(test parse-counter-instructions
  "Test parsing counter instructions"
  (let* ((source (list "NETWORK 1"
                       "STR X1"
                       "STR X2"
                       "CNTU CT1 10"
                       ""
                       "NETWORK 2"
                       "STR X3"
                       "STR X4"
                       "CNTD CT2 5"))
         (parser (make-il-parser :source source))
         (result (parse-program parser)))
    (is (not (null result)))
    (is (null (program-errors result)))))

(test parse-data-instructions
  "Test parsing data movement instructions"
  (let* ((source (list "NETWORK 1"
                       "STR SC1"
                       "COPY 100 DS1"
                       "COPY DS1 DS2"
                       "CPYBLK DS1 DS5 DS100"
                       "FILL 0 DS200 DS210"))
         (parser (make-il-parser :source source))
         (result (parse-program parser)))
    (is (not (null result)))
    (is (null (program-errors result)))))

(test parse-math-instructions
  "Test parsing math instructions"
  (let* ((source (list "NETWORK 1"
                       "STR SC1"
                       "MATHDEC DS100 0 DS1 + DS2 * 2"
                       "MATHHEX DH100 0 DH1 AND DH2"))
         (parser (make-il-parser :source source))
         (result (parse-program parser)))
    (is (not (null result)))
    (is (null (program-errors result)))))

;;; ============================================================
;;; Subroutine Parsing Tests
;;; ============================================================

(test parse-subroutine
  "Test parsing subroutine definition"
  (let* ((source (list "NETWORK 1"
                       "STR SC1"
                       "CALL TestSub"
                       ""
                       "NETWORK 2"
                       "END"
                       ""
                       "SBR TestSub"
                       "NETWORK 1"
                       "STR SC1"
                       "OUT Y1"
                       "RT"))
         (parser (make-il-parser :source source))
         (result (parse-program parser)))
    (is (not (null result)))
    (is (null (program-errors result)))
    (is (>= (hash-table-count (program-subroutines result)) 1))))

(test parse-control-flow
  "Test parsing control flow instructions"
  (let* ((source (list "NETWORK 1"
                       "STR X1"
                       "ENDC"
                       ""
                       "NETWORK 2"
                       "STR SC1"
                       "FOR 10"
                       "OUT C1"
                       "NEXT"
                       ""
                       "NETWORK 3"
                       "END"))
         (parser (make-il-parser :source source))
         (result (parse-program parser)))
    (is (not (null result)))
    (is (null (program-errors result)))))

;;; ============================================================
;;; Edge Cases and Error Handling
;;; ============================================================

(test parse-multiple-outputs
  "Test parsing instruction with multiple outputs"
  (let* ((source (list "NETWORK 1"
                       "STR X1"
                       "OUT Y1 Y2 Y3 Y4"))
         (parser (make-il-parser :source source))
         (result (parse-program parser)))
    (is (not (null result)))
    (is (null (program-errors result)))))

(test parse-edge-detection
  "Test parsing edge detection instructions"
  (let* ((source (list "NETWORK 1"
                       "STRPD X1"
                       "ANDPD C1"
                       "ORPD T1"
                       "STRND X2"
                       "ANDND C2"
                       "ORND T2"
                       "OUT Y1"))
         (parser (make-il-parser :source source))
         (result (parse-program parser)))
    (is (not (null result)))
    (is (null (program-errors result)))))

(test parse-numeric-constants
  "Test parsing numeric constants"
  (let* ((source (list "NETWORK 1"
                       "STR SC1"
                       "COPY 12345 DS1"
                       "COPY -999 DS2"
                       "STRE DS1 100"
                       "STRGT DS2 -50"))
         (parser (make-il-parser :source source))
         (result (parse-program parser)))
    (is (not (null result)))
    (is (null (program-errors result)))))

;;; ============================================================
;;; Comprehensive plcprog.txt Parse Test
;;; ============================================================

(test parse-plcprog
  "Test parsing the full plcprog.txt sample program"
  (let* ((source (read-test-file "plcprog.txt"))
         (parser (make-il-parser :source source))
         (result (parse-program parser)))
    (is (not (null result)) "Parser should return result")
    (is (null (program-errors result))
        "Parser should have no errors")
    ;; Check we parsed multiple networks
    (is (>= (length (program-main-networks result)) 10)
        "Should parse at least 10 main networks")
    ;; Check we parsed subroutines
    (is (>= (hash-table-count (program-subroutines result)) 5)
        "Should parse at least 5 subroutines")))

;;; End of test-parser.lisp
