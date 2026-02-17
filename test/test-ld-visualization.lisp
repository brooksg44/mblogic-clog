;;;; test-ld-visualization.lisp
;;;; Phase 7+: Ladder Diagram Visualization Tests

(in-package #:mblogic-cl-test)

;;; ============================================================
;;; Test Suite Definition
;;; ============================================================

(def-suite ld-visualization-tests
  :description "Ladder diagram visualization tests"
  :in all-tests)

(in-suite ld-visualization-tests)

;;; ============================================================
;;; Helper Functions for Tests
;;; ============================================================

(defun parse-test-il (source)
  "Parse IL source string and return the parsed program"
  (mblogic-cl:parse-il-string source))

(defun make-test-instruction (opcode &rest params)
  "Create a parsed instruction for testing"
  (let ((instr-def (mblogic-cl:find-instruction opcode)))
    (make-instance 'mblogic-cl:parsed-instruction
                   :opcode opcode
                   :params params
                   :line-number 1
                   :instruction-def instr-def)))

;;; ============================================================
;;; Instruction Classification Tests
;;; ============================================================

(test contact-instruction-classification
  "Test that contact instructions are correctly identified"
  (is (mblogic-cl-web::contact-instruction-p "STR"))
  (is (mblogic-cl-web::contact-instruction-p "STRN"))
  (is (mblogic-cl-web::contact-instruction-p "AND"))
  (is (mblogic-cl-web::contact-instruction-p "ANDN"))
  (is (mblogic-cl-web::contact-instruction-p "OR"))
  (is (mblogic-cl-web::contact-instruction-p "ORN"))
  (is (mblogic-cl-web::contact-instruction-p "STRPD"))
  (is (mblogic-cl-web::contact-instruction-p "STRND"))
  (is (mblogic-cl-web::contact-instruction-p "ANDPD"))
  (is (mblogic-cl-web::contact-instruction-p "ANDND"))
  (is (mblogic-cl-web::contact-instruction-p "ORPD"))
  (is (mblogic-cl-web::contact-instruction-p "ORND"))
  ;; Non-contacts
  (is (not (mblogic-cl-web::contact-instruction-p "OUT")))
  (is (not (mblogic-cl-web::contact-instruction-p "TMR")))
  (is (not (mblogic-cl-web::contact-instruction-p "COPY"))))

(test coil-instruction-classification
  "Test that coil instructions are correctly identified"
  (is (mblogic-cl-web::coil-instruction-p "OUT"))
  (is (mblogic-cl-web::coil-instruction-p "SET"))
  (is (mblogic-cl-web::coil-instruction-p "RST"))
  (is (mblogic-cl-web::coil-instruction-p "PD"))
  ;; Non-coils
  (is (not (mblogic-cl-web::coil-instruction-p "STR")))
  (is (not (mblogic-cl-web::coil-instruction-p "AND")))
  (is (not (mblogic-cl-web::coil-instruction-p "TMR"))))

(test branch-instruction-classification
  "Test that branch instructions are correctly identified"
  ;; Branch start (parallel)
  (is (mblogic-cl-web::branch-start-p "OR"))
  (is (mblogic-cl-web::branch-start-p "ORN"))
  (is (mblogic-cl-web::branch-start-p "ORPD"))
  (is (mblogic-cl-web::branch-start-p "ORND"))
  ;; Branch end
  (is (mblogic-cl-web::branch-end-p "ANDSTR"))
  (is (mblogic-cl-web::branch-end-p "ORSTR"))
  ;; Non-branch
  (is (not (mblogic-cl-web::branch-start-p "STR")))
  (is (not (mblogic-cl-web::branch-end-p "AND"))))

(test block-instruction-classification
  "Test that block instructions are correctly identified"
  ;; Timers
  (is (mblogic-cl-web::block-instruction-p "TMR"))
  (is (mblogic-cl-web::block-instruction-p "TMRA"))
  (is (mblogic-cl-web::block-instruction-p "TMROFF"))
  ;; Counters
  (is (mblogic-cl-web::block-instruction-p "CNTU"))
  (is (mblogic-cl-web::block-instruction-p "CNTD"))
  (is (mblogic-cl-web::block-instruction-p "UDC"))
  ;; Data operations
  (is (mblogic-cl-web::block-instruction-p "COPY"))
  (is (mblogic-cl-web::block-instruction-p "CPYBLK"))
  (is (mblogic-cl-web::block-instruction-p "FILL"))
  (is (mblogic-cl-web::block-instruction-p "PACK"))
  (is (mblogic-cl-web::block-instruction-p "UNPACK"))
  ;; Math
  (is (mblogic-cl-web::block-instruction-p "MATHDEC"))
  (is (mblogic-cl-web::block-instruction-p "MATHHEX"))
  ;; Comparisons
  (is (mblogic-cl-web::block-instruction-p "STRE"))
  (is (mblogic-cl-web::block-instruction-p "STRGT"))
  ;; Non-blocks
  (is (not (mblogic-cl-web::block-instruction-p "STR")))
  (is (not (mblogic-cl-web::block-instruction-p "OUT"))))

(test control-instruction-classification
  "Test that control instructions are correctly identified"
  (is (mblogic-cl-web::control-instruction-p "END"))
  (is (mblogic-cl-web::control-instruction-p "ENDC"))
  (is (mblogic-cl-web::control-instruction-p "RT"))
  (is (mblogic-cl-web::control-instruction-p "RTC"))
  (is (mblogic-cl-web::control-instruction-p "NEXT"))
  ;; Non-control
  (is (not (mblogic-cl-web::control-instruction-p "STR")))
  (is (not (mblogic-cl-web::control-instruction-p "CALL"))))

;;; ============================================================
;;; Ladder Symbol Mapping Tests
;;; ============================================================

(test ladsymb-to-svg-mapping
  "Test ladder symbol to SVG mapping"
  ;; Contacts
  (is (string= "noc" (mblogic-cl-web::ladsymb-to-svg-symbol :contact-no "STR")))
  (is (string= "ncc" (mblogic-cl-web::ladsymb-to-svg-symbol :contact-nc "STRN")))
  (is (string= "nocpd" (mblogic-cl-web::ladsymb-to-svg-symbol :contact-pd "STRPD")))
  (is (string= "nocnd" (mblogic-cl-web::ladsymb-to-svg-symbol :contact-nd "STRND")))
  ;; Coils
  (is (string= "out" (mblogic-cl-web::ladsymb-to-svg-symbol :coil "OUT")))
  (is (string= "set" (mblogic-cl-web::ladsymb-to-svg-symbol :coil-set "SET")))
  (is (string= "rst" (mblogic-cl-web::ladsymb-to-svg-symbol :coil-reset "RST")))
  (is (string= "pd" (mblogic-cl-web::ladsymb-to-svg-symbol :coil-pd "PD")))
  ;; Timers (opcode-specific)
  (is (string= "tmr" (mblogic-cl-web::ladsymb-to-svg-symbol :timer "TMR")))
  (is (string= "tmra" (mblogic-cl-web::ladsymb-to-svg-symbol :timer "TMRA")))
  (is (string= "tmroff" (mblogic-cl-web::ladsymb-to-svg-symbol :timer "TMROFF")))
  ;; Counters (opcode-specific)
  (is (string= "cntu" (mblogic-cl-web::ladsymb-to-svg-symbol :counter "CNTU")))
  (is (string= "cntd" (mblogic-cl-web::ladsymb-to-svg-symbol :counter "CNTD")))
  (is (string= "udc" (mblogic-cl-web::ladsymb-to-svg-symbol :counter "UDC")))
  ;; Unknown falls back to "il"
  (is (string= "il" (mblogic-cl-web::ladsymb-to-svg-symbol nil "UNKNOWN"))))

;;; ============================================================
;;; Address Extraction Tests
;;; ============================================================

(test extract-addresses-contact
  "Test address extraction from contact instructions"
  (let ((instr (make-test-instruction "STR" "X1")))
    (let ((addrs (mblogic-cl-web::extract-addresses instr)))
      (is (= 1 (length addrs)))
      (is (string= "X1" (first addrs))))))

(test extract-addresses-coil
  "Test address extraction from coil instructions"
  (let ((instr (make-test-instruction "OUT" "Y1" "Y2" "Y3")))
    (let ((addrs (mblogic-cl-web::extract-addresses instr)))
      (is (= 3 (length addrs)))
      (is (member "Y1" addrs :test #'string=))
      (is (member "Y2" addrs :test #'string=))
      (is (member "Y3" addrs :test #'string=)))))

(test extract-addresses-timer
  "Test address extraction from timer instructions"
  (let ((instr (make-test-instruction "TMR" "T1" "100")))
    (let ((addrs (mblogic-cl-web::extract-addresses instr)))
      ;; Should get T1 (timer bit) and TD1 (timer data)
      (is (= 2 (length addrs)))
      (is (member "T1" addrs :test #'string=))
      (is (member "TD1" addrs :test #'string=)))))

(test extract-addresses-counter
  "Test address extraction from counter instructions"
  (let ((instr (make-test-instruction "CNTU" "CT5" "100")))
    (let ((addrs (mblogic-cl-web::extract-addresses instr)))
      ;; Should get CT5 (counter bit) and CTD5 (counter data)
      (is (= 2 (length addrs)))
      (is (member "CT5" addrs :test #'string=))
      (is (member "CTD5" addrs :test #'string=)))))

;;; ============================================================
;;; Ladder Cell Tests
;;; ============================================================

(test instruction-to-cell-contact
  "Test converting contact instruction to ladder cell"
  (let* ((instr (make-test-instruction "STR" "X1"))
         (cell (mblogic-cl-web::instruction-to-cell instr 0)))
    (is (eq :contact (mblogic-cl-web::ladder-cell-type cell)))
    (is (string= "X1" (mblogic-cl-web::ladder-cell-address cell)))
    (is (string= "STR" (mblogic-cl-web::ladder-cell-opcode cell)))
    (is (= 0 (mblogic-cl-web::ladder-cell-col cell)))))

(test instruction-to-cell-coil
  "Test converting coil instruction to ladder cell"
  (let* ((instr (make-test-instruction "OUT" "Y1"))
         (cell (mblogic-cl-web::instruction-to-cell instr 5)))
    (is (eq :coil (mblogic-cl-web::ladder-cell-type cell)))
    (is (string= "Y1" (mblogic-cl-web::ladder-cell-address cell)))
    (is (string= "OUT" (mblogic-cl-web::ladder-cell-opcode cell)))
    (is (= 5 (mblogic-cl-web::ladder-cell-col cell)))))

(test instruction-to-cell-block
  "Test converting block instruction to ladder cell"
  (let* ((instr (make-test-instruction "TMR" "T1" "100"))
         (cell (mblogic-cl-web::instruction-to-cell instr 2)))
    (is (eq :block (mblogic-cl-web::ladder-cell-type cell)))
    (is (string= "TMR" (mblogic-cl-web::ladder-cell-opcode cell)))
    (is (= 2 (mblogic-cl-web::ladder-cell-col cell)))))

;;; ============================================================
;;; Network to Rung Conversion Tests
;;; ============================================================

(test simple-rung-conversion
  "Test converting a simple network to a ladder rung"
  (let* ((source "NETWORK 1
STR X1
AND X2
OUT Y1
")
         (program (parse-test-il source))
         (network (first (mblogic-cl:program-main-networks program)))
         (rung (mblogic-cl-web::network-to-ladder-rung network)))
    ;; Check rung structure
    (is (= 1 (mblogic-cl-web::ladder-rung-number rung)))
    (is (= 1 (mblogic-cl-web::ladder-rung-rows rung)))  ; No branches = 1 row
    (is (= 2 (mblogic-cl-web::ladder-rung-cols rung)))  ; 2 input columns (outputs are separate)
    ;; Check cells
    (let ((cells (mblogic-cl-web::ladder-rung-cells rung)))
      (is (= 3 (length cells)))
      (is (string= "STR" (mblogic-cl-web::ladder-cell-opcode (first cells))))
      (is (string= "AND" (mblogic-cl-web::ladder-cell-opcode (second cells))))
      (is (string= "OUT" (mblogic-cl-web::ladder-cell-opcode (third cells)))))))

(test rung-with-addresses
  "Test that rung tracks all addresses"
  (let* ((source "NETWORK 1
STR X1
AND X2
OUT Y1 Y2
")
         (program (parse-test-il source))
         (network (first (mblogic-cl:program-main-networks program)))
         (rung (mblogic-cl-web::network-to-ladder-rung network)))
    (let ((addrs (mblogic-cl-web::ladder-rung-addresses rung)))
      ;; Should have X1, X2, Y1, Y2
      (is (>= (length addrs) 4))
      (is (member "X1" addrs :test #'string=))
      (is (member "X2" addrs :test #'string=))
      (is (member "Y1" addrs :test #'string=))
      (is (member "Y2" addrs :test #'string=)))))

;;; ============================================================
;;; Program to Ladder Conversion Tests
;;; ============================================================

(test program-to-ladder-main
  "Test converting main program to ladder structure"
  (let* ((source "NETWORK 1
STR X1
OUT Y1

NETWORK 2
STR X2
OUT Y2
")
         (program (parse-test-il source))
         (ladder (mblogic-cl-web::program-to-ladder program "main")))
    (is (string= "main" (mblogic-cl-web::ladder-program-name ladder)))
    (is (= 2 (length (mblogic-cl-web::ladder-program-rungs ladder))))
    ;; Check addresses collected from all rungs
    (let ((addrs (mblogic-cl-web::ladder-program-addresses ladder)))
      (is (member "X1" addrs :test #'string=))
      (is (member "Y1" addrs :test #'string=))
      (is (member "X2" addrs :test #'string=))
      (is (member "Y2" addrs :test #'string=)))))

(test program-to-ladder-subroutine
  "Test converting subroutine to ladder structure"
  (let* ((source "NETWORK 1
STR X1
OUT Y1

SBR TestSub
NETWORK 1
STR C1
OUT C2
")
         (program (parse-test-il source))
         (ladder (mblogic-cl-web::program-to-ladder program "TestSub")))
    (is (string= "TestSub" (mblogic-cl-web::ladder-program-name ladder)))
    (is (= 1 (length (mblogic-cl-web::ladder-program-rungs ladder))))))

(test list-subroutine-names
  "Test listing all subroutine names"
  (let* ((source "NETWORK 1
STR X1
OUT Y1

SBR Sub1
NETWORK 1
STR C1
OUT C2

SBR Sub2
NETWORK 1
STR C3
OUT C4
")
         (program (parse-test-il source))
         (names (mblogic-cl-web::list-subroutine-names program)))
    (is (member "main" names :test #'string=))
    (is (member "Sub1" names :test #'string=))
    (is (member "Sub2" names :test #'string=))))

;;; ============================================================
;;; JSON Serialization Tests
;;; ============================================================

(test cell-to-plist-conversion
  "Test converting ladder cell to plist"
  (let* ((instr (make-test-instruction "STR" "X1"))
         (cell (mblogic-cl-web::instruction-to-cell instr 0))
         (plist (mblogic-cl-web::cell-to-plist cell)))
    (is (string= "contact" (getf plist :type)))
    (is (string= "X1" (getf plist :addr)))
    (is (string= "STR" (getf plist :opcode)))
    (is (= 0 (getf plist :col)))))

(test rung-to-plist-conversion
  "Test converting ladder rung to plist"
  (let* ((source "NETWORK 5
STR X1
OUT Y1
")
         (program (parse-test-il source))
         (network (first (mblogic-cl:program-main-networks program)))
         (rung (mblogic-cl-web::network-to-ladder-rung network))
         (plist (mblogic-cl-web::rung-to-plist rung)))
    (is (= 5 (getf plist :rungnum)))
    (is (= 1 (getf plist :rows)))
    (is (= 1 (getf plist :cols)))  ; 1 input column (outputs are separate)
    (is (listp (getf plist :cells)))
    (is (= 2 (length (getf plist :cells))))))

(test ladder-program-to-plist-conversion
  "Test converting ladder program to plist"
  (let* ((source "NETWORK 1
STR X1
OUT Y1
")
         (program (parse-test-il source))
         (ladder (mblogic-cl-web::program-to-ladder program "main"))
         (plist (mblogic-cl-web::ladder-program-to-plist ladder)))
    (is (string= "main" (getf plist :subrname)))
    (is (listp (getf plist :addresses)))
    (is (listp (getf plist :subrdata)))
    (is (= 1 (length (getf plist :subrdata))))))

;;; ============================================================
;;; JSON API Tests
;;; ============================================================

(test plist-to-alist-conversion
  "Test plist to alist conversion"
  (let ((plist '(:name "test" :value 42 :active t)))
    (let ((alist (mblogic-cl-web::plist-to-alist plist)))
      (is (equal "test" (cdr (assoc :name alist))))
      (is (= 42 (cdr (assoc :value alist))))
      (is (eq t (cdr (assoc :active alist)))))))

(test plist-to-alist-nested
  "Test nested plist to alist conversion"
  (let ((plist '(:outer (:inner "value"))))
    (let ((alist (mblogic-cl-web::plist-to-alist plist)))
      (is (listp (cdr (assoc :outer alist)))))))

(test parse-address-list
  "Test parsing comma-separated address list"
  (is (equal '("X1" "Y1" "DS1")
             (mblogic-cl-web::parse-address-list "X1,Y1,DS1")))
  (is (equal '("X1" "Y1")
             (mblogic-cl-web::parse-address-list "X1, Y1")))
  (is (null (mblogic-cl-web::parse-address-list "")))
  (is (null (mblogic-cl-web::parse-address-list nil))))

;;; ============================================================
;;; Integration Tests
;;; ============================================================

(test full-ladder-conversion-integration
  "Test full IL program to ladder conversion pipeline"
  (let* ((source "// Simple motor control
NETWORK 1
STR X1
AND X2
OUT Y1

NETWORK 2
STR X3
TMR T1 100
OUT Y2
")
         (program (parse-test-il source))
         (ladder (mblogic-cl-web::program-to-ladder program "main")))
    ;; Verify structure
    (is (not (null ladder)))
    (is (= 2 (length (mblogic-cl-web::ladder-program-rungs ladder))))
    ;; Verify first rung
    (let ((rung1 (first (mblogic-cl-web::ladder-program-rungs ladder))))
      (is (= 1 (mblogic-cl-web::ladder-rung-number rung1)))
      (is (= 3 (length (mblogic-cl-web::ladder-rung-cells rung1)))))
    ;; Verify second rung has timer as output (type :coil)
    (let* ((rung2 (second (mblogic-cl-web::ladder-program-rungs ladder)))
           (cells (mblogic-cl-web::ladder-rung-cells rung2))
           (timer-cell (find "TMR" cells
                            :key #'mblogic-cl-web::ladder-cell-opcode
                            :test #'string=)))
      (is (not (null timer-cell)))
      (is (eq :coil (mblogic-cl-web::ladder-cell-type timer-cell))))))

(test ladder-to-json-integration
  "Test complete ladder to JSON conversion"
  (let* ((source "NETWORK 1
STR X1
OUT Y1
")
         (program (parse-test-il source))
         (ladder (mblogic-cl-web::program-to-ladder program "main"))
         (plist (mblogic-cl-web::ladder-program-to-plist ladder))
         (json (mblogic-cl-web::plist-to-json plist)))
    ;; JSON should be a non-empty string
    (is (stringp json))
    (is (> (length json) 0))
    ;; Should contain expected keys
    (is (search "subrname" json))
    (is (search "subrdata" json))
    (is (search "main" json))))

;;; ============================================================
;;; Edge Case Tests
;;; ============================================================

(test empty-network
  "Test handling of minimal network"
  (let* ((source "NETWORK 1
")
         (program (parse-test-il source))
         (networks (mblogic-cl:program-main-networks program)))
    ;; Should handle gracefully even if empty
    (is (listp networks))))

(test multiple-outputs-per-rung
  "Test rung with multiple coil outputs"
  (let* ((source "NETWORK 1
STR X1
OUT Y1 Y2 Y3 Y4
")
         (program (parse-test-il source))
         (network (first (mblogic-cl:program-main-networks program)))
         (rung (mblogic-cl-web::network-to-ladder-rung network)))
    (let ((addrs (mblogic-cl-web::ladder-rung-addresses rung)))
      (is (member "Y1" addrs :test #'string=))
      (is (member "Y2" addrs :test #'string=))
      (is (member "Y3" addrs :test #'string=))
      (is (member "Y4" addrs :test #'string=)))))

(test comparison-instruction-addresses
  "Test address extraction from comparison instructions"
  (let ((instr (make-test-instruction "STRE" "DS1" "DS2")))
    (let ((addrs (mblogic-cl-web::extract-addresses instr)))
      (is (member "DS1" addrs :test #'string=))
      (is (member "DS2" addrs :test #'string=)))))

;;; ============================================================
;;; Ladder Structure Validation Tests
;;; ============================================================

(test ladder-cell-structure
  "Test ladder-cell struct has all required fields"
  (let ((cell (mblogic-cl-web::make-ladder-cell
               :type :contact
               :symbol "noc"
               :address "X1"
               :opcode "STR"
               :row 0
               :col 0)))
    (is (eq :contact (mblogic-cl-web::ladder-cell-type cell)))
    (is (string= "noc" (mblogic-cl-web::ladder-cell-symbol cell)))
    (is (string= "X1" (mblogic-cl-web::ladder-cell-address cell)))
    (is (string= "STR" (mblogic-cl-web::ladder-cell-opcode cell)))
    (is (= 0 (mblogic-cl-web::ladder-cell-row cell)))
    (is (= 0 (mblogic-cl-web::ladder-cell-col cell)))))

(test ladder-rung-structure
  "Test ladder-rung struct has all required fields"
  (let ((rung (mblogic-cl-web::make-ladder-rung
               :number 1
               :cells nil
               :rows 1
               :cols 0)))
    (is (= 1 (mblogic-cl-web::ladder-rung-number rung)))
    (is (null (mblogic-cl-web::ladder-rung-cells rung)))
    (is (= 1 (mblogic-cl-web::ladder-rung-rows rung)))
    (is (= 0 (mblogic-cl-web::ladder-rung-cols rung)))))

(test ladder-program-structure
  "Test ladder-program struct has all required fields"
  (let ((prog (mblogic-cl-web::make-ladder-program
               :name "test"
               :rungs nil
               :addresses nil)))
    (is (string= "test" (mblogic-cl-web::ladder-program-name prog)))
    (is (null (mblogic-cl-web::ladder-program-rungs prog)))
    (is (null (mblogic-cl-web::ladder-program-addresses prog)))))

;;; ============================================================
;;; Branch Rung Tests
;;; ============================================================

(test branch-rung-with-or
  "Test rung with OR creates parallel branch with correct connectors"
  (let* ((source "NETWORK 1
STR X1
OR X2
AND X3
OUT Y1
")
         (program (parse-test-il source))
         (network (first (mblogic-cl:program-main-networks program)))
         (rung (mblogic-cl-web::network-to-ladder-rung network)))
    ;; Should have 2 rows due to OR creating parallel branch
    (is (= 2 (mblogic-cl-web::ladder-rung-rows rung)))
    ;; Check cells exist at expected positions
    (let ((cells (mblogic-cl-web::ladder-rung-cells rung)))
      ;; Should have: X1 at (0,0), X2 at (1,0), connectors, X3, output
      (is (>= (length cells) 4))
      ;; Find branch connectors
      (let ((branch-cells (remove-if-not
                           (lambda (c)
                             (member (mblogic-cl-web::ladder-cell-symbol c)
                                     '("branchttr" "branchtr" "branchr" "brancht"
                                       "branchttl" "branchtl" "branchl" "vbarl" "vbarr" "hbar")
                                     :test #'string-equal))
                           cells)))
        ;; Should have branch connectors (at least 2 from close-branch-block)
        (is (>= (length branch-cells) 2))))))

(test branch-rung-js-format
  "Test that branch rung produces correct JS format"
  (let* ((source "NETWORK 1
STR X1
OR X2
OUT Y1
")
         (program (parse-test-il source))
         (ladder (mblogic-cl-web::program-to-ladder program "main"))
         (js-data (mblogic-cl-web::ladder-program-to-js-format ladder)))
    ;; Should have rungdata
    (let ((rungdata (cdr (assoc :rungdata js-data))))
      (is (not (null rungdata)))
      (when rungdata
        (let* ((first-rung (first rungdata))
               (matrixdata (cdr (assoc :matrixdata first-rung))))
          ;; Should have inputedit00 (X1) and inputedit10 (X2)
          (is (assoc "inputedit00" matrixdata :test #'string=))
          (is (assoc "inputedit10" matrixdata :test #'string=))
          ;; Should have branch connectors in column 1
          (let ((brancht-cell (assoc "inputedit01" matrixdata :test #'string=)))
            (when brancht-cell
              (let ((value (cdr (assoc :value (cdr brancht-cell)))))
                ;; Top connector should be one of the branch symbols
                (is (member value '("branchttl" "branchttr" "brancht") :test #'string=))))))))))

;;; ============================================================
;;; Output Block Instruction Tests
;;; ============================================================

(test output-block-instruction-classification
  "Test that output block instructions are correctly identified"
  ;; Timers
  (is (mblogic-cl-web::output-block-instruction-p "TMR"))
  (is (mblogic-cl-web::output-block-instruction-p "TMRA"))
  (is (mblogic-cl-web::output-block-instruction-p "TMROFF"))
  ;; Counters
  (is (mblogic-cl-web::output-block-instruction-p "CNTU"))
  (is (mblogic-cl-web::output-block-instruction-p "CNTD"))
  (is (mblogic-cl-web::output-block-instruction-p "UDC"))
  ;; Data operations
  (is (mblogic-cl-web::output-block-instruction-p "COPY"))
  (is (mblogic-cl-web::output-block-instruction-p "CPYBLK"))
  (is (mblogic-cl-web::output-block-instruction-p "FILL"))
  (is (mblogic-cl-web::output-block-instruction-p "PACK"))
  (is (mblogic-cl-web::output-block-instruction-p "UNPACK"))
  (is (mblogic-cl-web::output-block-instruction-p "SHFRG"))
  ;; Math
  (is (mblogic-cl-web::output-block-instruction-p "MATHDEC"))
  (is (mblogic-cl-web::output-block-instruction-p "MATHHEX"))
  (is (mblogic-cl-web::output-block-instruction-p "SUM"))
  ;; Find
  (is (mblogic-cl-web::output-block-instruction-p "FINDEQ"))
  (is (mblogic-cl-web::output-block-instruction-p "FINDNE"))
  ;; Control blocks
  (is (mblogic-cl-web::output-block-instruction-p "CALL"))
  (is (mblogic-cl-web::output-block-instruction-p "FOR"))
  ;; NOT output blocks: comparisons are inputs
  (is (not (mblogic-cl-web::output-block-instruction-p "STRE")))
  (is (not (mblogic-cl-web::output-block-instruction-p "ANDE")))
  (is (not (mblogic-cl-web::output-block-instruction-p "ORE")))
  ;; NOT output blocks: contacts and coils
  (is (not (mblogic-cl-web::output-block-instruction-p "STR")))
  (is (not (mblogic-cl-web::output-block-instruction-p "OUT")))
  (is (not (mblogic-cl-web::output-block-instruction-p "END"))))

(test rungtype-for-output-mapping
  "Test rungtype determination from output block opcode"
  ;; Double rungs
  (is (string= "double" (mblogic-cl-web::rungtype-for-output "CNTU")))
  (is (string= "double" (mblogic-cl-web::rungtype-for-output "CNTD")))
  (is (string= "double" (mblogic-cl-web::rungtype-for-output "TMRA")))
  ;; Triple rungs
  (is (string= "triple" (mblogic-cl-web::rungtype-for-output "UDC")))
  (is (string= "triple" (mblogic-cl-web::rungtype-for-output "SHFRG")))
  ;; Single rungs
  (is (string= "single" (mblogic-cl-web::rungtype-for-output "TMR")))
  (is (string= "single" (mblogic-cl-web::rungtype-for-output "TMROFF")))
  (is (string= "single" (mblogic-cl-web::rungtype-for-output "COPY")))
  (is (string= "single" (mblogic-cl-web::rungtype-for-output "MATHDEC")))
  (is (string= "single" (mblogic-cl-web::rungtype-for-output "CALL")))
  (is (string= "single" (mblogic-cl-web::rungtype-for-output "FOR"))))

;;; ============================================================
;;; Timer/Counter/Block Output Rendering Tests
;;; ============================================================

(test timer-rung-output-column
  "Test that TMR appears in the output column, not input"
  (let* ((source "NETWORK 1
STR X3
TMR T1 100
OUT Y2
")
         (program (parse-test-il source))
         (network (first (mblogic-cl:program-main-networks program)))
         (rung (mblogic-cl-web::network-to-ladder-rung network))
         (cells (mblogic-cl-web::ladder-rung-cells rung)))
    ;; TMR should be in output column (type :coil), not input
    (let ((tmr-cell (find "TMR" cells
                          :key #'mblogic-cl-web::ladder-cell-opcode
                          :test #'string=)))
      (is (not (null tmr-cell)))
      (is (eq :coil (mblogic-cl-web::ladder-cell-type tmr-cell)))
      ;; Should have all params as addresses
      (is (member "T1" (mblogic-cl-web::ladder-cell-addresses tmr-cell)
                  :test #'string=))
      (is (member "100" (mblogic-cl-web::ladder-cell-addresses tmr-cell)
                  :test #'string=)))))

(test counter-double-rung
  "Test CNTU creates a double rung with 2 input rows"
  (let* ((source "NETWORK 1
STR X1
STR X2
CNTU CT1 100
")
         (program (parse-test-il source))
         (network (first (mblogic-cl:program-main-networks program)))
         (rung (mblogic-cl-web::network-to-ladder-rung network)))
    ;; Should have 2 input rows
    (is (>= (mblogic-cl-web::ladder-rung-rows rung) 2))
    ;; CNTU should be in output column
    (let* ((cells (mblogic-cl-web::ladder-rung-cells rung))
           (cntu-cell (find "CNTU" cells
                            :key #'mblogic-cl-web::ladder-cell-opcode
                            :test #'string=)))
      (is (not (null cntu-cell)))
      (is (eq :coil (mblogic-cl-web::ladder-cell-type cntu-cell)))
      ;; Rungtype should be double
      (is (string= "double" (mblogic-cl-web::determine-rungtype rung))))))

(test udc-triple-rung
  "Test UDC creates a triple rung with 3 input rows"
  (let* ((source "NETWORK 1
STR X1
STR X2
STR X3
UDC CT1 100
")
         (program (parse-test-il source))
         (network (first (mblogic-cl:program-main-networks program)))
         (rung (mblogic-cl-web::network-to-ladder-rung network)))
    ;; Should have 3 input rows
    (is (>= (mblogic-cl-web::ladder-rung-rows rung) 3))
    ;; UDC should be in output column
    (let* ((cells (mblogic-cl-web::ladder-rung-cells rung))
           (udc-cell (find "UDC" cells
                           :key #'mblogic-cl-web::ladder-cell-opcode
                           :test #'string=)))
      (is (not (null udc-cell)))
      (is (eq :coil (mblogic-cl-web::ladder-cell-type udc-cell)))
      ;; Rungtype should be triple
      (is (string= "triple" (mblogic-cl-web::determine-rungtype rung))))))

(test or-branch-stays-single
  "Test OR-branched rung stays single type (not double)"
  (let* ((source "NETWORK 1
STR X1
OR X2
OUT Y1
")
         (program (parse-test-il source))
         (network (first (mblogic-cl:program-main-networks program)))
         (rung (mblogic-cl-web::network-to-ladder-rung network)))
    ;; Rungtype should be single even though there are 2 input rows
    (is (string= "single" (mblogic-cl-web::determine-rungtype rung)))))

(test mathdec-output-column
  "Test MATHDEC appears in the output column"
  (let* ((source "NETWORK 1
STR X1
MATHDEC DS1 0 1+1
OUT Y1
")
         (program (parse-test-il source))
         (network (first (mblogic-cl:program-main-networks program)))
         (rung (mblogic-cl-web::network-to-ladder-rung network))
         (cells (mblogic-cl-web::ladder-rung-cells rung)))
    ;; MATHDEC should be in output column
    (let ((math-cell (find "MATHDEC" cells
                           :key #'mblogic-cl-web::ladder-cell-opcode
                           :test #'string=)))
      (is (not (null math-cell)))
      (is (eq :coil (mblogic-cl-web::ladder-cell-type math-cell)))
      ;; Rungtype should be single
      (is (string= "single" (mblogic-cl-web::determine-rungtype rung))))))

(test copy-output-column
  "Test COPY appears in the output column"
  (let* ((source "NETWORK 1
STR X1
COPY DS1 DS2
OUT Y1
")
         (program (parse-test-il source))
         (network (first (mblogic-cl:program-main-networks program)))
         (rung (mblogic-cl-web::network-to-ladder-rung network))
         (cells (mblogic-cl-web::ladder-rung-cells rung)))
    ;; COPY should be in output column
    (let ((copy-cell (find "COPY" cells
                           :key #'mblogic-cl-web::ladder-cell-opcode
                           :test #'string=)))
      (is (not (null copy-cell)))
      (is (eq :coil (mblogic-cl-web::ladder-cell-type copy-cell))))))

(test timer-rung-js-format
  "Test TMR rung produces correct JS format with outputedit entry"
  (let* ((source "NETWORK 1
STR X1
TMR T1 100
OUT Y1
")
         (program (parse-test-il source))
         (ladder (mblogic-cl-web::program-to-ladder program "main"))
         (js-data (mblogic-cl-web::ladder-program-to-js-format ladder)))
    (let* ((rungdata (cdr (assoc :rungdata js-data)))
           (first-rung (first rungdata))
           (matrixdata (cdr (assoc :matrixdata first-rung)))
           (rungtype (cdr (assoc :rungtype first-rung))))
      ;; Should be single rungtype
      (is (string= "single" rungtype))
      ;; Should have TMR as outputedit0
      (let ((output0 (cdr (assoc "outputedit0" matrixdata :test #'string=))))
        (is (not (null output0)))
        (when output0
          (is (string= "tmr" (cdr (assoc :value output0)))))))))

(test counter-double-rung-js-format
  "Test CNTU rung produces correct JS format as double rung"
  (let* ((source "NETWORK 1
STR X1
STR X2
CNTU CT1 100
")
         (program (parse-test-il source))
         (ladder (mblogic-cl-web::program-to-ladder program "main"))
         (js-data (mblogic-cl-web::ladder-program-to-js-format ladder)))
    (let* ((rungdata (cdr (assoc :rungdata js-data)))
           (first-rung (first rungdata))
           (matrixdata (cdr (assoc :matrixdata first-rung)))
           (rungtype (cdr (assoc :rungtype first-rung))))
      ;; Should be double rungtype
      (is (string= "double" rungtype))
      ;; Should have two input rows
      (is (assoc "inputedit00" matrixdata :test #'string=))
      (is (assoc "inputedit10" matrixdata :test #'string=))
      ;; Should have CNTU as outputedit0
            (let ((output0 (cdr (assoc "outputedit0" matrixdata :test #'string=))))
         (is (not (null output0)))
         (when output0
           (is (string= "cntu" (cdr (assoc :value output0)))))))))

(test complex-nested-branch-with-continuation
  "Test complex rung with nested branches and continuation instructions
   Continuation instructions (STRE, ORPD, ANDGT) should appear on the top wire path"
  (let* ((source "NETWORK 1
STR T5
ORN C1
AND C2
STR C3
AND C4
STR C5
OR C6
ANDSTR
ORSTR
STRE DS100 50
ORPD C100
ANDGT DS112 86
ANDSTR
SET C101
")
         (program (parse-test-il source))
         (network (first (mblogic-cl:program-main-networks program)))
         (rung (mblogic-cl-web::network-to-ladder-rung network))
         (cells (mblogic-cl-web::ladder-rung-cells rung)))
    ;; Should have at least 3 rows (nested branches create multi-row structure)
    (is (>= (mblogic-cl-web::ladder-rung-rows rung) 3))
    ;; Find continuation instruction cells (STRE, ORPD, ANDGT)
    (let ((stre-cell (find "STRE" cells
                          :key #'mblogic-cl-web::ladder-cell-opcode
                          :test #'string=))
          (orpd-cell (find "ORPD" cells
                          :key #'mblogic-cl-web::ladder-cell-opcode
                          :test #'string=))
          (andgt-cell (find "ANDGT" cells
                           :key #'mblogic-cl-web::ladder-cell-opcode
                           :test #'string=)))
      ;; All continuation instructions should exist
      (is (not (null stre-cell)))
      (is (not (null orpd-cell)))
      (is (not (null andgt-cell)))
      ;; Verify continuation instructions are on row 0 (top wire)
      (is (= (mblogic-cl-web::ladder-cell-row stre-cell) 0))
      (is (= (mblogic-cl-web::ladder-cell-row andgt-cell) 0))
      ;; Verify ORPD is on row 1 (bottom wire)
      (is (= (mblogic-cl-web::ladder-cell-row orpd-cell) 1)))))

;;; End of test-ld-visualization.lisp
