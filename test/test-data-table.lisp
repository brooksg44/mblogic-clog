;;;; test-data-table.lisp
;;;; Phase 7: Data table tests

(in-package #:mblogic-cl-test)

(in-suite data-table-tests)

;;; ============================================================
;;; Data Table Creation and Initialization
;;; ============================================================

(test create-data-table
  "Test data table creation"
  (let ((dt (make-data-table)))
    (is (not (null dt)))
    (is (typep dt 'data-table))))

(defun hash-key-exists-p (key hash-table)
  "Check if KEY exists in HASH-TABLE (distinct from value being NIL)"
  (multiple-value-bind (value present-p) (gethash key hash-table)
    (declare (ignore value))
    present-p))

(test init-data-table
  "Test data table initialization"
  (with-test-data-table (dt)
    ;; Check boolean addresses exist (value is NIL, but key exists)
    (is (hash-key-exists-p "X1" (bool-table dt)))
    (is (hash-key-exists-p "Y2000" (bool-table dt)))
    (is (hash-key-exists-p "C1" (bool-table dt)))
    (is (hash-key-exists-p "SC1" (bool-table dt)))
    (is (hash-key-exists-p "T1" (bool-table dt)))
    (is (hash-key-exists-p "CT1" (bool-table dt)))
    ;; Check word addresses exist
    (is (eql 0 (gethash "DS1" (word-table dt))))
    (is (eql 0 (gethash "DS10000" (word-table dt))))
    (is (eql 0 (gethash "DD1" (word-table dt))))
    (is (eql 0 (gethash "DH1" (word-table dt))))
    (is (eql 0 (gethash "TD1" (word-table dt))))
    (is (eql 0 (gethash "CTD1" (word-table dt))))
    ;; Check float addresses exist
    (is (eql 0.0 (gethash "DF1" (float-table dt))))
    (is (eql 0.0 (gethash "DF2000" (float-table dt))))
    ;; Check string addresses exist
    (is (string= "" (gethash "TXT1" (string-table dt))))
    (is (string= "" (gethash "TXT10000" (string-table dt))))))

;;; ============================================================
;;; Boolean Operations
;;; ============================================================

(test bool-read-write
  "Test boolean read/write operations"
  (with-test-data-table (dt)
    ;; Initial value is nil
    (is (null (get-bool dt "X1")))
    ;; Set to true
    (set-bool dt "X1" t)
    (is (eq t (get-bool dt "X1")))
    ;; Set to false
    (set-bool dt "X1" nil)
    (is (null (get-bool dt "X1")))))

(test bool-address-types
  "Test all boolean address types"
  (with-test-data-table (dt)
    ;; Inputs (X)
    (set-bool dt "X100" t)
    (is (eq t (get-bool dt "X100")))
    ;; Outputs (Y)
    (set-bool dt "Y100" t)
    (is (eq t (get-bool dt "Y100")))
    ;; Control relays (C)
    (set-bool dt "C100" t)
    (is (eq t (get-bool dt "C100")))
    ;; System control (SC)
    (set-bool dt "SC100" t)
    (is (eq t (get-bool dt "SC100")))
    ;; Timers (T)
    (set-bool dt "T50" t)
    (is (eq t (get-bool dt "T50")))
    ;; Counters (CT)
    (set-bool dt "CT25" t)
    (is (eq t (get-bool dt "CT25")))))

;;; ============================================================
;;; Word Operations
;;; ============================================================

(test word-read-write
  "Test word read/write operations"
  (with-test-data-table (dt)
    ;; Initial value is 0
    (is (= 0 (get-word dt "DS1")))
    ;; Set positive value
    (set-word dt "DS1" 12345)
    (is (= 12345 (get-word dt "DS1")))
    ;; Set negative value
    (set-word dt "DS1" -9999)
    (is (= -9999 (get-word dt "DS1")))
    ;; Set zero
    (set-word dt "DS1" 0)
    (is (= 0 (get-word dt "DS1")))))

(test word-address-types
  "Test all word address types"
  (with-test-data-table (dt)
    ;; DS (signed data)
    (set-word dt "DS100" -1000)
    (is (= -1000 (get-word dt "DS100")))
    ;; DD (double data)
    (set-word dt "DD100" 100000)
    (is (= 100000 (get-word dt "DD100")))
    ;; DH (hex/unsigned)
    (set-word dt "DH100" #xFFFF)
    (is (= #xFFFF (get-word dt "DH100")))
    ;; XD (input register)
    (set-word dt "XD10" 500)
    (is (= 500 (get-word dt "XD10")))
    ;; YD (output register)
    (set-word dt "YD10" 600)
    (is (= 600 (get-word dt "YD10")))
    ;; TD (timer data)
    (set-word dt "TD50" 1000)
    (is (= 1000 (get-word dt "TD50")))
    ;; CTD (counter data)
    (set-word dt "CTD25" 50)
    (is (= 50 (get-word dt "CTD25")))))

;;; ============================================================
;;; Float Operations
;;; ============================================================

(test float-read-write
  "Test float read/write operations"
  (with-test-data-table (dt)
    ;; Initial value is 0.0
    (is (= 0.0 (get-float dt "DF1")))
    ;; Set positive value
    (set-float dt "DF1" 3.14159)
    (is (< (abs (- 3.14159 (get-float dt "DF1"))) 0.00001))
    ;; Set negative value
    (set-float dt "DF1" -273.15)
    (is (< (abs (- -273.15 (get-float dt "DF1"))) 0.00001))
    ;; Set very small value
    (set-float dt "DF1" 0.000001)
    (is (< (abs (- 0.000001 (get-float dt "DF1"))) 0.0000001))))

;;; ============================================================
;;; String Operations
;;; ============================================================

(test string-read-write
  "Test string read/write operations"
  (with-test-data-table (dt)
    ;; Initial value is empty string
    (is (string= "" (get-string dt "TXT1")))
    ;; Set string value
    (set-string dt "TXT1" "Hello")
    (is (string= "Hello" (get-string dt "TXT1")))
    ;; Set empty string
    (set-string dt "TXT1" "")
    (is (string= "" (get-string dt "TXT1")))
    ;; Set unicode string
    (set-string dt "TXT1" "Test123")
    (is (string= "Test123" (get-string dt "TXT1")))))

;;; ============================================================
;;; Address Validation (from instructions.lisp)
;;; ============================================================

(test bool-addr-p
  "Test boolean address validation"
  (is (bool-addr-p "X1"))
  (is (bool-addr-p "X2000"))
  (is (bool-addr-p "Y1"))
  (is (bool-addr-p "C100"))
  (is (bool-addr-p "SC500"))
  (is (bool-addr-p "T50"))
  (is (bool-addr-p "CT25"))
  (is (not (bool-addr-p "DS1")))
  (is (not (bool-addr-p "DF1")))
  (is (not (bool-addr-p "TXT1")))
  (is (not (bool-addr-p "invalid"))))

(test word-addr-p
  "Test word address validation"
  (is (word-addr-p "DS1"))
  (is (word-addr-p "DS10000"))
  (is (word-addr-p "DD100"))
  (is (word-addr-p "DH500"))
  (is (word-addr-p "XD10"))
  (is (word-addr-p "YD10"))
  (is (word-addr-p "TD50"))
  (is (word-addr-p "CTD25"))
  (is (not (word-addr-p "X1")))
  (is (not (word-addr-p "DF1")))
  (is (not (word-addr-p "TXT1"))))

(test float-addr-p
  "Test float address validation"
  (is (float-addr-p "DF1"))
  (is (float-addr-p "DF2000"))
  (is (not (float-addr-p "DS1")))
  (is (not (float-addr-p "X1"))))

(test string-addr-p
  "Test string address validation"
  (is (string-addr-p "TXT1"))
  (is (string-addr-p "TXT10000"))
  (is (not (string-addr-p "DS1")))
  (is (not (string-addr-p "X1"))))

;;; ============================================================
;;; Address Range Utilities (from table-ops.lisp)
;;; ============================================================

(test parse-address
  "Test address parsing"
  (is (equal '("DS" . 100) (parse-address "DS100")))
  (is (equal '("CTD" . 25) (parse-address "CTD25")))
  (is (equal '("X" . 1) (parse-address "X1")))
  (is (equal '("TXT" . 5000) (parse-address "TXT5000"))))

(test make-address
  "Test address creation"
  (is (string= "DS100" (make-address "DS" 100)))
  (is (string= "CTD25" (make-address "CTD" 25)))
  (is (string= "X1" (make-address "X" 1))))

(test generate-address-range
  "Test address range generation"
  (is (equal '("DS1" "DS2" "DS3" "DS4" "DS5")
             (generate-address-range "DS1" "DS5")))
  (is (equal '("C10" "C9" "C8")
             (generate-address-range "C10" "C8")))
  (is (equal '("DF100")
             (generate-address-range "DF100" "DF100"))))

(test address-count
  "Test address count"
  (is (= 5 (address-count "DS1" "DS5")))
  (is (= 3 (address-count "C10" "C8")))
  (is (= 1 (address-count "DF100" "DF100")))
  (is (= 100 (address-count "X1" "X100"))))

;;; End of test-data-table.lisp
