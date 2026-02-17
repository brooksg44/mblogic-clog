;;;; data-table.lisp
;;;;
;;;; Phase 1: Core Data Structures
;;;; Implements PLC memory spaces (Boolean, Word, Float, String)

(in-package #:mblogic-cl)

;;; CLOS Class Definitions

(defclass data-table ()
  ((bool-table :initform (make-hash-table :test 'equal)
               :accessor bool-table
               :documentation "Boolean data table (X, Y, C, SC, T, CT)")
   (word-table :initform (make-hash-table :test 'equal)
               :accessor word-table
               :documentation "Word data table (DS, DD, DH, XD, YD, XS, YS, TD, CTD)")
   (float-table :initform (make-hash-table :test 'equal)
                :accessor float-table
                :documentation "Floating point data table (DF)")
   (string-table :initform (make-hash-table :test 'equal)
                 :accessor string-table
                 :documentation "String data table (TXT)")
   (instr-table :initform (make-hash-table :test 'equal)
                :accessor instr-table
                :documentation "Instruction data table for differentiating instructions"))
  (:documentation "PLC data table containing all memory spaces"))

;;; Constructor

(defun make-data-table ()
  "Create a new PLC data table"
  (make-instance 'data-table))

;;; Address Generation

(defun generate-addresses (prefix start end)
  "Generate address labels from START to END with PREFIX
   Example: (generate-addresses \"X\" 1 10) => (\"X1\" \"X2\" ... \"X10\")"
  (loop for i from start to end
        collect (format nil "~A~D" prefix i)))

;;; Initialization

(defun init-data-table (dt)
  "Initialize all address spaces in data table DT"
  ;; Boolean addresses
  (init-boolean-addresses dt)
  ;; Word addresses
  (init-word-addresses dt)
  ;; Float addresses
  (init-float-addresses dt)
  ;; String addresses
  (init-string-addresses dt)
  dt)

(defun init-boolean-addresses (dt)
  "Initialize Boolean address spaces"
  (let ((bool-table (bool-table dt)))
    ;; X1-X2000: Inputs
    (dolist (addr (generate-addresses "X" 1 2000))
      (setf (gethash addr bool-table) nil))
    ;; Y1-Y2000: Outputs
    (dolist (addr (generate-addresses "Y" 1 2000))
      (setf (gethash addr bool-table) nil))
    ;; C1-C2000: Control relays
    (dolist (addr (generate-addresses "C" 1 2000))
      (setf (gethash addr bool-table) nil))
    ;; SC1-SC1000: System control
    (dolist (addr (generate-addresses "SC" 1 1000))
      (setf (gethash addr bool-table) nil))
    ;; T1-T500: Timer status
    (dolist (addr (generate-addresses "T" 1 500))
      (setf (gethash addr bool-table) nil))
    ;; CT1-CT250: Counter status
    (dolist (addr (generate-addresses "CT" 1 250))
      (setf (gethash addr bool-table) nil))))

(defun init-word-addresses (dt)
  "Initialize Word address spaces"
  (let ((word-table (word-table dt)))
    ;; XD1-XD125: Input registers (unsigned)
    (dolist (addr (generate-addresses "XD" 1 125))
      (setf (gethash addr word-table) 0))
    ;; YD1-YD125: Output registers (unsigned)
    (dolist (addr (generate-addresses "YD" 1 125))
      (setf (gethash addr word-table) 0))
    ;; XS1-XS125: Input registers (signed)
    (dolist (addr (generate-addresses "XS" 1 125))
      (setf (gethash addr word-table) 0))
    ;; YS1-YS125: Output registers (signed)
    (dolist (addr (generate-addresses "YS" 1 125))
      (setf (gethash addr word-table) 0))
    ;; DS1-DS10000: Data registers (signed integer)
    (dolist (addr (generate-addresses "DS" 1 10000))
      (setf (gethash addr word-table) 0))
    ;; DD1-DD2000: Data registers (double integer)
    (dolist (addr (generate-addresses "DD" 1 2000))
      (setf (gethash addr word-table) 0))
    ;; DH1-DH2000: Data registers (hex/unsigned)
    (dolist (addr (generate-addresses "DH" 1 2000))
      (setf (gethash addr word-table) 0))
    ;; SD1-SD1000: System data (signed integer)
    (dolist (addr (generate-addresses "SD" 1 1000))
      (setf (gethash addr word-table) 0))
    ;; TD1-TD500: Timer current value
    (dolist (addr (generate-addresses "TD" 1 500))
      (setf (gethash addr word-table) 0))
    ;; CTD1-CTD250: Counter current value
    (dolist (addr (generate-addresses "CTD" 1 250))
      (setf (gethash addr word-table) 0))))

(defun init-float-addresses (dt)
  "Initialize Float address spaces"
  (let ((float-table (float-table dt)))
    ;; DF1-DF2000: Floating point data
    (dolist (addr (generate-addresses "DF" 1 2000))
      (setf (gethash addr float-table) 0.0))))

(defun init-string-addresses (dt)
  "Initialize String address spaces"
  (let ((string-table (string-table dt)))
    ;; TXT1-TXT10000: Text/string data
    (dolist (addr (generate-addresses "TXT" 1 10000))
      (setf (gethash addr string-table) ""))))

;;; Access Functions

(defun get-bool (dt address)
  "Get Boolean value at ADDRESS from data table DT"
  (gethash address (bool-table dt)))

(defun set-bool (dt address value)
  "Set Boolean value at ADDRESS in data table DT to VALUE"
  (setf (gethash address (bool-table dt)) value))

(defun get-word (dt address)
  "Get Word value at ADDRESS from data table DT"
  (gethash address (word-table dt)))

(defun set-word (dt address value)
  "Set Word value at ADDRESS in data table DT to VALUE"
  (setf (gethash address (word-table dt)) value))

(defun get-float (dt address)
  "Get Float value at ADDRESS from data table DT"
  (gethash address (float-table dt)))

(defun set-float (dt address value)
  "Set Float value at ADDRESS in data table DT to VALUE"
  (setf (gethash address (float-table dt)) value))

(defun get-string (dt address)
  "Get String value at ADDRESS from data table DT"
  (gethash address (string-table dt)))

(defun set-string (dt address value)
  "Set String value at ADDRESS in data table DT to VALUE"
  (setf (gethash address (string-table dt)) value))

;;; TODO: Phase 1 implementation
;;; - Add address validation
;;; - Add address range lookups
;;; - Add bulk read/write operations
;;; - Optimize initialization for large address spaces
