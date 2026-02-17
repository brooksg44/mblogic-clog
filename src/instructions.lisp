;;;; instructions.lisp
;;;;
;;;; Phase 2: Instruction Definitions
;;;; Defines the complete IL instruction set using CLOS

(in-package #:mblogic-cl)

;;; ============================================================
;;; Instruction Type and Class Definitions
;;; ============================================================

(deftype instruction-type ()
  "Types of instruction data handling"
  '(member :boolean :word :float :string :mixed :none))

(deftype instruction-class ()
  "Functional classification of instructions"
  '(member
    :boolean-input      ; STR, AND, OR variants
    :boolean-output     ; OUT, SET, RST, PD
    :boolean-compare    ; STRE, STRGT, etc.
    :edge-contact       ; STRPD, ANDPD, etc.
    :stack-operation    ; ANDSTR, ORSTR
    :timer              ; TMR, TMRA, TMROFF
    :counter            ; CNTU, CNTD, UDC
    :data-move          ; COPY, CPYBLK, FILL
    :data-pack          ; PACK, UNPACK
    :math               ; MATHDEC, MATHHEX
    :search             ; FIND* variants
    :control            ; CALL, RT, END, FOR, NEXT
    :special            ; NETWORK, SBR, SHFRG
    :no-op))            ; Comments, blank lines

;;; ============================================================
;;; CLOS Class Definitions
;;; ============================================================

(defclass plc-instruction ()
  ((opcode :initarg :opcode
           :accessor instruction-opcode
           :type string
           :documentation "Instruction mnemonic (e.g., STR, AND, OUT)")
   (description :initarg :description
                :accessor instruction-description
                :type string
                :documentation "Human-readable description")
   (instr-type :initarg :instr-type
               :accessor instruction-type
               :type instruction-type
               :documentation "Data type this instruction operates on")
   (instr-class :initarg :instr-class
                :accessor instruction-class
                :type instruction-class
                :documentation "Functional classification")
   (min-params :initarg :min-params
               :accessor instruction-min-params
               :initform 0
               :type fixnum
               :documentation "Minimum number of parameters")
   (max-params :initarg :max-params
               :accessor instruction-max-params
               :initform 0
               :type fixnum
               :documentation "Maximum number of parameters")
   (param-types :initarg :param-types
                :accessor instruction-param-types
                :initform nil
                :documentation "List of parameter type specifications")
   (validator :initarg :validator
              :accessor instruction-validator
              :initform nil
              :documentation "Optional validation function")
   (ladsymb :initarg :ladsymb
            :accessor instruction-ladsymb
            :initform nil
            :documentation "Ladder diagram symbol type")
   (monitor :initarg :monitor
            :accessor instruction-monitor
            :initform nil
            :documentation "Monitoring category"))
  (:documentation "PLC instruction definition"))

(defmethod print-object ((instr plc-instruction) stream)
  (print-unreadable-object (instr stream :type t)
    (format stream "~A" (instruction-opcode instr))))

;;; ============================================================
;;; Parameter Type Specifications (Precompiled Scanners)
;;; ============================================================

(defparameter *bool-addr-pattern*
  (cl-ppcre:create-scanner "^(X|Y|C|SC|T|CT)[0-9]+$")
  "Precompiled scanner for boolean addresses")

(defparameter *word-addr-pattern*
  (cl-ppcre:create-scanner "^(DS|DD|DH|XD|YD|XS|YS|SD|TD|CTD)[0-9]+$")
  "Precompiled scanner for word addresses")

(defparameter *float-addr-pattern*
  (cl-ppcre:create-scanner "^DF[0-9]+$")
  "Precompiled scanner for float addresses")

(defparameter *string-addr-pattern*
  (cl-ppcre:create-scanner "^TXT[0-9]+$")
  "Precompiled scanner for string addresses")

(defparameter *any-addr-pattern*
  (cl-ppcre:create-scanner "^(X|Y|C|SC|T|CT|DS|DD|DH|XD|YD|XS|YS|SD|TD|CTD|DF|TXT)[0-9]+$")
  "Precompiled scanner for any address type")

(defparameter *numeric-pattern*
  (cl-ppcre:create-scanner "^-?[0-9]+\\.?[0-9]*$")
  "Precompiled scanner for numeric constants")

(defparameter *hex-constant-pattern*
  (cl-ppcre:create-scanner "^[0-9A-Fa-f]+[hH]$")
  "Precompiled scanner for hex constants")

(declaim (inline bool-addr-p word-addr-p float-addr-p string-addr-p any-addr-p))

(defun bool-addr-p (str)
  "Check if STR is a boolean address"
  (declare (optimize (speed 3) (safety 1)))
  (and (stringp str)
       (cl-ppcre:scan *bool-addr-pattern* str)))

(defun word-addr-p (str)
  "Check if STR is a word address"
  (declare (optimize (speed 3) (safety 1)))
  (and (stringp str)
       (cl-ppcre:scan *word-addr-pattern* str)))

(defun float-addr-p (str)
  "Check if STR is a float address"
  (declare (optimize (speed 3) (safety 1)))
  (and (stringp str)
       (cl-ppcre:scan *float-addr-pattern* str)))

(defun string-addr-p (str)
  "Check if STR is a string address"
  (declare (optimize (speed 3) (safety 1)))
  (and (stringp str)
       (cl-ppcre:scan *string-addr-pattern* str)))

(defun any-addr-p (str)
  "Check if STR is any valid address"
  (declare (optimize (speed 3) (safety 1)))
  (and (stringp str)
       (cl-ppcre:scan *any-addr-pattern* str)))

(defun numeric-p (str)
  "Check if STR is a numeric constant"
  (and (stringp str)
       (cl-ppcre:scan *numeric-pattern* str)))

(defun hex-constant-p (str)
  "Check if STR is a hex constant (ends with h)"
  (and (stringp str)
       (cl-ppcre:scan *hex-constant-pattern* str)))

(defun time-unit-p (str)
  "Check if STR is a time unit"
  (and (stringp str)
       (member (string-downcase str) '("ms" "sec" "min" "hour" "day")
               :test #'string=)))

;;; ============================================================
;;; Instruction Registry
;;; ============================================================

(defvar *instruction-set* (make-hash-table :test 'equal)
  "Hash table mapping opcode strings to instruction objects")

(defun register-instruction (instr)
  "Register an instruction in the instruction set"
  (setf (gethash (instruction-opcode instr) *instruction-set*) instr))

(defun find-instruction (opcode)
  "Find instruction by opcode string"
  (gethash (string-upcase opcode) *instruction-set*))

(defun list-instructions ()
  "Return list of all registered instructions"
  (loop for instr being the hash-values of *instruction-set*
        collect instr))

(defun list-instructions-by-class (class)
  "Return instructions of a specific class"
  (loop for instr being the hash-values of *instruction-set*
        when (eq (instruction-class instr) class)
        collect instr))

;;; ============================================================
;;; Instruction Definition Macro
;;; ============================================================

(defmacro define-instruction (opcode &key description type class
                                       (min-params 0) (max-params 0)
                                       param-types validator ladsymb monitor)
  "Define and register a PLC instruction"
  `(register-instruction
    (make-instance 'plc-instruction
                   :opcode ,(string-upcase (string opcode))
                   :description ,description
                   :instr-type ,type
                   :instr-class ,class
                   :min-params ,min-params
                   :max-params ,max-params
                   :param-types ',param-types
                   :validator ,validator
                   :ladsymb ,ladsymb
                   :monitor ,monitor)))

;;; ============================================================
;;; Boolean Input Instructions
;;; ============================================================

(define-instruction "STR"
  :description "Store - Load boolean address onto logic stack"
  :type :boolean
  :class :boolean-input
  :min-params 1
  :max-params 1
  :param-types (bool-addr)
  :ladsymb :contact-no
  :monitor :bool)

(define-instruction "STRN"
  :description "Store Not - Load inverted boolean onto logic stack"
  :type :boolean
  :class :boolean-input
  :min-params 1
  :max-params 1
  :param-types (bool-addr)
  :ladsymb :contact-nc
  :monitor :bool)

(define-instruction "AND"
  :description "AND - AND boolean with top of stack"
  :type :boolean
  :class :boolean-input
  :min-params 1
  :max-params 1
  :param-types (bool-addr)
  :ladsymb :contact-no
  :monitor :bool)

(define-instruction "ANDN"
  :description "AND Not - AND inverted boolean with top of stack"
  :type :boolean
  :class :boolean-input
  :min-params 1
  :max-params 1
  :param-types (bool-addr)
  :ladsymb :contact-nc
  :monitor :bool)

(define-instruction "OR"
  :description "OR - OR boolean with top of stack"
  :type :boolean
  :class :boolean-input
  :min-params 1
  :max-params 1
  :param-types (bool-addr)
  :ladsymb :contact-no
  :monitor :bool)

(define-instruction "ORN"
  :description "OR Not - OR inverted boolean with top of stack"
  :type :boolean
  :class :boolean-input
  :min-params 1
  :max-params 1
  :param-types (bool-addr)
  :ladsymb :contact-nc
  :monitor :bool)

;;; ============================================================
;;; Stack Operations
;;; ============================================================

(define-instruction "ANDSTR"
  :description "AND Stack - AND top two stack elements"
  :type :boolean
  :class :stack-operation
  :min-params 0
  :max-params 0
  :ladsymb :branch-end)

(define-instruction "ORSTR"
  :description "OR Stack - OR top two stack elements"
  :type :boolean
  :class :stack-operation
  :min-params 0
  :max-params 0
  :ladsymb :branch-end)

;;; ============================================================
;;; Boolean Output Instructions
;;; ============================================================

(define-instruction "OUT"
  :description "Output - Write top of stack to boolean address(es)"
  :type :boolean
  :class :boolean-output
  :min-params 1
  :max-params 8
  :param-types (bool-addr)
  :ladsymb :coil
  :monitor :bool)

(define-instruction "SET"
  :description "Set - Latch boolean address(es) when stack is true"
  :type :boolean
  :class :boolean-output
  :min-params 1
  :max-params 8
  :param-types (bool-addr)
  :ladsymb :coil-set
  :monitor :bool)

(define-instruction "RST"
  :description "Reset - Unlatch boolean address(es) when stack is true"
  :type :boolean
  :class :boolean-output
  :min-params 1
  :max-params 8
  :param-types (bool-addr)
  :ladsymb :coil-reset
  :monitor :bool)

(define-instruction "PD"
  :description "Pulse/Differentiate - One-shot output on rising edge"
  :type :boolean
  :class :boolean-output
  :min-params 1
  :max-params 8
  :param-types (bool-addr)
  :ladsymb :coil-pd
  :monitor :bool)

;;; ============================================================
;;; Edge Detection Instructions
;;; ============================================================

(define-instruction "STRPD"
  :description "Store Positive Differential - Detect rising edge"
  :type :boolean
  :class :edge-contact
  :min-params 1
  :max-params 1
  :param-types (bool-addr)
  :ladsymb :contact-pd
  :monitor :bool)

(define-instruction "STRND"
  :description "Store Negative Differential - Detect falling edge"
  :type :boolean
  :class :edge-contact
  :min-params 1
  :max-params 1
  :param-types (bool-addr)
  :ladsymb :contact-nd
  :monitor :bool)

(define-instruction "ANDPD"
  :description "AND Positive Differential - AND with rising edge"
  :type :boolean
  :class :edge-contact
  :min-params 1
  :max-params 1
  :param-types (bool-addr)
  :ladsymb :contact-pd
  :monitor :bool)

(define-instruction "ANDND"
  :description "AND Negative Differential - AND with falling edge"
  :type :boolean
  :class :edge-contact
  :min-params 1
  :max-params 1
  :param-types (bool-addr)
  :ladsymb :contact-nd
  :monitor :bool)

(define-instruction "ORPD"
  :description "OR Positive Differential - OR with rising edge"
  :type :boolean
  :class :edge-contact
  :min-params 1
  :max-params 1
  :param-types (bool-addr)
  :ladsymb :contact-pd
  :monitor :bool)

(define-instruction "ORND"
  :description "OR Negative Differential - OR with falling edge"
  :type :boolean
  :class :edge-contact
  :min-params 1
  :max-params 1
  :param-types (bool-addr)
  :ladsymb :contact-nd
  :monitor :bool)

;;; ============================================================
;;; Comparison Instructions - Store variants
;;; ============================================================

(define-instruction "STRE"
  :description "Store Equal - Compare two values for equality"
  :type :word
  :class :boolean-compare
  :min-params 2
  :max-params 2
  :param-types (word-or-const word-or-const)
  :ladsymb :compare
  :monitor :word)

(define-instruction "STRNE"
  :description "Store Not Equal - Compare two values for inequality"
  :type :word
  :class :boolean-compare
  :min-params 2
  :max-params 2
  :param-types (word-or-const word-or-const)
  :ladsymb :compare
  :monitor :word)

(define-instruction "STRGT"
  :description "Store Greater Than - Compare if first > second"
  :type :word
  :class :boolean-compare
  :min-params 2
  :max-params 2
  :param-types (word-or-const word-or-const)
  :ladsymb :compare
  :monitor :word)

(define-instruction "STRLT"
  :description "Store Less Than - Compare if first < second"
  :type :word
  :class :boolean-compare
  :min-params 2
  :max-params 2
  :param-types (word-or-const word-or-const)
  :ladsymb :compare
  :monitor :word)

(define-instruction "STRGE"
  :description "Store Greater/Equal - Compare if first >= second"
  :type :word
  :class :boolean-compare
  :min-params 2
  :max-params 2
  :param-types (word-or-const word-or-const)
  :ladsymb :compare
  :monitor :word)

(define-instruction "STRLE"
  :description "Store Less/Equal - Compare if first <= second"
  :type :word
  :class :boolean-compare
  :min-params 2
  :max-params 2
  :param-types (word-or-const word-or-const)
  :ladsymb :compare
  :monitor :word)

;;; ============================================================
;;; Comparison Instructions - AND variants
;;; ============================================================

(define-instruction "ANDE"
  :description "AND Equal - AND stack with equality comparison"
  :type :word
  :class :boolean-compare
  :min-params 2
  :max-params 2
  :param-types (word-or-const word-or-const)
  :ladsymb :compare
  :monitor :word)

(define-instruction "ANDNE"
  :description "AND Not Equal - AND stack with inequality comparison"
  :type :word
  :class :boolean-compare
  :min-params 2
  :max-params 2
  :param-types (word-or-const word-or-const)
  :ladsymb :compare
  :monitor :word)

(define-instruction "ANDGT"
  :description "AND Greater Than - AND stack with > comparison"
  :type :word
  :class :boolean-compare
  :min-params 2
  :max-params 2
  :param-types (word-or-const word-or-const)
  :ladsymb :compare
  :monitor :word)

(define-instruction "ANDLT"
  :description "AND Less Than - AND stack with < comparison"
  :type :word
  :class :boolean-compare
  :min-params 2
  :max-params 2
  :param-types (word-or-const word-or-const)
  :ladsymb :compare
  :monitor :word)

(define-instruction "ANDGE"
  :description "AND Greater/Equal - AND stack with >= comparison"
  :type :word
  :class :boolean-compare
  :min-params 2
  :max-params 2
  :param-types (word-or-const word-or-const)
  :ladsymb :compare
  :monitor :word)

(define-instruction "ANDLE"
  :description "AND Less/Equal - AND stack with <= comparison"
  :type :word
  :class :boolean-compare
  :min-params 2
  :max-params 2
  :param-types (word-or-const word-or-const)
  :ladsymb :compare
  :monitor :word)

;;; ============================================================
;;; Comparison Instructions - OR variants
;;; ============================================================

(define-instruction "ORE"
  :description "OR Equal - OR stack with equality comparison"
  :type :word
  :class :boolean-compare
  :min-params 2
  :max-params 2
  :param-types (word-or-const word-or-const)
  :ladsymb :compare
  :monitor :word)

(define-instruction "ORNE"
  :description "OR Not Equal - OR stack with inequality comparison"
  :type :word
  :class :boolean-compare
  :min-params 2
  :max-params 2
  :param-types (word-or-const word-or-const)
  :ladsymb :compare
  :monitor :word)

(define-instruction "ORGT"
  :description "OR Greater Than - OR stack with > comparison"
  :type :word
  :class :boolean-compare
  :min-params 2
  :max-params 2
  :param-types (word-or-const word-or-const)
  :ladsymb :compare
  :monitor :word)

(define-instruction "ORLT"
  :description "OR Less Than - OR stack with < comparison"
  :type :word
  :class :boolean-compare
  :min-params 2
  :max-params 2
  :param-types (word-or-const word-or-const)
  :ladsymb :compare
  :monitor :word)

(define-instruction "ORGE"
  :description "OR Greater/Equal - OR stack with >= comparison"
  :type :word
  :class :boolean-compare
  :min-params 2
  :max-params 2
  :param-types (word-or-const word-or-const)
  :ladsymb :compare
  :monitor :word)

(define-instruction "ORLE"
  :description "OR Less/Equal - OR stack with <= comparison"
  :type :word
  :class :boolean-compare
  :min-params 2
  :max-params 2
  :param-types (word-or-const word-or-const)
  :ladsymb :compare
  :monitor :word)

;;; ============================================================
;;; Timer Instructions
;;; ============================================================

(define-instruction "TMR"
  :description "Timer On-Delay - Timer with preset and time base"
  :type :mixed
  :class :timer
  :min-params 2
  :max-params 3
  :param-types (timer-addr word-or-const time-unit)
  :ladsymb :timer
  :monitor :timer)

(define-instruction "TMRA"
  :description "Timer Accumulating - Retentive on-delay timer"
  :type :mixed
  :class :timer
  :min-params 2
  :max-params 3
  :param-types (timer-addr word-or-const time-unit)
  :ladsymb :timer
  :monitor :timer)

(define-instruction "TMROFF"
  :description "Timer Off-Delay - Off-delay timer"
  :type :mixed
  :class :timer
  :min-params 2
  :max-params 3
  :param-types (timer-addr word-or-const time-unit)
  :ladsymb :timer
  :monitor :timer)

;;; ============================================================
;;; Counter Instructions
;;; ============================================================

(define-instruction "CNTU"
  :description "Counter Up - Increment counter on rising edge"
  :type :mixed
  :class :counter
  :min-params 2
  :max-params 2
  :param-types (counter-addr word-or-const)
  :ladsymb :counter
  :monitor :counter)

(define-instruction "CNTD"
  :description "Counter Down - Decrement counter on rising edge"
  :type :mixed
  :class :counter
  :min-params 2
  :max-params 2
  :param-types (counter-addr word-or-const)
  :ladsymb :counter
  :monitor :counter)

(define-instruction "UDC"
  :description "Up/Down Counter - Bidirectional counter"
  :type :mixed
  :class :counter
  :min-params 2
  :max-params 2
  :param-types (counter-addr word-or-const)
  :ladsymb :counter
  :monitor :counter)

;;; ============================================================
;;; Data Movement Instructions
;;; ============================================================

(define-instruction "COPY"
  :description "Copy - Copy single value from source to destination"
  :type :mixed
  :class :data-move
  :min-params 2
  :max-params 2
  :param-types (any-or-const any-addr)
  :ladsymb :copy
  :monitor :word)

(define-instruction "CPYBLK"
  :description "Copy Block - Copy range of values"
  :type :mixed
  :class :data-move
  :min-params 3
  :max-params 3
  :param-types (any-addr any-addr any-addr)
  :ladsymb :cpyblk
  :monitor :word)

(define-instruction "FILL"
  :description "Fill - Fill range with constant value"
  :type :mixed
  :class :data-move
  :min-params 3
  :max-params 3
  :param-types (any-or-const any-addr any-addr)
  :ladsymb :fill
  :monitor :word)

;;; ============================================================
;;; Pack/Unpack Instructions
;;; ============================================================

(define-instruction "PACK"
  :description "Pack - Pack boolean range into word"
  :type :mixed
  :class :data-pack
  :min-params 3
  :max-params 3
  :param-types (bool-addr bool-addr word-addr)
  :ladsymb :pack
  :monitor :word)

(define-instruction "UNPACK"
  :description "Unpack - Unpack word into boolean range"
  :type :mixed
  :class :data-pack
  :min-params 3
  :max-params 3
  :param-types (word-addr bool-addr bool-addr)
  :ladsymb :unpack
  :monitor :word)

;;; ============================================================
;;; Math Instructions
;;; ============================================================

(define-instruction "MATHDEC"
  :description "Math Decimal - Evaluate decimal math expression"
  :type :mixed
  :class :math
  :min-params 3
  :max-params 100
  :param-types (any-addr flags expression)
  :ladsymb :math
  :monitor :word)

(define-instruction "MATHHEX"
  :description "Math Hex - Evaluate hexadecimal/bitwise expression"
  :type :mixed
  :class :math
  :min-params 3
  :max-params 100
  :param-types (any-addr flags expression)
  :ladsymb :math
  :monitor :word)

(define-instruction "SUM"
  :description "Sum - Sum array of values"
  :type :word
  :class :math
  :min-params 4
  :max-params 4
  :param-types (word-addr word-addr word-addr flags)
  :ladsymb :sum
  :monitor :word)

;;; ============================================================
;;; Search Instructions - Non-incremental
;;; ============================================================

(define-instruction "FINDEQ"
  :description "Find Equal - Search for equal value in table"
  :type :mixed
  :class :search
  :min-params 5
  :max-params 6
  :param-types (any-or-const any-addr any-addr word-addr bool-addr flags)
  :ladsymb :find
  :monitor :word)

(define-instruction "FINDNE"
  :description "Find Not Equal - Search for not equal value"
  :type :mixed
  :class :search
  :min-params 5
  :max-params 6
  :param-types (any-or-const any-addr any-addr word-addr bool-addr flags)
  :ladsymb :find
  :monitor :word)

(define-instruction "FINDGT"
  :description "Find Greater Than - Search for > value"
  :type :mixed
  :class :search
  :min-params 5
  :max-params 6
  :param-types (any-or-const any-addr any-addr word-addr bool-addr flags)
  :ladsymb :find
  :monitor :word)

(define-instruction "FINDLT"
  :description "Find Less Than - Search for < value"
  :type :mixed
  :class :search
  :min-params 5
  :max-params 6
  :param-types (any-or-const any-addr any-addr word-addr bool-addr flags)
  :ladsymb :find
  :monitor :word)

(define-instruction "FINDGE"
  :description "Find Greater/Equal - Search for >= value"
  :type :mixed
  :class :search
  :min-params 5
  :max-params 6
  :param-types (any-or-const any-addr any-addr word-addr bool-addr flags)
  :ladsymb :find
  :monitor :word)

(define-instruction "FINDLE"
  :description "Find Less/Equal - Search for <= value"
  :type :mixed
  :class :search
  :min-params 5
  :max-params 6
  :param-types (any-or-const any-addr any-addr word-addr bool-addr flags)
  :ladsymb :find
  :monitor :word)

;;; ============================================================
;;; Search Instructions - Incremental
;;; ============================================================

(define-instruction "FINDIEQ"
  :description "Find Incremental Equal - Continue search for equal"
  :type :mixed
  :class :search
  :min-params 5
  :max-params 6
  :param-types (any-or-const any-addr any-addr word-addr bool-addr flags)
  :ladsymb :find
  :monitor :word)

(define-instruction "FINDINE"
  :description "Find Incremental Not Equal - Continue search"
  :type :mixed
  :class :search
  :min-params 5
  :max-params 6
  :param-types (any-or-const any-addr any-addr word-addr bool-addr flags)
  :ladsymb :find
  :monitor :word)

(define-instruction "FINDIGT"
  :description "Find Incremental Greater Than - Continue search"
  :type :mixed
  :class :search
  :min-params 5
  :max-params 6
  :param-types (any-or-const any-addr any-addr word-addr bool-addr flags)
  :ladsymb :find
  :monitor :word)

(define-instruction "FINDILT"
  :description "Find Incremental Less Than - Continue search"
  :type :mixed
  :class :search
  :min-params 5
  :max-params 6
  :param-types (any-or-const any-addr any-addr word-addr bool-addr flags)
  :ladsymb :find
  :monitor :word)

(define-instruction "FINDIGE"
  :description "Find Incremental Greater/Equal - Continue search"
  :type :mixed
  :class :search
  :min-params 5
  :max-params 6
  :param-types (any-or-const any-addr any-addr word-addr bool-addr flags)
  :ladsymb :find
  :monitor :word)

(define-instruction "FINDILE"
  :description "Find Incremental Less/Equal - Continue search"
  :type :mixed
  :class :search
  :min-params 5
  :max-params 6
  :param-types (any-or-const any-addr any-addr word-addr bool-addr flags)
  :ladsymb :find
  :monitor :word)

;;; ============================================================
;;; Control Flow Instructions
;;; ============================================================

(define-instruction "CALL"
  :description "Call - Call subroutine"
  :type :none
  :class :control
  :min-params 1
  :max-params 1
  :param-types (subroutine-name)
  :ladsymb :call
  :monitor nil)

(define-instruction "RT"
  :description "Return - Return from subroutine"
  :type :none
  :class :control
  :min-params 0
  :max-params 0
  :ladsymb :return
  :monitor nil)

(define-instruction "RTC"
  :description "Return Conditional - Return if stack is true"
  :type :none
  :class :control
  :min-params 0
  :max-params 0
  :ladsymb :return
  :monitor nil)

(define-instruction "END"
  :description "End - End program execution"
  :type :none
  :class :control
  :min-params 0
  :max-params 0
  :ladsymb :end
  :monitor nil)

(define-instruction "ENDC"
  :description "End Conditional - End if stack is true"
  :type :none
  :class :control
  :min-params 0
  :max-params 0
  :ladsymb :end
  :monitor nil)

(define-instruction "FOR"
  :description "For - Begin for/next loop"
  :type :word
  :class :control
  :min-params 1
  :max-params 1
  :param-types (word-or-const)
  :ladsymb :for
  :monitor :word)

(define-instruction "NEXT"
  :description "Next - End for/next loop"
  :type :none
  :class :control
  :min-params 0
  :max-params 0
  :ladsymb :next
  :monitor nil)

;;; ============================================================
;;; Special Instructions
;;; ============================================================

(define-instruction "NETWORK"
  :description "Network - Begin new network/rung"
  :type :none
  :class :special
  :min-params 1
  :max-params 1
  :param-types (network-number)
  :ladsymb nil
  :monitor nil)

(define-instruction "SBR"
  :description "Subroutine - Begin subroutine definition"
  :type :none
  :class :special
  :min-params 1
  :max-params 1
  :param-types (subroutine-name)
  :ladsymb nil
  :monitor nil)

(define-instruction "SHFRG"
  :description "Shift Register - Shift boolean values through range"
  :type :boolean
  :class :special
  :min-params 2
  :max-params 2
  :param-types (bool-addr bool-addr)
  :ladsymb :shfrg
  :monitor :bool)

;;; ============================================================
;;; Validation Functions
;;; ============================================================

(defgeneric validate-instruction (instr params)
  (:documentation "Validate parameters for an instruction"))

(defmethod validate-instruction ((instr plc-instruction) params)
  "Default validation - check parameter count"
  (let ((count (length params))
        (min-p (instruction-min-params instr))
        (max-p (instruction-max-params instr)))
    (cond
      ((< count min-p)
       (values nil (format nil "~A requires at least ~D parameter~:P, got ~D"
                           (instruction-opcode instr) min-p count)))
      ((> count max-p)
       (values nil (format nil "~A accepts at most ~D parameter~:P, got ~D"
                           (instruction-opcode instr) max-p count)))
      (t (values t nil)))))

;;; ============================================================
;;; Instruction Set Initialization
;;; ============================================================

(defun make-instruction-set ()
  "Initialize and return the instruction set hash table.
   Instructions are registered when this file loads."
  *instruction-set*)

(defun instruction-count ()
  "Return count of registered instructions"
  (hash-table-count *instruction-set*))

(defun print-instruction-summary ()
  "Print summary of registered instructions by class"
  (format t "~%=== Instruction Set Summary ===~%")
  (format t "Total instructions: ~D~%~%" (instruction-count))
  (dolist (class '(:boolean-input :boolean-output :boolean-compare
                   :edge-contact :stack-operation :timer :counter
                   :data-move :data-pack :math :search :control :special))
    (let ((instrs (list-instructions-by-class class)))
      (when instrs
        (format t "~A (~D):~%" class (length instrs))
        (format t "  ~{~A~^, ~}~%"
                (mapcar #'instruction-opcode instrs))))))

;;; End of instructions.lisp
