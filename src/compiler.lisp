;;;; compiler.lisp
;;;;
;;;; Phase 4: IL Compiler
;;;; Generates executable Lisp code from parsed IL

(in-package #:mblogic-cl)

;;; ============================================================
;;; Compile Error Condition
;;; ============================================================

(define-condition plc-compile-error (error)
  ((message :initarg :message :reader compile-error-message)
   (instruction :initarg :instruction :reader compile-error-instruction :initform nil)
   (line-number :initarg :line-number :reader compile-error-line :initform nil))
  (:report (lambda (c stream)
             (format stream "Compile error~@[ at line ~D~]: ~A"
                     (compile-error-line c)
                     (compile-error-message c)))))

;;; ============================================================
;;; Runtime Conditions (signaled by compiled code)
;;; ============================================================

(define-condition plc-end-condition (condition)
  ((end-type :initarg :type :reader end-type :initform :normal))
  (:documentation "Signaled when END or ENDC is executed"))

(define-condition plc-return-condition (condition)
  ((return-type :initarg :type :reader return-type :initform :normal))
  (:documentation "Signaled when RT or RTC is executed"))

;;; ============================================================
;;; Compiled Program Structure
;;; ============================================================

(defclass compiled-program ()
  ((main-function :initarg :main-function
                  :accessor program-main-function
                  :documentation "Compiled main program function")
   (subroutines :initarg :subroutines
                :accessor program-compiled-subroutines
                :initform (make-hash-table :test 'equal)
                :documentation "Hash table of compiled subroutine functions")
   (source-program :initarg :source-program
                   :accessor program-source
                   :documentation "Reference to parsed program"))
  (:documentation "A compiled IL program ready for execution"))

;;; ============================================================
;;; IL Compiler Class
;;; ============================================================

(defclass il-compiler ()
  ((errors :accessor compiler-errors
           :initform nil
           :documentation "List of compilation errors")
   (warnings :accessor compiler-warnings
             :initform nil
             :documentation "List of compilation warnings")
   (current-subroutine :accessor compiler-current-subroutine
                       :initform nil
                       :documentation "Name of subroutine being compiled"))
  (:documentation "IL to Lisp compiler"))

(defun make-il-compiler ()
  "Create a new IL compiler"
  (make-instance 'il-compiler))

;;; ============================================================
;;; Code Generation Helpers
;;; ============================================================

(defun gen-get-bool (addr)
  "Generate code to get boolean value"
  `(get-bool data-table ,addr))

(defun gen-set-bool (addr value)
  "Generate code to set boolean value"
  `(set-bool data-table ,addr ,value))

(defun gen-get-word (addr)
  "Generate code to get word value"
  `(get-word data-table ,addr))

(defun gen-set-word (addr value)
  "Generate code to set word value"
  `(set-word data-table ,addr ,value))

(defun gen-get-float (addr)
  "Generate code to get float value"
  `(get-float data-table ,addr))

(defun gen-set-float (addr value)
  "Generate code to set float value"
  `(set-float data-table ,addr ,value))

(defun gen-get-string (addr)
  "Generate code to get string value"
  `(get-string data-table ,addr))

(defun gen-set-string (addr value)
  "Generate code to set string value"
  `(set-string data-table ,addr ,value))

(defun gen-get-value (addr)
  "Generate code to get value based on address type"
  (cond
    ((bool-addr-p addr) (gen-get-bool addr))
    ((word-addr-p addr) (gen-get-word addr))
    ((float-addr-p addr) (gen-get-float addr))
    ((string-addr-p addr) (gen-get-string addr))
    ((numeric-p addr) (parse-number:parse-number addr))
    ((hex-constant-p addr) (parse-integer (string-right-trim "hH" addr) :radix 16))
    (t addr)))  ; Return as-is for constants or expressions

(defun gen-set-value (addr value)
  "Generate code to set value based on address type"
  (cond
    ((bool-addr-p addr) (gen-set-bool addr value))
    ((word-addr-p addr) (gen-set-word addr value))
    ((float-addr-p addr) (gen-set-float addr value))
    ((string-addr-p addr) (gen-set-string addr value))
    (t (error "Cannot set value to non-address: ~A" addr))))

(defun parse-value (param)
  "Parse a parameter value - returns code to get the value at runtime"
  (cond
    ((bool-addr-p param) (gen-get-bool param))
    ((word-addr-p param) (gen-get-word param))
    ((float-addr-p param) (gen-get-float param))
    ((string-addr-p param) (gen-get-string param))
    ((numeric-p param) (parse-number:parse-number param))
    ((hex-constant-p param) (parse-integer (string-right-trim "hH" param) :radix 16))
    ((and (stringp param) (> (length param) 0)
          (char= (char param 0) #\"))
     ;; Quoted string - remove quotes
     (subseq param 1 (1- (length param))))
    (t param)))

;;; ============================================================
;;; Instruction Compilation - Boolean Input
;;; ============================================================

(defun compile-str (params)
  "STR - Store boolean on stack"
  (let ((addr (first params)))
    `(progn
       (push ,(gen-get-bool addr) logic-stack)
       (setf stacktop (first logic-stack)))))

(defun compile-strn (params)
  "STRN - Store NOT boolean on stack"
  (let ((addr (first params)))
    `(progn
       (push (not ,(gen-get-bool addr)) logic-stack)
       (setf stacktop (first logic-stack)))))

(defun compile-and (params)
  "AND - AND with top of stack"
  (let ((addr (first params)))
    `(progn
       (setf stacktop (and stacktop ,(gen-get-bool addr)))
       (setf (first logic-stack) stacktop))))

(defun compile-andn (params)
  "ANDN - AND NOT with top of stack"
  (let ((addr (first params)))
    `(progn
       (setf stacktop (and stacktop (not ,(gen-get-bool addr))))
       (setf (first logic-stack) stacktop))))

(defun compile-or (params)
  "OR - OR with top of stack"
  (let ((addr (first params)))
    `(progn
       (setf stacktop (or stacktop ,(gen-get-bool addr)))
       (setf (first logic-stack) stacktop))))

(defun compile-orn (params)
  "ORN - OR NOT with top of stack"
  (let ((addr (first params)))
    `(progn
       (setf stacktop (or stacktop (not ,(gen-get-bool addr))))
       (setf (first logic-stack) stacktop))))

;;; ============================================================
;;; Instruction Compilation - Stack Operations
;;; ============================================================

(defun compile-andstr (params)
  "ANDSTR - AND top two stack elements"
  (declare (ignore params))
  `(progn
     (let ((top (pop logic-stack))
           (second (pop logic-stack)))
       (push (and top second) logic-stack)
       (setf stacktop (first logic-stack)))))

(defun compile-orstr (params)
  "ORSTR - OR top two stack elements"
  (declare (ignore params))
  `(progn
     (let ((top (pop logic-stack))
           (second (pop logic-stack)))
       (push (or top second) logic-stack)
       (setf stacktop (first logic-stack)))))

;;; ============================================================
;;; Instruction Compilation - Boolean Output
;;; ============================================================

(defun compile-out (params)
  "OUT - Output stacktop to address(es)"
  `(progn
     ,@(mapcar (lambda (addr)
                 (gen-set-bool addr 'stacktop))
               params)))

(defun compile-set (params)
  "SET - Latch address(es) if stacktop is true"
  `(when stacktop
     ,@(mapcar (lambda (addr)
                 (gen-set-bool addr t))
               params)))

(defun compile-rst (params)
  "RST - Reset address(es) if stacktop is true"
  `(when stacktop
     ,@(mapcar (lambda (addr)
                 (gen-set-bool addr nil))
               params)))

(defun compile-pd (params)
  "PD - Pulse/differentiate output on rising edge"
  ;; Uses instr-table to track previous state
  `(progn
     ,@(mapcar (lambda (addr)
                 (let ((edge-key (format nil "~A-pd" addr)))
                   `(let ((prev (gethash ,edge-key (instr-table data-table))))
                      (when (and stacktop (not prev))
                        ,(gen-set-bool addr t))
                      (when (and (not stacktop) prev)
                        ,(gen-set-bool addr nil))
                      (setf (gethash ,edge-key (instr-table data-table)) stacktop))))
               params)))

;;; ============================================================
;;; Instruction Compilation - Edge Detection
;;; ============================================================

(defun compile-strpd (params)
  "STRPD - Store positive differential (rising edge)"
  (let* ((addr (first params))
         (edge-key (format nil "~A-strpd" addr)))
    `(let* ((current ,(gen-get-bool addr))
            (prev (gethash ,edge-key (instr-table data-table)))
            (edge (and current (not prev))))
       (setf (gethash ,edge-key (instr-table data-table)) current)
       (push edge logic-stack)
       (setf stacktop edge))))

(defun compile-strnd (params)
  "STRND - Store negative differential (falling edge)"
  (let* ((addr (first params))
         (edge-key (format nil "~A-strnd" addr)))
    `(let* ((current ,(gen-get-bool addr))
            (prev (gethash ,edge-key (instr-table data-table)))
            (edge (and (not current) prev)))
       (setf (gethash ,edge-key (instr-table data-table)) current)
       (push edge logic-stack)
       (setf stacktop edge))))

(defun compile-andpd (params)
  "ANDPD - AND with positive differential"
  (let* ((addr (first params))
         (edge-key (format nil "~A-andpd" addr)))
    `(let* ((current ,(gen-get-bool addr))
            (prev (gethash ,edge-key (instr-table data-table)))
            (edge (and current (not prev))))
       (setf (gethash ,edge-key (instr-table data-table)) current)
       (setf stacktop (and stacktop edge))
       (setf (first logic-stack) stacktop))))

(defun compile-andnd (params)
  "ANDND - AND with negative differential"
  (let* ((addr (first params))
         (edge-key (format nil "~A-andnd" addr)))
    `(let* ((current ,(gen-get-bool addr))
            (prev (gethash ,edge-key (instr-table data-table)))
            (edge (and (not current) prev)))
       (setf (gethash ,edge-key (instr-table data-table)) current)
       (setf stacktop (and stacktop edge))
       (setf (first logic-stack) stacktop))))

(defun compile-orpd (params)
  "ORPD - OR with positive differential"
  (let* ((addr (first params))
         (edge-key (format nil "~A-orpd" addr)))
    `(let* ((current ,(gen-get-bool addr))
            (prev (gethash ,edge-key (instr-table data-table)))
            (edge (and current (not prev))))
       (setf (gethash ,edge-key (instr-table data-table)) current)
       (setf stacktop (or stacktop edge))
       (setf (first logic-stack) stacktop))))

(defun compile-ornd (params)
  "ORND - OR with negative differential"
  (let* ((addr (first params))
         (edge-key (format nil "~A-ornd" addr)))
    `(let* ((current ,(gen-get-bool addr))
            (prev (gethash ,edge-key (instr-table data-table)))
            (edge (and (not current) prev)))
       (setf (gethash ,edge-key (instr-table data-table)) current)
       (setf stacktop (or stacktop edge))
       (setf (first logic-stack) stacktop))))

;;; ============================================================
;;; Instruction Compilation - Comparisons
;;; ============================================================

(defun compile-compare (op params stack-op)
  "Generic comparison compilation"
  (let ((val1 (parse-value (first params)))
        (val2 (parse-value (second params))))
    (let ((compare-expr
            (case op
              (:eq `(eql ,val1 ,val2))
              (:ne `(not (eql ,val1 ,val2)))
              (:gt `(> ,val1 ,val2))
              (:lt `(< ,val1 ,val2))
              (:ge `(>= ,val1 ,val2))
              (:le `(<= ,val1 ,val2)))))
      (case stack-op
        (:str `(progn
                 (push ,compare-expr logic-stack)
                 (setf stacktop (first logic-stack))))
        (:and `(progn
                 (setf stacktop (and stacktop ,compare-expr))
                 (setf (first logic-stack) stacktop)))
        (:or `(progn
                (setf stacktop (or stacktop ,compare-expr))
                (setf (first logic-stack) stacktop)))))))

(defun compile-stre (params) (compile-compare :eq params :str))
(defun compile-strne (params) (compile-compare :ne params :str))
(defun compile-strgt (params) (compile-compare :gt params :str))
(defun compile-strlt (params) (compile-compare :lt params :str))
(defun compile-strge (params) (compile-compare :ge params :str))
(defun compile-strle (params) (compile-compare :le params :str))

(defun compile-ande (params) (compile-compare :eq params :and))
(defun compile-andne (params) (compile-compare :ne params :and))
(defun compile-andgt (params) (compile-compare :gt params :and))
(defun compile-andlt (params) (compile-compare :lt params :and))
(defun compile-andge (params) (compile-compare :ge params :and))
(defun compile-andle (params) (compile-compare :le params :and))

(defun compile-ore (params) (compile-compare :eq params :or))
(defun compile-orne (params) (compile-compare :ne params :or))
(defun compile-orgt (params) (compile-compare :gt params :or))
(defun compile-orlt (params) (compile-compare :lt params :or))
(defun compile-orge (params) (compile-compare :ge params :or))
(defun compile-orle (params) (compile-compare :le params :or))

;;; ============================================================
;;; Instruction Compilation - Data Movement
;;; ============================================================

(defun compile-copy (params)
  "COPY - Copy value from source to destination"
  (let ((source (first params))
        (dest (second params)))
    `(when stacktop
       ,(gen-set-value dest (parse-value source)))))

(defun compile-cpyblk (params)
  "CPYBLK - Copy block of values"
  (let ((source-start (first params))
        (source-end (second params))
        (dest-start (third params)))
    `(when stacktop
       (copy-block data-table ,source-start ,source-end ,dest-start))))

(defun compile-fill (params)
  "FILL - Fill range with value"
  (let ((value (first params))
        (dest-start (second params))
        (dest-end (third params)))
    `(when stacktop
       (fill-range data-table ,(parse-value value) ,dest-start ,dest-end))))

;;; ============================================================
;;; Instruction Compilation - Pack/Unpack
;;; ============================================================

(defun compile-pack (params)
  "PACK - Pack booleans into word"
  (let ((bool-start (first params))
        (bool-end (second params))
        (word-addr (third params)))
    `(when stacktop
       (pack-bits data-table ,bool-start ,bool-end ,word-addr))))

(defun compile-unpack (params)
  "UNPACK - Unpack word into booleans"
  (let ((word-addr (first params))
        (bool-start (second params))
        (bool-end (third params)))
    `(when stacktop
       (unpack-bits data-table ,word-addr ,bool-start ,bool-end))))

;;; ============================================================
;;; Instruction Compilation - Math (compile-time expression parsing)
;;; ============================================================

(defun substitute-data-table (code)
  "Replace *math-data-table* with data-table in generated code"
  (cond
    ((eq code '*math-data-table*) 'data-table)
    ((atom code) code)
    (t (mapcar #'substitute-data-table code))))

(defun compile-mathdec (params)
  "MATHDEC - Decimal math expression (compile-time parsing)"
  (let ((dest (first params))
        (flags (second params))
        (expr (third params)))
    (declare (ignore flags))
    ;; Parse the expression at compile time
    (let* ((*math-hex-mode* nil)
           (*math-tokens* (tokenize-math-expr expr))
           (raw-code (parse-expression))
           ;; Replace *math-data-table* with data-table for direct access
           (code (substitute-data-table raw-code)))
      `(when stacktop
         (let ((result ,code))
           ,(cond
              ((word-addr-p dest)
               `(set-word data-table ,dest (truncate result)))
              ((float-addr-p dest)
               `(set-float data-table ,dest (float result 1.0d0)))
              (t (error "MATHDEC: Invalid destination: ~A" dest))))))))

(defun compile-mathhex (params)
  "MATHHEX - Hexadecimal/bitwise math expression (compile-time parsing)"
  (let ((dest (first params))
        (flags (second params))
        (expr (third params)))
    (declare (ignore flags))
    ;; Parse the expression at compile time
    (let* ((*math-hex-mode* t)
           (*math-tokens* (tokenize-math-expr expr))
           (raw-code (parse-expression))
           ;; Replace *math-data-table* with data-table for direct access
           (code (substitute-data-table raw-code)))
      `(when stacktop
         (let ((result (truncate ,code)))
           ,(if (word-addr-p dest)
                `(set-word data-table ,dest result)
                (error "MATHHEX: Invalid destination: ~A" dest)))))))

(defun compile-sum (params)
  "SUM - Sum array of values"
  (let ((start (first params))
        (end (second params))
        (dest (third params))
        (flags (fourth params)))
    `(when stacktop
       (sum-range data-table ,start ,end ,dest ,(parse-value flags)))))

;;; ============================================================
;;; Instruction Compilation - Timers (delegates to Phase 6)
;;; ============================================================

(defun parse-time-value (value unit)
  "Convert time value and unit to milliseconds"
  (let ((u (if unit (string-downcase unit) "ms")))
    (cond
      ((string= u "ms")
       (typecase value
         (string 
          (cond
            ((word-addr-p value) (gen-get-word value))
            ((numeric-p value) (parse-number:parse-number value))
            ((hex-constant-p value)
             (parse-integer (string-right-trim "hH" value) :radix 16))
            (t (error "Invalid time value: ~A" value))))
         (t value)))
      ((string= u "sec")
       `(let ((v ,(typecase value
                    (string 
                     (cond
                       ((word-addr-p value) (gen-get-word value))
                       ((numeric-p value) (parse-number:parse-number value))
                       ((hex-constant-p value)
                        (parse-integer (string-right-trim "hH" value) :radix 16))
                       (t (error "Invalid time value: ~A" value))))
                    (t value))))
          (* v 1000)))
      ((string= u "min")
       `(let ((v ,(typecase value
                    (string 
                     (cond
                       ((word-addr-p value) (gen-get-word value))
                       ((numeric-p value) (parse-number:parse-number value))
                       ((hex-constant-p value)
                        (parse-integer (string-right-trim "hH" value) :radix 16))
                       (t (error "Invalid time value: ~A" value))))
                    (t value))))
          (* v 60000)))
      ((string= u "hour")
       `(let ((v ,(typecase value
                    (string 
                     (cond
                       ((word-addr-p value) (gen-get-word value))
                       ((numeric-p value) (parse-number:parse-number value))
                       ((hex-constant-p value)
                        (parse-integer (string-right-trim "hH" value) :radix 16))
                       (t (error "Invalid time value: ~A" value))))
                    (t value))))
          (* v 3600000)))
      ((string= u "day")
       `(let ((v ,(typecase value
                    (string 
                     (cond
                       ((word-addr-p value) (gen-get-word value))
                       ((numeric-p value) (parse-number:parse-number value))
                       ((hex-constant-p value)
                        (parse-integer (string-right-trim "hH" value) :radix 16))
                       (t (error "Invalid time value: ~A" value))))
                    (t value))))
          (* v 86400000)))
      (t (error "Unknown time unit: ~A" u)))))

(defun compile-tmr (params)
  "TMR - On-delay timer"
  (let ((timer-addr (first params))
        (preset (second params))
        (unit (third params)))
    `(tmr-execute data-table ,timer-addr stacktop
                  ,(parse-time-value preset unit) scan-time)))

(defun compile-tmra (params)
  "TMRA - Accumulating timer"
  (let ((timer-addr (first params))
        (preset (second params))
        (unit (third params)))
    `(tmra-execute data-table ,timer-addr stacktop
                   (second logic-stack) ; reset input
                   ,(parse-time-value preset unit) scan-time)))

(defun compile-tmroff (params)
  "TMROFF - Off-delay timer"
  (let ((timer-addr (first params))
        (preset (second params))
        (unit (third params)))
    `(tmroff-execute data-table ,timer-addr stacktop
                     ,(parse-time-value preset unit) scan-time)))

;;; ============================================================
;;; Instruction Compilation - Counters (delegates to Phase 6)
;;; ============================================================

(defun compile-cntu (params)
  "CNTU - Up counter"
  (let ((counter-addr (first params))
        (preset (second params)))
    `(cntu-execute data-table ,counter-addr
                   (second logic-stack) ; count input
                   stacktop ; reset input
                   ,(parse-value preset))))

(defun compile-cntd (params)
  "CNTD - Down counter"
  (let ((counter-addr (first params))
        (preset (second params)))
    `(cntd-execute data-table ,counter-addr
                   (second logic-stack) ; count input
                   stacktop ; reset input
                   ,(parse-value preset))))

(defun compile-udc (params)
  "UDC - Up/down counter"
  (let ((counter-addr (first params))
        (preset (second params)))
    `(udc-execute data-table ,counter-addr
                  (third logic-stack) ; up input
                  (second logic-stack) ; down input
                  stacktop ; reset input
                  ,(parse-value preset))))

;;; ============================================================
;;; Instruction Compilation - Search (delegates to Phase 6)
;;; ============================================================

(defun compile-find (find-fn params)
  "Generic find compilation"
  (let ((search-val (first params))
        (table-start (second params))
        (table-end (third params))
        (result-addr (fourth params))
        (found-bit (fifth params)))
    `(when stacktop
       (,find-fn data-table ,(parse-value search-val)
                 ,table-start ,table-end ,result-addr ,found-bit))))

(defun compile-findeq (params) (compile-find 'find-equal params))
(defun compile-findne (params) (compile-find 'find-not-equal params))
(defun compile-findgt (params) (compile-find 'find-greater params))
(defun compile-findlt (params) (compile-find 'find-less params))
(defun compile-findge (params) (compile-find 'find-greater-equal params))
(defun compile-findle (params) (compile-find 'find-less-equal params))

(defun compile-findieq (params) (compile-find 'findi-equal params))
(defun compile-findine (params) (compile-find 'findi-not-equal params))
(defun compile-findigt (params) (compile-find 'findi-greater params))
(defun compile-findilt (params) (compile-find 'findi-less params))
(defun compile-findige (params) (compile-find 'findi-greater-equal params))
(defun compile-findile (params) (compile-find 'findi-less-equal params))

;;; ============================================================
;;; Instruction Compilation - Control Flow
;;; ============================================================

(defun compile-call (params subroutines)
  "CALL - Call subroutine"
  (let ((sbr-name (first params)))
    `(when stacktop
       (let ((sbr-fn (gethash ,sbr-name ,subroutines)))
         (if sbr-fn
             (funcall sbr-fn data-table scan-time ,subroutines)
             (error "Undefined subroutine: ~A" ,sbr-name))))))

(defun compile-rt (params)
  "RT - Return from subroutine"
  (declare (ignore params))
  `(signal 'plc-return-condition :type :normal))

(defun compile-rtc (params)
  "RTC - Conditional return"
  (declare (ignore params))
  `(when stacktop
     (signal 'plc-return-condition :type :conditional)))

(defun compile-end (params)
  "END - End program"
  (declare (ignore params))
  `(signal 'plc-end-condition :type :normal))

(defun compile-endc (params)
  "ENDC - Conditional end"
  (declare (ignore params))
  `(when stacktop
     (signal 'plc-end-condition :type :conditional)))

(defun compile-for (params)
  "FOR - Begin for loop (returns marker for network-level handling)"
  (declare (ignore params))
  ;; FOR instruction is handled specially by compile-network
  ;; Returns a marker that compile-network recognizes
  :for-marker)

(defun compile-next (params)
  "NEXT - End for loop (returns marker for network-level handling)"
  (declare (ignore params))
  ;; NEXT instruction is handled specially by compile-network
  ;; Returns a marker that compile-network recognizes
  :next-marker)

;;; ============================================================
;;; Instruction Compilation - Special
;;; ============================================================

(defun compile-shfrg (params)
  "SHFRG - Shift register"
  (let ((start-addr (first params))
        (end-addr (second params)))
    ;; Three stack inputs: data, clock, reset
    `(shfrg-execute data-table ,start-addr ,end-addr
                    (third logic-stack)  ; data in
                    (second logic-stack) ; clock
                    stacktop)))          ; reset

;;; ============================================================
;;; Main Instruction Dispatcher
;;; ============================================================

(defun compile-instruction (instr subroutines)
  "Compile a single parsed instruction to Lisp code"
  (let ((opcode (parsed-opcode instr))
        (params (parsed-params instr)))
    (cond
      ;; Boolean Input
      ((string= opcode "STR") (compile-str params))
      ((string= opcode "STRN") (compile-strn params))
      ((string= opcode "AND") (compile-and params))
      ((string= opcode "ANDN") (compile-andn params))
      ((string= opcode "OR") (compile-or params))
      ((string= opcode "ORN") (compile-orn params))

      ;; Stack Operations
      ((string= opcode "ANDSTR") (compile-andstr params))
      ((string= opcode "ORSTR") (compile-orstr params))

      ;; Boolean Output
      ((string= opcode "OUT") (compile-out params))
      ((string= opcode "SET") (compile-set params))
      ((string= opcode "RST") (compile-rst params))
      ((string= opcode "PD") (compile-pd params))

      ;; Edge Detection
      ((string= opcode "STRPD") (compile-strpd params))
      ((string= opcode "STRND") (compile-strnd params))
      ((string= opcode "ANDPD") (compile-andpd params))
      ((string= opcode "ANDND") (compile-andnd params))
      ((string= opcode "ORPD") (compile-orpd params))
      ((string= opcode "ORND") (compile-ornd params))

      ;; Comparisons - Store
      ((string= opcode "STRE") (compile-stre params))
      ((string= opcode "STRNE") (compile-strne params))
      ((string= opcode "STRGT") (compile-strgt params))
      ((string= opcode "STRLT") (compile-strlt params))
      ((string= opcode "STRGE") (compile-strge params))
      ((string= opcode "STRLE") (compile-strle params))

      ;; Comparisons - AND
      ((string= opcode "ANDE") (compile-ande params))
      ((string= opcode "ANDNE") (compile-andne params))
      ((string= opcode "ANDGT") (compile-andgt params))
      ((string= opcode "ANDLT") (compile-andlt params))
      ((string= opcode "ANDGE") (compile-andge params))
      ((string= opcode "ANDLE") (compile-andle params))

      ;; Comparisons - OR
      ((string= opcode "ORE") (compile-ore params))
      ((string= opcode "ORNE") (compile-orne params))
      ((string= opcode "ORGT") (compile-orgt params))
      ((string= opcode "ORLT") (compile-orlt params))
      ((string= opcode "ORGE") (compile-orge params))
      ((string= opcode "ORLE") (compile-orle params))

      ;; Data Movement
      ((string= opcode "COPY") (compile-copy params))
      ((string= opcode "CPYBLK") (compile-cpyblk params))
      ((string= opcode "FILL") (compile-fill params))
      ((string= opcode "PACK") (compile-pack params))
      ((string= opcode "UNPACK") (compile-unpack params))

      ;; Math
      ((string= opcode "MATHDEC") (compile-mathdec params))
      ((string= opcode "MATHHEX") (compile-mathhex params))
      ((string= opcode "SUM") (compile-sum params))

      ;; Timers
      ((string= opcode "TMR") (compile-tmr params))
      ((string= opcode "TMRA") (compile-tmra params))
      ((string= opcode "TMROFF") (compile-tmroff params))

      ;; Counters
      ((string= opcode "CNTU") (compile-cntu params))
      ((string= opcode "CNTD") (compile-cntd params))
      ((string= opcode "UDC") (compile-udc params))

      ;; Search
      ((string= opcode "FINDEQ") (compile-findeq params))
      ((string= opcode "FINDNE") (compile-findne params))
      ((string= opcode "FINDGT") (compile-findgt params))
      ((string= opcode "FINDLT") (compile-findlt params))
      ((string= opcode "FINDGE") (compile-findge params))
      ((string= opcode "FINDLE") (compile-findle params))
      ((string= opcode "FINDIEQ") (compile-findieq params))
      ((string= opcode "FINDINE") (compile-findine params))
      ((string= opcode "FINDIGT") (compile-findigt params))
      ((string= opcode "FINDILT") (compile-findilt params))
      ((string= opcode "FINDIGE") (compile-findige params))
      ((string= opcode "FINDILE") (compile-findile params))

      ;; Control Flow
      ((string= opcode "CALL") (compile-call params subroutines))
      ((string= opcode "RT") (compile-rt params))
      ((string= opcode "RTC") (compile-rtc params))
      ((string= opcode "END") (compile-end params))
      ((string= opcode "ENDC") (compile-endc params))
      ((string= opcode "FOR") (compile-for params))
      ((string= opcode "NEXT") (compile-next params))

      ;; Special
      ((string= opcode "SHFRG") (compile-shfrg params))

      ;; Skip NETWORK and SBR - handled at higher level
      ((string= opcode "NETWORK") nil)
      ((string= opcode "SBR") nil)

      (t (error 'plc-compile-error
                :message (format nil "Unknown instruction: ~A" opcode)
                :instruction instr
                :line-number (parsed-line-number instr))))))

;;; ============================================================
;;; Network Compilation
;;; ============================================================

(defmethod compile-network ((compiler il-compiler) network subroutines-var)
  "Compile a parsed network to a list of Lisp forms"
  (let ((instructions (network-instructions network))
        (for-index nil)
        (next-index nil)
        (for-count-expr nil))

    ;; First pass: find FOR and NEXT positions and get FOR count
    (loop for i from 0
          for instr in instructions
          do (let ((opcode (parsed-opcode instr)))
               (when (string= opcode "FOR")
                 (setf for-index i)
                 (setf for-count-expr (parse-value (first (parsed-params instr)))))
               (when (string= opcode "NEXT")
                 (setf next-index i))))

    ;; If no FOR/NEXT, compile normally
    (if (not (and for-index next-index))
        ;; Simple case: no FOR/NEXT loop
        (let ((forms nil))
          (dolist (instr instructions)
            (let ((form (compile-instruction instr subroutines-var)))
              (when (and form (not (eq form :for-marker)) (not (eq form :next-marker)))
                (push form forms))))
          (nreverse forms))

        ;; Complex case: has FOR/NEXT loop
        (let ((pre-for-forms nil)
              (loop-body-forms nil)
              (post-next-forms nil))

          ;; Compile instructions before FOR
          (loop for i from 0 below for-index
                for instr = (nth i instructions)
                for form = (compile-instruction instr subroutines-var)
                when (and form (not (eq form :for-marker)) (not (eq form :next-marker)))
                do (push form pre-for-forms))
          (setf pre-for-forms (nreverse pre-for-forms))

          ;; Compile instructions between FOR and NEXT (the loop body)
          (loop for i from (1+ for-index) below next-index
                for instr = (nth i instructions)
                for form = (compile-instruction instr subroutines-var)
                when (and form (not (eq form :for-marker)) (not (eq form :next-marker)))
                do (push form loop-body-forms))
          (setf loop-body-forms (nreverse loop-body-forms))

          ;; Compile instructions after NEXT (rare but possible)
          (loop for i from (1+ next-index) below (length instructions)
                for instr = (nth i instructions)
                for form = (compile-instruction instr subroutines-var)
                when (and form (not (eq form :for-marker)) (not (eq form :next-marker)))
                do (push form post-next-forms))
          (setf post-next-forms (nreverse post-next-forms))

          ;; Generate the complete network with FOR/NEXT structure
          ;; Structure: pre-FOR forms, then conditional loop, then post-NEXT forms
          `(,@pre-for-forms
            (when stacktop
              (let ((for-loop-count ,for-count-expr)
                    (for-loop-limit 100000)
                    (for-loop-iterations 0))
                (when (> for-loop-count 0)
                  (tagbody
                   for-loop-start
                     (when (> for-loop-iterations for-loop-limit)
                       (error 'plc-runtime-error
                              :message "FOR/NEXT loop exceeded maximum iterations"
                              :scan 0))
                     (incf for-loop-iterations)
                     ,@loop-body-forms
                     (decf for-loop-count)
                     (when (> for-loop-count 0)
                       (go for-loop-start))))))
            ,@post-next-forms)))))

(defmethod compile-networks ((compiler il-compiler) networks subroutines-var)
  "Compile multiple networks into a single form list"
  (let ((all-forms nil))
    (dolist (net networks)
      (let ((net-forms (compile-network compiler net subroutines-var)))
        (setf all-forms (append all-forms net-forms))))
    all-forms))

;;; ============================================================
;;; Subroutine Compilation
;;; ============================================================

(defmethod compile-subroutine ((compiler il-compiler) subroutine subroutines-var)
  "Compile a subroutine to a lambda function"
  (let ((forms (compile-networks compiler
                                  (subroutine-networks subroutine)
                                  subroutines-var)))
    `(lambda (data-table scan-time subroutines)
       (declare (ignorable scan-time subroutines))
       (let ((logic-stack nil)
             (stacktop nil)
             (for-count 0))
         (declare (ignorable for-count))
         (block subroutine-block
           (handler-case
               (progn ,@forms)
             (plc-return-condition () (return-from subroutine-block))))))))

;;; ============================================================
;;; Program Compilation
;;; ============================================================

(defmethod compile-program ((compiler il-compiler) parsed-program)
  "Compile a complete parsed program"
  (let ((subroutines-table (make-hash-table :test 'equal))
        (subroutines-var 'subroutines))

    ;; First pass: compile all subroutines
    (maphash (lambda (name sbr)
               (setf (compiler-current-subroutine compiler) name)
               (let* ((sbr-form (compile-subroutine compiler sbr subroutines-var))
                      (sbr-fn (compile nil sbr-form)))
                 (setf (gethash name subroutines-table) sbr-fn)))
             (program-subroutines parsed-program))

    ;; Second pass: compile main program
    (setf (compiler-current-subroutine compiler) nil)
    (let* ((main-forms (compile-networks compiler
                                          (program-main-networks parsed-program)
                                          subroutines-var))
           (main-lambda
             `(lambda (data-table scan-time subroutines)
                (declare (ignorable scan-time subroutines))
                (let ((logic-stack nil)
                      (stacktop nil)
                      (for-count 0))
                  (declare (ignorable for-count))
                  (block main-program
                    (handler-case
                        (progn ,@main-forms)
                      (plc-end-condition () (return-from main-program))
                      (plc-return-condition ()
                        (error "RT outside subroutine")))))))
           (main-fn (compile nil main-lambda)))

      ;; Return compiled program
      (make-instance 'compiled-program
                     :main-function main-fn
                     :subroutines subroutines-table
                     :source-program parsed-program))))

;;; ============================================================
;;; Convenience Functions
;;; ============================================================

(defun compile-il-file (pathname)
  "Parse and compile an IL file"
  (let* ((parsed (parse-il-file pathname))
         (compiler (make-il-compiler)))
    (compile-program compiler parsed)))

(defun compile-il-string (source)
  "Parse and compile an IL string"
  (let* ((parsed (parse-il-string source))
         (compiler (make-il-compiler)))
    (compile-program compiler parsed)))

;;; ============================================================
;;; Debug Functions
;;; ============================================================

(defun show-generated-code (source)
  "Show the generated Lisp code for an IL source string (for debugging)"
  (let* ((parsed (parse-il-string source))
         (compiler (make-il-compiler))
         (subroutines-var 'subroutines))
    (format t "~%Generated code for main program:~%")
    (dolist (net (program-main-networks parsed))
      (format t "~%Network ~D:~%" (network-number net))
      (dolist (instr (network-instructions net))
        (let ((form (compile-instruction instr subroutines-var)))
          (format t "  ~A~{ ~A~}~%    => ~S~%"
                  (parsed-opcode instr)
                  (parsed-params instr)
                  form))))))

;;; End of compiler.lisp
