;;;; table-ops.lisp
;;;;
;;;; Phase 6: Table Operations Library
;;;; Implements COPY, CPYBLK, FILL, PACK, UNPACK, SHFRG, FIND* operations

(in-package #:mblogic-cl)

;;; ============================================================
;;; Address Parsing and Range Utilities
;;; ============================================================

(defun parse-address (addr)
  "Parse address string into (prefix . index).
   Example: \"DS100\" -> (\"DS\" . 100)"
  (let ((pos (position-if #'digit-char-p addr)))
    (if pos
        (cons (subseq addr 0 pos)
              (parse-integer (subseq addr pos)))
        (cons addr 0))))

(defun make-address (prefix index)
  "Create address string from prefix and index.
   Example: (\"DS\" 100) -> \"DS100\""
  (format nil "~A~D" prefix index))

(defun address-prefix (addr)
  "Extract prefix from address string"
  (car (parse-address addr)))

(defun address-index (addr)
  "Extract numeric index from address string"
  (cdr (parse-address addr)))

(defun generate-address-range (start-addr end-addr)
  "Generate list of addresses from START-ADDR to END-ADDR.
   Both addresses must have the same prefix."
  (let* ((start-parsed (parse-address start-addr))
         (end-parsed (parse-address end-addr))
         (prefix (car start-parsed))
         (start-idx (cdr start-parsed))
         (end-idx (cdr end-parsed)))
    (unless (string= prefix (car end-parsed))
      (error "Address prefix mismatch: ~A vs ~A" start-addr end-addr))
    (if (<= start-idx end-idx)
        (loop for i from start-idx to end-idx
              collect (make-address prefix i))
        ;; Reverse order
        (loop for i from start-idx downto end-idx
              collect (make-address prefix i)))))

(defun address-count (start-addr end-addr)
  "Return the number of addresses in range"
  (let* ((start-parsed (parse-address start-addr))
         (end-parsed (parse-address end-addr)))
    (1+ (abs (- (cdr end-parsed) (cdr start-parsed))))))

;;; ============================================================
;;; Value Access Helpers
;;; ============================================================

(defun get-value (data-table addr)
  "Get value at address, auto-detecting table type"
  (cond
    ((bool-addr-p addr) (get-bool data-table addr))
    ((word-addr-p addr) (get-word data-table addr))
    ((float-addr-p addr) (get-float data-table addr))
    ((string-addr-p addr) (get-string data-table addr))
    (t (error "Unknown address type: ~A" addr))))

(defun set-value (data-table addr value)
  "Set value at address, auto-detecting table type"
  (cond
    ((bool-addr-p addr) (set-bool data-table addr value))
    ((word-addr-p addr) (set-word data-table addr value))
    ((float-addr-p addr) (set-float data-table addr value))
    ((string-addr-p addr) (set-string data-table addr value))
    (t (error "Unknown address type: ~A" addr))))

;;; ============================================================
;;; COPY - Copy Single Value
;;; ============================================================

(defun copy-single (data-table source dest)
  "Copy single value from SOURCE to DEST.
   SOURCE can be a constant or address.
   DEST must be an address.
   Sets SC43 if data conversion error, SC44 if address error."
  ;; Reset error flags
  (set-bool data-table "SC43" nil)
  (set-bool data-table "SC44" nil)

  ;; Get source value
  (let ((value (cond
                 ;; Source is a number
                 ((numberp source) source)
                 ;; Source is a string constant (quoted)
                 ((and (stringp source)
                       (not (any-addr-p source)))
                  source)
                 ;; Source is an address
                 ((any-addr-p source)
                  (get-value data-table source))
                 (t
                  (set-bool data-table "SC44" t)
                  (return-from copy-single nil)))))

    ;; Validate destination
    (unless (any-addr-p dest)
      (set-bool data-table "SC44" t)
      (return-from copy-single nil))

    ;; Convert and store value based on destination type
    (handler-case
        (cond
          ;; Boolean destination (0 = NIL, non-zero = T)
          ((bool-addr-p dest)
           (set-bool data-table dest
                     (cond
                       ((null value) nil)
                       ((and (numberp value) (zerop value)) nil)
                       (t t))))
          ;; Word destination (integer)
          ((word-addr-p dest)
           (set-word data-table dest (truncate value)))
          ;; Float destination
          ((float-addr-p dest)
           (set-float data-table dest (float value 1.0d0)))
          ;; String destination
          ((string-addr-p dest)
           (set-string data-table dest (princ-to-string value))))
      (error (e)
        (declare (ignore e))
        (set-bool data-table "SC43" t)
        nil))))

;;; ============================================================
;;; FILL - Fill Range with Value
;;; ============================================================

(defun fill-range (data-table value dest-start dest-end)
  "Fill range of addresses with VALUE.
   Sets SC43 if data conversion error, SC44 if address error."
  ;; Reset error flags
  (set-bool data-table "SC43" nil)
  (set-bool data-table "SC44" nil)

  ;; Generate address range
  (handler-case
      (let ((addrs (generate-address-range dest-start dest-end)))
        ;; Fill each address with value
        (dolist (addr addrs)
          (cond
            ((word-addr-p addr)
             (set-word data-table addr (truncate value)))
            ((float-addr-p addr)
             (set-float data-table addr (float value 1.0d0)))
            ((bool-addr-p addr)
             (set-bool data-table addr
                       (cond
                         ((null value) nil)
                         ((and (numberp value) (zerop value)) nil)
                         (t t))))
            ((string-addr-p addr)
             (set-string data-table addr (princ-to-string value))))))
    (error (e)
      (declare (ignore e))
      (set-bool data-table "SC44" t)
      nil)))

;;; ============================================================
;;; CPYBLK - Copy Block of Values
;;; ============================================================

(defun copy-block (data-table source-start source-end dest-start)
  "Copy block of values from source range to destination.
   Sets SC43 if data conversion error, SC44 if address error."
  ;; Reset error flags
  (set-bool data-table "SC43" nil)
  (set-bool data-table "SC44" nil)

  (handler-case
      (let* ((source-addrs (generate-address-range source-start source-end))
             (count (length source-addrs))
             (dest-parsed (parse-address dest-start))
             (dest-prefix (car dest-parsed))
             (dest-start-idx (cdr dest-parsed))
             ;; Read source values first to handle overlapping ranges
             (values (mapcar (lambda (addr) (get-value data-table addr))
                            source-addrs)))

        ;; Write to destination addresses
        (loop for i from 0 below count
              for dest-idx = (+ dest-start-idx i)
              for dest-addr = (make-address dest-prefix dest-idx)
              for value in values
              do (cond
                   ((word-addr-p dest-addr)
                    (set-word data-table dest-addr
                              (if (numberp value) (truncate value) 0)))
                   ((float-addr-p dest-addr)
                    (set-float data-table dest-addr
                               (if (numberp value) (float value 1.0d0) 0.0d0)))
                   ((bool-addr-p dest-addr)
                    (set-bool data-table dest-addr
                              (cond
                                ((null value) nil)
                                ((and (numberp value) (zerop value)) nil)
                                (t t))))
                   ((string-addr-p dest-addr)
                    (set-string data-table dest-addr
                                (if (stringp value) value (princ-to-string value)))))))
    (error (e)
      (declare (ignore e))
      (set-bool data-table "SC44" t)
      nil)))

;;; ============================================================
;;; PACK - Pack Boolean Range into Word
;;; ============================================================

(defun pack-bits (data-table source-start source-end dest)
  "Pack boolean range into a word register.
   Bits are packed LSB first (source-start = bit 0)."
  (let* ((source-addrs (generate-address-range source-start source-end))
         (value 0))
    ;; Pack bits - first address is LSB
    (loop for addr in source-addrs
          for bit from 0
          do (when (get-bool data-table addr)
               (setf value (logior value (ash 1 bit)))))
    ;; Store result
    (set-word data-table dest value)
    value))

;;; ============================================================
;;; UNPACK - Unpack Word into Boolean Range
;;; ============================================================

(defun unpack-bits (data-table source dest-start dest-end)
  "Unpack word register into boolean range.
   Bits are unpacked LSB first (dest-start = bit 0)."
  (let* ((value (get-word data-table source))
         (dest-addrs (generate-address-range dest-start dest-end)))
    ;; Unpack bits - first address is LSB
    (loop for addr in dest-addrs
          for bit from 0
          do (set-bool data-table addr
                       (logbitp bit value)))
    value))

;;; ============================================================
;;; SHFRG - Shift Register
;;; ============================================================

(defun shift-register (data-table data-in clock last-clock reset start-addr end-addr)
  "Shift register operation.
   DATA-IN: Data bit to shift in
   CLOCK: Clock input (shift on rising edge)
   LAST-CLOCK: Previous clock state (for edge detection)
   RESET: Reset all bits to false
   START-ADDR, END-ADDR: Boolean address range for shift register"
  ;; Update start address with data input
  (set-bool data-table start-addr data-in)

  ;; If no clock edge and no reset, nothing more to do
  (unless (or (and clock (not last-clock)) reset)
    (return-from shift-register nil))

  ;; Get address sequence
  (let* ((addrs (generate-address-range start-addr end-addr)))

    ;; Handle reset
    (when reset
      (dolist (addr addrs)
        (set-bool data-table addr nil))
      (return-from shift-register nil))

    ;; Handle shift on rising edge
    (when (and clock (not last-clock))
      (let ((new-value data-in))
        (dolist (addr addrs)
          (let ((previous (get-bool data-table addr)))
            (set-bool data-table addr new-value)
            (setf new-value previous)))))))

;;; ============================================================
;;; FIND* - Search Operations
;;; ============================================================

(defun search-compare (compare-type value1 value2)
  "Compare two values according to COMPARE-TYPE.
   Returns T if comparison succeeds."
  (case compare-type
    (:eq (equal value1 value2))
    (:ne (not (equal value1 value2)))
    (:gt (> value1 value2))
    (:ge (>= value1 value2))
    (:lt (< value1 value2))
    (:le (<= value1 value2))
    (otherwise nil)))

(defun search-table (data-table search-value search-start search-end
                     result-addr result-flag compare-type &optional (incremental nil))
  "Search a table for a value matching the comparison.
   SEARCH-VALUE: Value to search for
   SEARCH-START, SEARCH-END: Range to search
   RESULT-ADDR: Word address to store result index (1-based, -1 if not found)
   RESULT-FLAG: Boolean address to store found/not-found
   COMPARE-TYPE: :eq :ne :gt :ge :lt :le
   INCREMENTAL: If true, continue from last result position"
  (let* ((addrs (generate-address-range search-start search-end))
         (last-result (if incremental
                         (get-word data-table result-addr)
                         0))
         (start-offset (if (and incremental (> last-result 0))
                          last-result  ; Continue after last result
                          0))
         (found nil)
         (found-index -1))

    ;; Perform search
    (loop for addr in (nthcdr start-offset addrs)
          for i from start-offset
          do (let ((table-value (get-value data-table addr)))
               (when (search-compare compare-type table-value search-value)
                 (setf found t)
                 (setf found-index (1+ i))  ; 1-based index
                 (return))))

    ;; Store results
    (set-bool data-table result-flag found)
    (set-word data-table result-addr found-index)

    found))

;; Convenience functions for each search type
(defun find-equal (data-table search-value search-start search-end result-addr result-flag)
  "Search for equal value"
  (search-table data-table search-value search-start search-end
                result-addr result-flag :eq nil))

(defun find-not-equal (data-table search-value search-start search-end result-addr result-flag)
  "Search for not equal value"
  (search-table data-table search-value search-start search-end
                result-addr result-flag :ne nil))

(defun find-greater (data-table search-value search-start search-end result-addr result-flag)
  "Search for greater than value"
  (search-table data-table search-value search-start search-end
                result-addr result-flag :gt nil))

(defun find-less (data-table search-value search-start search-end result-addr result-flag)
  "Search for less than value"
  (search-table data-table search-value search-start search-end
                result-addr result-flag :lt nil))

(defun find-greater-equal (data-table search-value search-start search-end result-addr result-flag)
  "Search for greater or equal value"
  (search-table data-table search-value search-start search-end
                result-addr result-flag :ge nil))

(defun find-less-equal (data-table search-value search-start search-end result-addr result-flag)
  "Search for less or equal value"
  (search-table data-table search-value search-start search-end
                result-addr result-flag :le nil))

;; Incremental search variants
(defun find-equal-inc (data-table search-value search-start search-end result-addr result-flag)
  "Incremental search for equal value"
  (search-table data-table search-value search-start search-end
                result-addr result-flag :eq t))

(defun find-not-equal-inc (data-table search-value search-start search-end result-addr result-flag)
  "Incremental search for not equal value"
  (search-table data-table search-value search-start search-end
                result-addr result-flag :ne t))

(defun find-greater-inc (data-table search-value search-start search-end result-addr result-flag)
  "Incremental search for greater than value"
  (search-table data-table search-value search-start search-end
                result-addr result-flag :gt t))

(defun find-less-inc (data-table search-value search-start search-end result-addr result-flag)
  "Incremental search for less than value"
  (search-table data-table search-value search-start search-end
                result-addr result-flag :lt t))

(defun find-greater-equal-inc (data-table search-value search-start search-end result-addr result-flag)
  "Incremental search for greater or equal value"
  (search-table data-table search-value search-start search-end
                result-addr result-flag :ge t))

(defun find-less-equal-inc (data-table search-value search-start search-end result-addr result-flag)
  "Incremental search for less or equal value"
  (search-table data-table search-value search-start search-end
                result-addr result-flag :le t))

;; Compiler expects these names (aliases for incremental variants)
(defun findi-equal (data-table search-value search-start search-end result-addr result-flag)
  "Alias for find-equal-inc (compiler generated)"
  (find-equal-inc data-table search-value search-start search-end result-addr result-flag))

(defun findi-not-equal (data-table search-value search-start search-end result-addr result-flag)
  "Alias for find-not-equal-inc (compiler generated)"
  (find-not-equal-inc data-table search-value search-start search-end result-addr result-flag))

(defun findi-greater (data-table search-value search-start search-end result-addr result-flag)
  "Alias for find-greater-inc (compiler generated)"
  (find-greater-inc data-table search-value search-start search-end result-addr result-flag))

(defun findi-less (data-table search-value search-start search-end result-addr result-flag)
  "Alias for find-less-inc (compiler generated)"
  (find-less-inc data-table search-value search-start search-end result-addr result-flag))

(defun findi-greater-equal (data-table search-value search-start search-end result-addr result-flag)
  "Alias for find-greater-equal-inc (compiler generated)"
  (find-greater-equal-inc data-table search-value search-start search-end result-addr result-flag))

(defun findi-less-equal (data-table search-value search-start search-end result-addr result-flag)
  "Alias for find-less-equal-inc (compiler generated)"
  (find-less-equal-inc data-table search-value search-start search-end result-addr result-flag))

;;; ============================================================
;;; SHFRG - Execute (Compiler Interface)
;;; ============================================================

(defun shfrg-execute (data-table start-addr end-addr reset clock data-in)
  "Shift register execute for compiler.
   Note: different parameter order than shift-register for compatibility."
  (shift-register data-table data-in clock nil reset start-addr end-addr))

;;; ============================================================
;;; SUM - Sum Range of Values
;;; ============================================================
;;; Note: Already implemented in math-lib.lisp as sum-range
;;; This is an alias for consistency

(defun sum-table (data-table source-start source-end dest)
  "Sum values from source range and store in dest.
   Alias for sum-range in math-lib.lisp"
  (sum-range data-table source-start source-end dest nil))

;;; ============================================================
;;; SETBITS - Set Range of Boolean Addresses
;;; ============================================================

(defun set-bits (data-table value start-addr end-addr)
  "Set a range of boolean addresses to VALUE"
  (let ((addrs (generate-address-range start-addr end-addr)))
    (dolist (addr addrs)
      (set-bool data-table addr value))))

;;; End of table-ops.lisp
