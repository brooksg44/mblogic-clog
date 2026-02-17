;;;; timer-counter.lisp
;;;;
;;;; Phase 6: Timer and Counter Library
;;;; Implements TMR, TMRA, TMROFF, CNTU, CNTD, UDC

(in-package #:mblogic-cl)

;;; ============================================================
;;; Timer State Storage Keys
;;; ============================================================
;;; Timers use the instruction table (instr-table) to store state:
;;; - "T<n>-accumulated" - accumulated time in ms
;;; - "T<n>-last-enable" - previous enable input state
;;; - "T<n>-last-reset" - previous reset input state

(declaim (inline timer-key))
(defun timer-key (timer-addr suffix)
  "Generate key for timer state storage"
  (concatenate 'string timer-addr "-" suffix))

;;; ============================================================
;;; TMR - On-Delay Timer
;;; ============================================================
;;; When enabled, accumulates time until preset is reached.
;;; When disabled or reset, accumulated time is cleared.
;;; Timer bit (T<n>) is set when accumulated >= preset.

(declaim (inline timer-data-addr))
(defun timer-data-addr (timer-addr)
  "Generate timer data address from timer bit address"
  (concatenate 'string "TD" (subseq timer-addr 1)))

(defun tmr-execute (data-table timer-addr enable preset scan-time)
  "Execute on-delay timer.
   TIMER-ADDR: Timer address (e.g., \"T1\")
   ENABLE: Enable input (stacktop)
   PRESET: Preset time in milliseconds
   SCAN-TIME: Current scan time in milliseconds"
  (let* ((instr-table (instr-table data-table))
         (acc-key (timer-key timer-addr "accumulated"))
         (accumulated (or (gethash acc-key instr-table) 0))
         (td-addr (timer-data-addr timer-addr)))

    (cond
      ;; Timer enabled - accumulate time
      (enable
       (incf accumulated scan-time)
       ;; Check if preset reached
       (if (>= accumulated preset)
           (progn
             (set-bool data-table timer-addr t)
             ;; Cap accumulated at preset to prevent overflow
             (setf accumulated preset))
           (set-bool data-table timer-addr nil)))

      ;; Timer disabled - reset
      (t
       (setf accumulated 0)
       (set-bool data-table timer-addr nil)))

    ;; Store accumulated time
    (setf (gethash acc-key instr-table) accumulated)
    ;; Store in timer data register
    (set-word data-table td-addr (truncate accumulated))

    ;; Return timer status
    (get-bool data-table timer-addr)))

;;; ============================================================
;;; TMRA - Accumulating On-Delay Timer
;;; ============================================================
;;; Like TMR but retains accumulated time when disabled.
;;; Only resets when reset input is true.

(defun tmra-execute (data-table timer-addr enable reset preset scan-time)
  "Execute accumulating on-delay timer.
   TIMER-ADDR: Timer address (e.g., \"T1\")
   ENABLE: Enable input
   RESET: Reset input
   PRESET: Preset time in milliseconds
   SCAN-TIME: Current scan time in milliseconds"
  (let* ((instr-table (instr-table data-table))
         (acc-key (timer-key timer-addr "accumulated"))
         (accumulated (or (gethash acc-key instr-table) 0))
         (td-addr (timer-data-addr timer-addr)))

    (cond
      ;; Reset takes priority
      (reset
       (setf accumulated 0)
       (set-bool data-table timer-addr nil))

      ;; Timer enabled - accumulate time
      (enable
       (incf accumulated scan-time)
       (if (>= accumulated preset)
           (progn
             (set-bool data-table timer-addr t)
             (setf accumulated preset))
           (set-bool data-table timer-addr nil)))

      ;; Timer disabled - retain accumulated time (do nothing)
      )

    ;; Store accumulated time
    (setf (gethash acc-key instr-table) accumulated)
    ;; Store in timer data register
    (set-word data-table td-addr (truncate accumulated))

    ;; Return timer status
    (get-bool data-table timer-addr)))

;;; ============================================================
;;; TMROFF - Off-Delay Timer
;;; ============================================================
;;; Output is immediately set when enabled.
;;; When disabled, timer starts counting and output stays on
;;; until preset time has elapsed.

(defun tmroff-execute (data-table timer-addr enable preset scan-time)
  "Execute off-delay timer.
   TIMER-ADDR: Timer address (e.g., \"T1\")
   ENABLE: Enable input
   PRESET: Preset time in milliseconds
   SCAN-TIME: Current scan time in milliseconds"
  (let* ((instr-table (instr-table data-table))
         (acc-key (timer-key timer-addr "accumulated"))
         (last-enable-key (timer-key timer-addr "last-enable"))
         (accumulated (or (gethash acc-key instr-table) 0))
         (last-enable (gethash last-enable-key instr-table))
         (td-addr (timer-data-addr timer-addr)))

    (cond
      ;; Timer enabled - output on, reset accumulated
      (enable
       (setf accumulated 0)
       (set-bool data-table timer-addr t))

      ;; Timer just disabled - start timing
      ((and last-enable (not enable))
       ;; Start accumulating from zero
       (setf accumulated scan-time)
       (set-bool data-table timer-addr t))

      ;; Timer disabled and timing
      ((and (not enable) (> accumulated 0))
       (incf accumulated scan-time)
       (if (>= accumulated preset)
           (progn
             (set-bool data-table timer-addr nil)
             (setf accumulated 0))
           (set-bool data-table timer-addr t)))

      ;; Timer disabled and not timing
      (t
       (set-bool data-table timer-addr nil)))

    ;; Store state
    (setf (gethash acc-key instr-table) accumulated)
    (setf (gethash last-enable-key instr-table) enable)
    ;; Store in timer data register
    (set-word data-table td-addr (truncate accumulated))

    ;; Return timer status
    (get-bool data-table timer-addr)))

;;; ============================================================
;;; Counter State Storage Keys
;;; ============================================================
;;; Counters use the instruction table to store state:
;;; - "CT<n>-accumulated" - current count
;;; - "CT<n>-last-count" - previous count input state (for edge detection)
;;; - "CT<n>-last-reset" - previous reset input state

(declaim (inline counter-key))
(defun counter-key (counter-addr suffix)
  "Generate key for counter state storage"
  (concatenate 'string counter-addr "-" suffix))

(declaim (inline counter-data-addr))
(defun counter-data-addr (counter-addr)
  "Generate counter data address from counter bit address"
  (concatenate 'string "CTD" (subseq counter-addr 2)))

;;; ============================================================
;;; CNTU - Up Counter
;;; ============================================================
;;; Counts up on rising edge of count input.
;;; Counter bit (CT<n>) is set when count >= preset.
;;; Reset clears counter to 0.

(defun cntu-execute (data-table counter-addr count-input reset preset)
  "Execute up counter.
   COUNTER-ADDR: Counter address (e.g., \"CT1\")
   COUNT-INPUT: Count input (rising edge triggers count)
   RESET: Reset input
   PRESET: Preset count value"
  (let* ((instr-table (instr-table data-table))
         (acc-key (counter-key counter-addr "accumulated"))
         (last-count-key (counter-key counter-addr "last-count"))
         (accumulated (or (gethash acc-key instr-table) 0))
         (last-count (gethash last-count-key instr-table))
         (ctd-addr (counter-data-addr counter-addr)))

    (cond
      ;; Reset takes priority
      (reset
       (setf accumulated 0)
       (set-bool data-table counter-addr nil))

      ;; Rising edge on count input - increment
      ((and count-input (not last-count))
       (incf accumulated)
       (if (>= accumulated preset)
           (set-bool data-table counter-addr t)
           (set-bool data-table counter-addr nil)))

      ;; No change - just update status based on current count
      (t
       (if (>= accumulated preset)
           (set-bool data-table counter-addr t)
           (set-bool data-table counter-addr nil))))

    ;; Store state
    (setf (gethash acc-key instr-table) accumulated)
    (setf (gethash last-count-key instr-table) count-input)
    ;; Store in counter data register
    (set-word data-table ctd-addr accumulated)

    ;; Return counter status
    (get-bool data-table counter-addr)))

;;; ============================================================
;;; CNTD - Down Counter
;;; ============================================================
;;; Counts down on rising edge of count input.
;;; Counter bit (CT<n>) is set when count <= 0.
;;; Reset loads preset value.

(defun cntd-execute (data-table counter-addr count-input reset preset)
  "Execute down counter.
   COUNTER-ADDR: Counter address (e.g., \"CT1\")
   COUNT-INPUT: Count input (rising edge triggers count)
   RESET: Reset input (loads preset)
   PRESET: Preset count value"
  (let* ((instr-table (instr-table data-table))
         (acc-key (counter-key counter-addr "accumulated"))
         (last-count-key (counter-key counter-addr "last-count"))
         (accumulated (or (gethash acc-key instr-table) preset))
         (last-count (gethash last-count-key instr-table))
         (ctd-addr (counter-data-addr counter-addr)))

    (cond
      ;; Reset loads preset
      (reset
       (setf accumulated preset)
       (set-bool data-table counter-addr nil))

      ;; Rising edge on count input - decrement
      ((and count-input (not last-count))
       (decf accumulated)
       (if (<= accumulated 0)
           (progn
             (set-bool data-table counter-addr t)
             (setf accumulated 0))  ; Prevent negative
           (set-bool data-table counter-addr nil)))

      ;; No change
      (t
       (if (<= accumulated 0)
           (set-bool data-table counter-addr t)
           (set-bool data-table counter-addr nil))))

    ;; Store state
    (setf (gethash acc-key instr-table) accumulated)
    (setf (gethash last-count-key instr-table) count-input)
    ;; Store in counter data register
    (set-word data-table ctd-addr accumulated)

    ;; Return counter status
    (get-bool data-table counter-addr)))

;;; ============================================================
;;; UDC - Up/Down Counter
;;; ============================================================
;;; Counts up on rising edge of up input.
;;; Counts down on rising edge of down input.
;;; Counter bit (CT<n>) is set when count >= preset.
;;; Reset clears counter to 0.

(defun udc-execute (data-table counter-addr up-input down-input reset preset)
  "Execute up/down counter.
   COUNTER-ADDR: Counter address (e.g., \"CT1\")
   UP-INPUT: Up count input (rising edge increments)
   DOWN-INPUT: Down count input (rising edge decrements)
   RESET: Reset input
   PRESET: Preset count value"
  (let* ((instr-table (instr-table data-table))
         (acc-key (counter-key counter-addr "accumulated"))
         (last-up-key (counter-key counter-addr "last-up"))
         (last-down-key (counter-key counter-addr "last-down"))
         (accumulated (or (gethash acc-key instr-table) 0))
         (last-up (gethash last-up-key instr-table))
         (last-down (gethash last-down-key instr-table))
         (ctd-addr (counter-data-addr counter-addr)))

    (cond
      ;; Reset takes priority
      (reset
       (setf accumulated 0)
       (set-bool data-table counter-addr nil))

      (t
       ;; Rising edge on up input - increment
       (when (and up-input (not last-up))
         (incf accumulated))
       ;; Rising edge on down input - decrement
       (when (and down-input (not last-down))
         (decf accumulated)
         (when (< accumulated 0)
           (setf accumulated 0)))
       ;; Update status
       (if (>= accumulated preset)
           (set-bool data-table counter-addr t)
           (set-bool data-table counter-addr nil))))

    ;; Store state
    (setf (gethash acc-key instr-table) accumulated)
    (setf (gethash last-up-key instr-table) up-input)
    (setf (gethash last-down-key instr-table) down-input)
    ;; Store in counter data register
    (set-word data-table ctd-addr accumulated)

    ;; Return counter status
    (get-bool data-table counter-addr)))

;;; End of timer-counter.lisp
