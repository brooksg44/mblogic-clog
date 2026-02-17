;;;; interpreter.lisp
;;;;
;;;; Phase 5: Runtime Interpreter
;;;; Executes compiled programs with scan-based real-time behavior

(in-package #:mblogic-cl)

;;; ============================================================
;;; Runtime Error Condition
;;; ============================================================

(define-condition plc-runtime-error (error)
  ((message :initarg :message :reader runtime-error-message)
   (subroutine :initarg :subroutine :reader runtime-error-subroutine :initform nil)
   (network :initarg :network :reader runtime-error-network :initform nil)
   (scan-number :initarg :scan :reader runtime-error-scan :initform nil))
  (:report (lambda (c stream)
             (format stream "PLC Runtime error~@[ in ~A~]~@[ network ~D~]: ~A"
                     (runtime-error-subroutine c)
                     (runtime-error-network c)
                     (runtime-error-message c)))))

;;; ============================================================
;;; Call Frame for Stack Tracking
;;; ============================================================

(defclass call-frame ()
  ((subroutine-name :initarg :name
                    :accessor frame-subroutine-name
                    :documentation "Name of called subroutine")
   (timestamp :initarg :timestamp
              :accessor frame-timestamp
              :initform (get-internal-real-time)
              :documentation "Time when call was made"))
  (:documentation "Call stack frame for subroutine tracking"))

(defmethod print-object ((frame call-frame) stream)
  (print-unreadable-object (frame stream :type t)
    (format stream "~A" (frame-subroutine-name frame))))

;;; ============================================================
;;; Scan Statistics
;;; ============================================================

(defclass scan-statistics ()
  ((total-scans :initform 0
                :accessor stats-total-scans
                :documentation "Total number of scans executed")
   (total-time :initform 0
               :accessor stats-total-time
               :documentation "Total execution time in internal units")
   (min-scan-time :initform most-positive-fixnum
                  :accessor stats-min-scan-time
                  :documentation "Minimum scan time")
   (max-scan-time :initform 0
                  :accessor stats-max-scan-time
                  :documentation "Maximum scan time")
   (last-scan-time :initform 0
                   :accessor stats-last-scan-time
                   :documentation "Most recent scan time")
   (errors :initform 0
           :accessor stats-errors
           :documentation "Number of runtime errors"))
  (:documentation "Statistics for PLC scan execution"))

(defun make-scan-statistics ()
  "Create new scan statistics object"
  (make-instance 'scan-statistics))

(defmethod update-statistics ((stats scan-statistics) scan-time-ms)
  "Update statistics with new scan time"
  (incf (stats-total-scans stats))
  (incf (stats-total-time stats) scan-time-ms)
  (setf (stats-last-scan-time stats) scan-time-ms)
  (when (< scan-time-ms (stats-min-scan-time stats))
    (setf (stats-min-scan-time stats) scan-time-ms))
  (when (> scan-time-ms (stats-max-scan-time stats))
    (setf (stats-max-scan-time stats) scan-time-ms)))

(defmethod average-scan-time ((stats scan-statistics))
  "Calculate average scan time in milliseconds"
  (if (> (stats-total-scans stats) 0)
      (/ (stats-total-time stats) (stats-total-scans stats))
      0))

;;; ============================================================
;;; PLC Interpreter Class
;;; ============================================================

(defclass plc-interpreter ()
  ((compiled-program :initarg :program
                     :accessor interpreter-program
                     :documentation "The compiled program to execute")
   (data-table :initarg :data-table
               :accessor interpreter-data-table
               :documentation "PLC data table")
   (running :initform nil
            :accessor interpreter-running
            :documentation "Whether interpreter is currently running")
   (scan-count :initform 0
               :accessor interpreter-scan-count
               :documentation "Current scan number")
   (scan-time :initform 0
              :accessor interpreter-scan-time
              :documentation "Last scan time in milliseconds")
   (exit-code :initform nil
              :accessor interpreter-exit-code
              :documentation "Exit code from last run")
   (call-stack :initform nil
               :accessor interpreter-call-stack
               :documentation "Current call stack for debugging")
   (statistics :initform (make-scan-statistics)
               :accessor interpreter-statistics
               :documentation "Scan statistics")
   (first-scan :initform t
               :accessor interpreter-first-scan
               :documentation "Flag for first scan after start")
   (last-second-time :initform 0
                     :accessor interpreter-last-second-time
                     :documentation "Time of last one-second pulse")
   (one-second-pulse :initform nil
                     :accessor interpreter-one-second-pulse
                     :documentation "One-second clock pulse state")
   (error-handler :initarg :error-handler
                  :accessor interpreter-error-handler
                  :initform nil
                  :documentation "Custom error handler function"))
  (:documentation "PLC program interpreter with scan-based execution"))

(defun make-plc-interpreter (&key program data-table error-handler)
  "Create a new PLC interpreter"
  (unless program
    (error "Program required for interpreter"))
  (let ((dt (or data-table (make-data-table))))
    (unless data-table
      (init-data-table dt))
    (make-instance 'plc-interpreter
                   :program program
                   :data-table dt
                   :error-handler error-handler)))

(defmethod interpreter-running-p ((interp plc-interpreter))
  "Check if interpreter is currently running"
  (interpreter-running interp))

;;; ============================================================
;;; System Control Bits
;;; ============================================================

(defmethod update-system-bits ((interp plc-interpreter))
  "Update system control bits before each scan"
  (let ((dt (interpreter-data-table interp))
        (current-time (get-internal-real-time)))

    ;; SC1 - Always ON
    (set-bool dt "SC1" t)

    ;; SC2 - Always OFF
    (set-bool dt "SC2" nil)

    ;; SC3 - Alternating bit (toggles each scan)
    (set-bool dt "SC3" (oddp (interpreter-scan-count interp)))

    ;; SC4 - Running status
    (set-bool dt "SC4" (interpreter-running interp))

    ;; SC5 - First scan flag
    (set-bool dt "SC5" (interpreter-first-scan interp))

    ;; SC6 - One-second clock (pulse on one-second boundaries)
    (let* ((elapsed-ms (/ (- current-time (interpreter-last-second-time interp))
                          (/ internal-time-units-per-second 1000))))
      (if (>= elapsed-ms 1000)
          (progn
            (setf (interpreter-last-second-time interp) current-time)
            (setf (interpreter-one-second-pulse interp) t)
            (set-bool dt "SC6" t))
          (progn
            (setf (interpreter-one-second-pulse interp) nil)
            (set-bool dt "SC6" nil))))

    ;; SC7 - Scan complete (set at end of scan, cleared at start)
    (set-bool dt "SC7" nil)

    ;; SD1 - Scan counter (low word)
    (set-word dt "SD1" (mod (interpreter-scan-count interp) 65536))

    ;; SD2 - Scan time (last scan in ms)
    (set-word dt "SD2" (round (interpreter-scan-time interp)))

    ;; SD3 - Average scan time
    (set-word dt "SD3" (round (average-scan-time (interpreter-statistics interp))))

    ;; Clear first scan flag after first scan
    (when (interpreter-first-scan interp)
      (setf (interpreter-first-scan interp) nil))))

(defmethod finalize-system-bits ((interp plc-interpreter))
  "Update system bits at end of scan"
  (let ((dt (interpreter-data-table interp)))
    ;; SC7 - Scan complete
    (set-bool dt "SC7" t)))

;;; ============================================================
;;; Error Handling
;;; ============================================================

(defmethod handle-runtime-error ((interp plc-interpreter) error)
  "Handle a runtime error during execution"
  ;; Increment error count
  (incf (stats-errors (interpreter-statistics interp)))

  ;; Call custom handler if provided
  (when (interpreter-error-handler interp)
    (funcall (interpreter-error-handler interp) interp error))

  ;; Re-signal as PLC runtime error with context
  (error 'plc-runtime-error
         :message (format nil "~A" error)
         :scan (interpreter-scan-count interp)))

;;; ============================================================
;;; Scan Execution
;;; ============================================================

(defmethod run-scan ((interp plc-interpreter))
  "Execute one complete PLC scan cycle"
  (let ((start-time (get-internal-real-time))
        (program (interpreter-program interp))
        (dt (interpreter-data-table interp)))

    ;; Update system bits at start of scan
    (update-system-bits interp)

    ;; Execute the main program
    (handler-case
        (funcall (program-main-function program)
                 dt
                 (interpreter-scan-time interp)
                 (program-compiled-subroutines program))

      ;; Normal end condition
      (plc-end-condition (c)
        (declare (ignore c))
        (setf (interpreter-exit-code interp) :normal-end))

      ;; Runtime errors
      (error (e)
        (handle-runtime-error interp e)))

    ;; Calculate scan time
    (let* ((end-time (get-internal-real-time))
           (elapsed (- end-time start-time))
           (scan-time-ms (* 1000.0 (/ elapsed internal-time-units-per-second))))

      ;; Update scan time and statistics
      (setf (interpreter-scan-time interp) scan-time-ms)
      (update-statistics (interpreter-statistics interp) scan-time-ms)

      ;; Increment scan counter
      (incf (interpreter-scan-count interp))

      ;; Finalize system bits
      (finalize-system-bits interp)

      ;; Return scan time
      scan-time-ms)))

;;; ============================================================
;;; Continuous Execution
;;; ============================================================

(defmethod run-continuous ((interp plc-interpreter) &key (max-scans nil) (target-scan-time nil))
  "Run PLC in continuous scan mode.
   MAX-SCANS: Stop after this many scans (nil = run forever)
   TARGET-SCAN-TIME: Target scan time in ms (nil = run as fast as possible)"

  (setf (interpreter-running interp) t)
  (setf (interpreter-first-scan interp) t)
  (setf (interpreter-scan-count interp) 0)
  (setf (interpreter-exit-code interp) nil)
  (setf (interpreter-last-second-time interp) (get-internal-real-time))

  (unwind-protect
       (loop while (interpreter-running interp)
             for scan-num from 0
             do
                ;; Execute one scan
                (let ((scan-time (run-scan interp)))

                  ;; Check for max scans
                  (when (and max-scans (>= scan-num (1- max-scans)))
                    (setf (interpreter-running interp) nil)
                    (setf (interpreter-exit-code interp) :max-scans-reached))

                  ;; Check for exit code from END instruction
                  (when (interpreter-exit-code interp)
                    (setf (interpreter-running interp) nil))

                  ;; Sleep to maintain target scan time if specified
                  (when (and target-scan-time (< scan-time target-scan-time))
                    (sleep (/ (- target-scan-time scan-time) 1000.0)))))

    ;; Cleanup
    (setf (interpreter-running interp) nil))

  ;; Return exit code
  (interpreter-exit-code interp))

(defmethod stop-interpreter ((interp plc-interpreter))
  "Stop a running interpreter"
  (setf (interpreter-running interp) nil)
  (setf (interpreter-exit-code interp) :stopped))

;;; ============================================================
;;; Single-Step Execution
;;; ============================================================

(defmethod step-scan ((interp plc-interpreter))
  "Execute a single scan (for debugging)"
  (when (zerop (interpreter-scan-count interp))
    (setf (interpreter-first-scan interp) t)
    (setf (interpreter-last-second-time interp) (get-internal-real-time)))
  (run-scan interp))

;;; ============================================================
;;; State Inspection
;;; ============================================================

(defmethod print-interpreter-status ((interp plc-interpreter) &optional (stream t))
  "Print current interpreter status"
  (format stream "~%=== PLC Interpreter Status ===~%")
  (format stream "Running: ~A~%" (interpreter-running interp))
  (format stream "Scan count: ~D~%" (interpreter-scan-count interp))
  (format stream "Last scan time: ~,3F ms~%" (interpreter-scan-time interp))
  (format stream "Exit code: ~A~%" (interpreter-exit-code interp))

  (let ((stats (interpreter-statistics interp)))
    (format stream "~%Statistics:~%")
    (format stream "  Total scans: ~D~%" (stats-total-scans stats))
    (format stream "  Average scan time: ~,3F ms~%" (average-scan-time stats))
    (format stream "  Min scan time: ~,3F ms~%"
            (if (= (stats-min-scan-time stats) most-positive-fixnum)
                0.0
                (stats-min-scan-time stats)))
    (format stream "  Max scan time: ~,3F ms~%" (stats-max-scan-time stats))
    (format stream "  Errors: ~D~%" (stats-errors stats))))

(defmethod get-bool-value ((interp plc-interpreter) address)
  "Get boolean value from interpreter's data table"
  (get-bool (interpreter-data-table interp) address))

(defmethod get-word-value ((interp plc-interpreter) address)
  "Get word value from interpreter's data table"
  (get-word (interpreter-data-table interp) address))

(defmethod set-bool-value ((interp plc-interpreter) address value)
  "Set boolean value in interpreter's data table"
  (set-bool (interpreter-data-table interp) address value))

(defmethod set-word-value ((interp plc-interpreter) address value)
  "Set word value in interpreter's data table"
  (set-word (interpreter-data-table interp) address value))

(defmethod get-float-value ((interp plc-interpreter) address)
  "Get float value from interpreter's data table"
  (get-float (interpreter-data-table interp) address))

(defmethod set-float-value ((interp plc-interpreter) address value)
  "Set float value in interpreter's data table"
  (set-float (interpreter-data-table interp) address value))

;;; ============================================================
;;; Convenience Functions
;;; ============================================================

(defun run-il-file (pathname &key (max-scans 1) data-table)
  "Parse, compile, and run an IL file"
  (let* ((compiled (compile-il-file pathname))
         (interp (make-plc-interpreter :program compiled
                                        :data-table data-table)))
    (run-continuous interp :max-scans max-scans)
    interp))

(defun run-il-string (source &key (max-scans 1) data-table)
  "Parse, compile, and run an IL string"
  (let* ((compiled (compile-il-string source))
         (interp (make-plc-interpreter :program compiled
                                        :data-table data-table)))
    (run-continuous interp :max-scans max-scans)
    interp))

;;; ============================================================
;;; Debug/Test Utilities
;;; ============================================================

(defun test-program (source &key (scans 1) inputs)
  "Test an IL program with optional inputs.
   Returns the interpreter after execution."
  (let* ((compiled (compile-il-string source))
         (interp (make-plc-interpreter :program compiled)))

    ;; Set input values
    (when inputs
      (loop for (addr . value) in inputs
            do (cond
                 ((bool-addr-p addr)
                  (set-bool-value interp addr value))
                 ((word-addr-p addr)
                  (set-word-value interp addr value))
                 ((float-addr-p addr)
                  (set-float (interpreter-data-table interp) addr value))
                 ((string-addr-p addr)
                  (set-string (interpreter-data-table interp) addr value)))))

    ;; Run the program
    (run-continuous interp :max-scans scans)

    ;; Return interpreter for inspection
    interp))

(defun quick-test (source inputs expected-outputs)
  "Quick test: run program and check expected outputs.
   INPUTS: alist of (address . value)
   EXPECTED-OUTPUTS: alist of (address . expected-value)
   Returns T if all outputs match, NIL otherwise."
  (let ((interp (test-program source :scans 1 :inputs inputs)))
    (every (lambda (expectation)
             (let* ((addr (car expectation))
                    (expected (cdr expectation))
                    (actual (cond
                              ((bool-addr-p addr) (get-bool-value interp addr))
                              ((word-addr-p addr) (get-word-value interp addr))
                              (t nil))))
               (eql actual expected)))
           expected-outputs)))

;;; End of interpreter.lisp
