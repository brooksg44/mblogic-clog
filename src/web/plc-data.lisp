;;;; src/web/plc-data.lisp
;;;;
;;;; PLC Data Access Helpers
;;;; Functions to read PLC state for the CLOG web interface

(in-package #:mblogic-clog-web)

;;; ============================================================
;;; Address Value Access
;;; ============================================================

(defun get-address-value (interpreter address)
  "Get value of an address from interpreter's data table.
   Returns (values value type) where type is :bool, :word, :float, :string or nil."
  (let ((dt (mblogic-clog:interpreter-data-table interpreter)))
    (cond
      ((mblogic-clog:bool-addr-p address)
       (values (mblogic-clog:get-bool dt address) :bool))
      ((mblogic-clog:word-addr-p address)
       (values (mblogic-clog:get-word dt address) :word))
      ((mblogic-clog:float-addr-p address)
       (values (mblogic-clog:get-float dt address) :float))
      ((mblogic-clog:string-addr-p address)
       (values (mblogic-clog:get-string dt address) :string))
      (t (values nil nil)))))

(defun format-address-value (value type)
  "Format an address value as a display string."
  (case type
    (:bool (if value "1" "0"))
    (:word (format nil "~D" (or value 0)))
    (:float (format nil "~,2F" (or value 0.0)))
    (:string (or value ""))
    (t "")))

;;; ============================================================
;;; Statistics Data
;;; ============================================================

(defun get-statistics-data (interpreter)
  "Return a plist of current PLC statistics.
   Keys: :running :scan-count :scan-time :min-scan-time :max-scan-time :avg-scan-time"
  (if interpreter
      (let ((stats (mblogic-clog:interpreter-statistics interpreter)))
        (list :running (mblogic-clog:interpreter-running-p interpreter)
              :scan-count (mblogic-clog:interpreter-scan-count interpreter)
              :scan-time (mblogic-clog:interpreter-scan-time interpreter)
              :min-scan-time (let ((min-t (mblogic-clog:stats-min-scan-time stats)))
                               (if (= min-t most-positive-fixnum) 0 min-t))
              :max-scan-time (mblogic-clog:stats-max-scan-time stats)
              :avg-scan-time (mblogic-clog:average-scan-time stats)))
      (list :running nil :scan-count 0 :scan-time 0
            :min-scan-time 0 :max-scan-time 0 :avg-scan-time 0)))

;;; ============================================================
;;; Subroutine and Address Helpers
;;; ============================================================

(defun get-all-monitored-addresses (interpreter subrname)
  "Get list of all addresses that should be monitored for a subroutine."
  (when interpreter
    (let* ((program (mblogic-clog:interpreter-program interpreter))
           (source (when program (mblogic-clog:program-source program))))
      (when source
        (let ((ladder (program-to-ladder source (or subrname "main"))))
          (when ladder
            (ladder-program-addresses ladder)))))))

(defun get-subroutine-names (interpreter)
  "Get list of subroutine names available in the loaded program."
  (if interpreter
      (let* ((program (mblogic-clog:interpreter-program interpreter))
             (source (when program (mblogic-clog:program-source program))))
        (if source
            (list-subroutine-names source)
            '("main")))
      '("main")))

(defun get-address-values-map (interpreter addresses)
  "Get a hash table mapping address string -> value for the given addresses."
  (let ((result (make-hash-table :test #'equal)))
    (when interpreter
      (dolist (addr addresses)
        (multiple-value-bind (value type)
            (get-address-value interpreter addr)
          (setf (gethash addr result) (cons value type)))))
    result))

;;; End of plc-data.lisp
