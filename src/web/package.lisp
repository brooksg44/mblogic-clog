;;;; src/web/package.lisp
;;;;
;;;; Package definition for MBLogic-CLOG web interface

(defpackage #:mblogic-clog-web
  (:use #:cl #:alexandria)
  (:export
   ;; Server management
   #:start-web-server
   #:stop-web-server
   #:web-server-running-p
   #:quick-start

   ;; PLC thread control
   #:start-plc-thread
   #:stop-plc-thread
   #:plc-thread-running-p

   ;; Configuration
   #:*plc-interpreter*
   #:*plc-thread*
   #:*static-directory*

   ;; Ladder rendering - main functions
   #:network-to-ladder
   #:program-to-ladder
   #:instruction-to-ladder-symbol
   #:ladder-program-to-js-format
   #:rung-to-js-format
   #:networks-to-ladder
   #:network-to-ladder-rung

   ;; Ladder program struct and accessors
   #:ladder-program
   #:ladder-program-name
   #:ladder-program-rungs
   #:ladder-program-addresses

   ;; Ladder rung struct and accessors
   #:ladder-rung
   #:ladder-rung-number
   #:ladder-rung-cells
   #:ladder-rung-rows
   #:ladder-rung-cols
   #:ladder-rung-addresses
   #:ladder-rung-comment

   ;; Ladder cell struct and accessors
   #:ladder-cell
   #:ladder-cell-type
   #:ladder-cell-symbol
   #:ladder-cell-address
   #:ladder-cell-addresses
   #:ladder-cell-row
   #:ladder-cell-col
   #:ladder-cell-monitor-type

   ;; SVG rendering
   #:render-rung-svg
   #:get-svg-prototypes-html
   #:*static-directory*))
