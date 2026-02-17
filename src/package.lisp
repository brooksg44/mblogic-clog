;;;; package.lisp

(defpackage #:mblogic-cl
  (:use #:cl #:alexandria)
  (:export
   ;; Data table exports
   #:data-table
   #:make-data-table
   #:init-data-table
   #:get-bool
   #:set-bool
   #:get-word
   #:set-word
   #:get-float
   #:set-float
   #:get-string
   #:set-string
   #:bool-table
   #:word-table
   #:float-table
   #:string-table

   ;; Instruction exports
   #:plc-instruction
   #:instruction-opcode
   #:instruction-description
   #:instruction-function
   #:instruction-type
   #:instruction-class
   #:instruction-params
   #:instruction-validator
   #:instruction-min-params
   #:instruction-max-params
   #:instruction-ladsymb
   #:instruction-monitor
   #:make-instruction-set
   #:find-instruction
   #:validate-parameter
   #:execute-instruction
   #:instruction-count
   ;; Address validation
   #:bool-addr-p
   #:word-addr-p
   #:float-addr-p
   #:string-addr-p
   #:any-addr-p
   
   ;; Parser exports
   #:il-parser
   #:make-il-parser
   #:parse-program
   #:parse-line
   #:parse-il-string
   #:parse-il-file
   #:parser-errors
   #:parser-networks
   #:parser-subroutines
   ;; Parsed program accessors
   #:parsed-program
   #:program-errors
   #:program-warnings
   #:program-main-networks
   #:program-subroutines
   ;; Parsed network accessors
   #:parsed-network
   #:network-number
   #:network-instructions
   #:network-comments
   ;; Parsed instruction accessors
   #:parsed-instruction
   #:parsed-opcode
   #:parsed-params
   #:parsed-line-number
   #:parsed-instruction-def
   #:parsed-comment
   ;; Parsed subroutine accessors
   #:parsed-subroutine
   #:subroutine-name
   #:subroutine-networks
   #:subroutine-line-number
   
   ;; Compiler exports
   #:il-compiler
   #:make-il-compiler
   #:compile-program
   #:compile-network
   #:compile-instruction
   #:compiler-errors
   #:compiled-networks
   #:program-main-function
   #:program-compiled-subroutines
   #:compile-il-string

   ;; Interpreter exports
   #:plc-interpreter
   #:make-plc-interpreter
   #:run-scan
   #:run-continuous
   #:step-scan
   #:stop-interpreter
   #:interpreter-running-p
   #:interpreter-scan-time
   #:interpreter-scan-count
   #:interpreter-call-stack
   #:interpreter-statistics
   #:interpreter-exit-code
   #:interpreter-program
   #:interpreter-data-table
   #:run-il-string
   #:run-il-file
   #:test-program
   #:quick-test
   #:get-bool-value
   #:set-bool-value
   #:get-word-value
   #:set-word-value
   #:get-float-value
   #:set-float-value
   #:stats-total-scans
   #:stats-min-scan-time
   #:stats-max-scan-time
   #:average-scan-time

   ;; Compiled program exports
   #:compiled-program
   #:program-source
   #:compile-il-file
   
   ;; Conditions
   #:plc-end-condition
   #:plc-runtime-error
   #:plc-compile-error
   #:plc-parse-error
   
   ;; Timer/Counter exports
   #:plc-timer
   #:plc-counter
   #:tmr-execute
   #:tmra-execute
   #:tmroff-execute
   #:cntu-execute
   #:cntd-execute
   #:udc-execute
   
   ;; Math library exports
   #:mathdec
   #:mathhex
   #:math-expression-eval
   
   ;; Table operations exports
   #:copy-single
   #:copy-block
   #:fill-range
   #:pack-bits
   #:unpack-bits
   #:shift-register
   #:set-bits
   #:sum-table
   ;; Search operations
   #:find-equal
   #:find-not-equal
   #:find-greater
   #:find-less
   #:find-greater-equal
   #:find-less-equal
   ;; Incremental search
   #:find-equal-inc
   #:find-not-equal-inc
   #:find-greater-inc
   #:find-less-inc
   #:find-greater-equal-inc
   #:find-less-equal-inc
   #:search-table
   ;; Address utilities
   #:parse-address
   #:make-address
   #:address-prefix
   #:address-index
   #:generate-address-range
   #:address-count
   #:get-value
   #:set-value
   ;; BCD conversions
   #:bcd-to-int
   #:int-to-bcd
   #:sum-range))

;; Test package defined in test/test-suite.lisp (loaded with fiveam)
