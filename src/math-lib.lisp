;;;; math-lib.lisp
;;;;
;;;; Phase 6: Math Library
;;;; Implements MATHDEC, MATHHEX, and supporting math operations

(in-package #:mblogic-cl)

;;; ============================================================
;;; Expression Tokenizer
;;; ============================================================

(defun tokenize-math-expr (expr)
  "Tokenize a math expression into a list of tokens"
  (let ((tokens nil)
        (current-token nil)
        (in-string nil)
        (i 0)
        (len (length expr)))
    (flet ((push-token ()
             (when current-token
               (push (coerce (nreverse current-token) 'string) tokens)
               (setf current-token nil))))
      (loop while (< i len)
            for ch = (char expr i)
            do (cond
                 ;; String literal
                 (in-string
                  (push ch current-token)
                  (when (char= ch #\")
                    (setf in-string nil)
                    (push-token)))
                 ;; Start of string
                 ((char= ch #\")
                  (push-token)
                  (setf in-string t)
                  (push ch current-token))
                 ;; Whitespace
                 ((member ch '(#\Space #\Tab #\Newline))
                  (push-token))
                 ;; Operators and punctuation (single char)
                 ((member ch '(#\+ #\- #\* #\/ #\% #\^ #\( #\) #\,))
                  (push-token)
                  (push (string ch) tokens))
                 ;; Comparison operators (may be two chars)
                 ((member ch '(#\< #\> #\= #\!))
                  (push-token)
                  (let ((next-ch (when (< (1+ i) len) (char expr (1+ i)))))
                    (if (and next-ch (char= next-ch #\=))
                        (progn
                          (push (coerce (list ch next-ch) 'string) tokens)
                          (incf i))
                        (push (string ch) tokens))))
                 ;; Everything else (identifiers, numbers)
                 (t
                  (push ch current-token)))
               (incf i))
      (push-token))
    (nreverse tokens)))

;;; ============================================================
;;; Expression Parser (Recursive Descent)
;;; ============================================================

(defvar *math-tokens* nil "Current token stream")
(defvar *math-data-table* nil "Current data table for variable lookup")
(defvar *math-hex-mode* nil "If T, use hex/bitwise mode")

(defun peek-token ()
  "Look at current token without consuming"
  (first *math-tokens*))

(defun next-token ()
  "Consume and return current token"
  (pop *math-tokens*))

(defun expect-token (expected)
  "Consume token and verify it matches expected"
  (let ((tok (next-token)))
    (unless (string= tok expected)
      (error "Expected ~A but got ~A" expected tok))))

(defun parse-primary ()
  "Parse primary expression (number, variable, function call, or parenthesized expr)"
  (let ((tok (peek-token)))
    (cond
      ;; Parenthesized expression
      ((string= tok "(")
       (next-token)
       (let ((result (parse-expression)))
         (expect-token ")")
         result))
      ;; Negative number or unary minus
      ((string= tok "-")
       (next-token)
       `(- ,(parse-primary)))
      ;; Function call or variable
      ((alpha-char-p (char tok 0))
       (next-token)
       (if (and (peek-token) (string= (peek-token) "("))
           ;; Function call
           (let ((fn-name (string-upcase tok))
                 (args nil))
             (next-token) ; consume (
             (unless (string= (peek-token) ")")
               (push (parse-expression) args)
               (loop while (and (peek-token) (string= (peek-token) ","))
                     do (next-token) ; consume ,
                        (push (parse-expression) args)))
             (expect-token ")")
             (parse-function-call fn-name (nreverse args)))
           ;; Variable reference
           (parse-variable-ref tok)))
      ;; Number literal
      ((or (digit-char-p (char tok 0))
           (char= (char tok 0) #\.))
       (next-token)
       (parse-number-literal tok))
      ;; Unknown
      (t (error "Unexpected token: ~A" tok)))))

(defun parse-number-literal (tok)
  "Parse a number literal (decimal or hex)"
  (cond
    ;; Hex number (ends with h or H)
    ((and (> (length tok) 1)
          (member (char tok (1- (length tok))) '(#\h #\H)))
     (parse-integer (subseq tok 0 (1- (length tok))) :radix 16))
    ;; Regular number
    (t (parse-number:parse-number tok))))

(defun parse-variable-ref (name)
  "Parse a variable reference, returning code to fetch value"
  (let ((upper-name (string-upcase name)))
    (cond
      ;; Boolean address
      ((bool-addr-p upper-name)
       `(if (get-bool *math-data-table* ,upper-name) 1 0))
      ;; Word address
      ((word-addr-p upper-name)
       `(get-word *math-data-table* ,upper-name))
      ;; Float address
      ((float-addr-p upper-name)
       `(get-float *math-data-table* ,upper-name))
      ;; Constants
      ((string= upper-name "PI") pi)
      ((string= upper-name "E") (exp 1.0d0))
      ;; Unknown - treat as zero
      (t (warn "Unknown variable: ~A" name) 0))))

(defun parse-function-call (name args)
  "Parse a function call"
  (cond
    ;; Trigonometric functions
    ((string= name "SIN") `(sin ,(first args)))
    ((string= name "COS") `(cos ,(first args)))
    ((string= name "TAN") `(tan ,(first args)))
    ((string= name "ASIN") `(asin ,(first args)))
    ((string= name "ACOS") `(acos ,(first args)))
    ((string= name "ATAN") `(atan ,(first args)))
    ((string= name "ATAN2") `(atan ,(first args) ,(second args)))
    ;; Hyperbolic functions
    ((string= name "SINH") `(sinh ,(first args)))
    ((string= name "COSH") `(cosh ,(first args)))
    ((string= name "TANH") `(tanh ,(first args)))
    ;; Other math functions
    ((string= name "SQRT") `(sqrt ,(first args)))
    ((string= name "ABS") `(abs ,(first args)))
    ((string= name "EXP") `(exp ,(first args)))
    ((string= name "LN") `(log ,(first args)))
    ((string= name "LOG") `(log ,(first args) 10))
    ((string= name "LOG10") `(log ,(first args) 10))
    ((string= name "POW") `(expt ,(first args) ,(second args)))
    ((string= name "CEIL") `(ceiling ,(first args)))
    ((string= name "FLOOR") `(floor ,(first args)))
    ((string= name "ROUND") `(round ,(first args)))
    ((string= name "TRUNC") `(truncate ,(first args)))
    ((string= name "MOD") `(mod ,(first args) ,(second args)))
    ((string= name "MIN") `(min ,(first args) ,(second args)))
    ((string= name "MAX") `(max ,(first args) ,(second args)))
    ;; Bitwise functions (for MATHHEX)
    ((string= name "AND") `(logand ,(first args) ,(second args)))
    ((string= name "OR") `(logior ,(first args) ,(second args)))
    ((string= name "XOR") `(logxor ,(first args) ,(second args)))
    ((string= name "NOT") `(lognot ,(first args)))
    ((string= name "LSH") `(ash ,(first args) ,(second args)))
    ((string= name "RSH") `(ash ,(first args) (- ,(second args))))
    ;; BCD functions
    ((string= name "BCDTOINT") `(bcd-to-int ,(first args)))
    ((string= name "INTTOBCD") `(int-to-bcd ,(first args)))
    ;; Unknown function
    (t (error "Unknown function: ~A" name))))

(defun parse-power ()
  "Parse power expressions (right-associative)"
  (let ((left (parse-primary)))
    (if (and (peek-token) (string= (peek-token) "^"))
        (progn
          (next-token)
          `(expt ,left ,(parse-power)))
        left)))

(defun parse-factor ()
  "Parse unary operators"
  (cond
    ((and (peek-token) (string= (peek-token) "-"))
     (next-token)
     `(- ,(parse-power)))
    ((and (peek-token) (string= (peek-token) "+"))
     (next-token)
     (parse-power))
    (t (parse-power))))

(defun parse-term ()
  "Parse multiplication, division, modulo"
  (let ((left (parse-factor)))
    (loop while (and (peek-token)
                     (member (peek-token) '("*" "/" "%") :test #'string=))
          do (let ((op (next-token)))
               (let ((right (parse-factor)))
                 (setf left
                       (cond
                         ((string= op "*") `(* ,left ,right))
                         ((string= op "/") `(/ ,left ,right))
                         ((string= op "%") `(mod ,left ,right)))))))
    left))

(defun parse-additive ()
  "Parse addition and subtraction"
  (let ((left (parse-term)))
    (loop while (and (peek-token)
                     (member (peek-token) '("+" "-") :test #'string=))
          do (let ((op (next-token)))
               (let ((right (parse-term)))
                 (setf left
                       (cond
                         ((string= op "+") `(+ ,left ,right))
                         ((string= op "-") `(- ,left ,right)))))))
    left))

(defun parse-bitwise-and ()
  "Parse bitwise AND (for hex mode)"
  (if *math-hex-mode*
      (let ((left (parse-additive)))
        (loop while (and (peek-token) (string-equal (peek-token) "AND"))
              do (next-token)
                 (let ((right (parse-additive)))
                   (setf left `(logand (truncate ,left) (truncate ,right)))))
        left)
      (parse-additive)))

(defun parse-bitwise-xor ()
  "Parse bitwise XOR (for hex mode)"
  (if *math-hex-mode*
      (let ((left (parse-bitwise-and)))
        (loop while (and (peek-token) (string-equal (peek-token) "XOR"))
              do (next-token)
                 (let ((right (parse-bitwise-and)))
                   (setf left `(logxor (truncate ,left) (truncate ,right)))))
        left)
      (parse-bitwise-and)))

(defun parse-bitwise-or ()
  "Parse bitwise OR (for hex mode)"
  (if *math-hex-mode*
      (let ((left (parse-bitwise-xor)))
        (loop while (and (peek-token) (string-equal (peek-token) "OR"))
              do (next-token)
                 (let ((right (parse-bitwise-xor)))
                   (setf left `(logior (truncate ,left) (truncate ,right)))))
        left)
      (parse-bitwise-xor)))

(defun parse-comparison ()
  "Parse comparison operators"
  (let ((left (parse-bitwise-or)))
    (when (and (peek-token)
               (member (peek-token) '("<" ">" "<=" ">=" "==" "!=" "=") :test #'string=))
      (let ((op (next-token))
            (right (parse-bitwise-or)))
        (setf left
              (cond
                ((string= op "<") `(if (< ,left ,right) 1 0))
                ((string= op ">") `(if (> ,left ,right) 1 0))
                ((string= op "<=") `(if (<= ,left ,right) 1 0))
                ((string= op ">=") `(if (>= ,left ,right) 1 0))
                ((or (string= op "==") (string= op "=")) `(if (= ,left ,right) 1 0))
                ((string= op "!=") `(if (/= ,left ,right) 1 0))))))
    left))

(defun parse-expression ()
  "Parse a complete expression"
  (parse-comparison))

;;; ============================================================
;;; BCD Conversion Functions
;;; ============================================================

(defun bcd-to-int (bcd)
  "Convert BCD encoded value to integer"
  (let ((result 0)
        (multiplier 1))
    (loop while (> bcd 0)
          do (let ((digit (logand bcd #xF)))
               (incf result (* digit multiplier))
               (setf multiplier (* multiplier 10))
               (setf bcd (ash bcd -4))))
    result))

(defun int-to-bcd (int)
  "Convert integer to BCD encoded value"
  (let ((result 0)
        (shift 0))
    (loop while (> int 0)
          do (let ((digit (mod int 10)))
               (setf result (logior result (ash digit shift)))
               (incf shift 4)
               (setf int (floor int 10))))
    result))

;;; ============================================================
;;; Main Math Functions
;;; ============================================================

(defun mathdec (data-table dest-addr flags expression)
  "Execute decimal math expression.
   DEST-ADDR: Address to store result
   FLAGS: Reserved (currently unused)
   EXPRESSION: Math expression string"
  (declare (ignore flags))
  (let* ((*math-data-table* data-table)
         (*math-hex-mode* nil)
         (*math-tokens* (tokenize-math-expr expression))
         (code (parse-expression))
         (result (eval code)))
    ;; Store result based on destination type
    (cond
      ((word-addr-p dest-addr)
       (set-word data-table dest-addr (truncate result)))
      ((float-addr-p dest-addr)
       (set-float data-table dest-addr (float result 1.0d0)))
      (t (error "MATHDEC: Invalid destination address: ~A" dest-addr)))
    result))

(defun mathhex (data-table dest-addr flags expression)
  "Execute hexadecimal/bitwise math expression.
   DEST-ADDR: Address to store result
   FLAGS: Reserved (currently unused)
   EXPRESSION: Math expression string with bitwise operations"
  (declare (ignore flags))
  (let* ((*math-data-table* data-table)
         (*math-hex-mode* t)
         (*math-tokens* (tokenize-math-expr expression))
         (code (parse-expression))
         (result (truncate (eval code))))
    ;; Store result (always integer for hex operations)
    (cond
      ((word-addr-p dest-addr)
       (set-word data-table dest-addr result))
      (t (error "MATHHEX: Invalid destination address: ~A" dest-addr)))
    result))

(defun sum-range (data-table start-addr end-addr dest-addr flags)
  "Sum values from start-addr to end-addr, store in dest-addr.
   FLAGS: Reserved"
  (declare (ignore flags))
  (let ((sum 0)
        (prefix (subseq start-addr 0 (position-if #'digit-char-p start-addr)))
        (start-idx (parse-integer (subseq start-addr
                                           (position-if #'digit-char-p start-addr))))
        (end-idx (parse-integer (subseq end-addr
                                         (position-if #'digit-char-p end-addr)))))
    (loop for i from start-idx to end-idx
          for addr = (format nil "~A~D" prefix i)
          do (incf sum (get-word data-table addr)))
    (set-word data-table dest-addr sum)
    sum))

;;; End of math-lib.lisp
