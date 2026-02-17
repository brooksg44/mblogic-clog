;;;; parser.lisp
;;;;
;;;; Phase 3: IL Parser
;;;; Parses text IL programs into structured instruction lists

(in-package #:mblogic-cl)

;;; ============================================================
;;; Parse Error Condition
;;; ============================================================

(define-condition plc-parse-error (error)
  ((message :initarg :message :reader parse-error-message)
   (line-number :initarg :line-number :reader parse-error-line :initform nil)
   (line-text :initarg :line-text :reader parse-error-text :initform nil))
  (:report (lambda (c stream)
             (format stream "Parse error~@[ at line ~D~]: ~A~@[~%  Line: ~A~]"
                     (parse-error-line c)
                     (parse-error-message c)
                     (parse-error-text c)))))

;;; ============================================================
;;; Parsed Instruction Class
;;; ============================================================

(defclass parsed-instruction ()
  ((opcode :initarg :opcode
           :accessor parsed-opcode
           :type string
           :documentation "Instruction opcode")
   (params :initarg :params
           :accessor parsed-params
           :initform nil
           :documentation "List of parameter strings")
   (line-number :initarg :line-number
                :accessor parsed-line-number
                :initform 0
                :documentation "Source line number")
   (instruction-def :initarg :instruction-def
                    :accessor parsed-instruction-def
                    :initform nil
                    :documentation "Reference to instruction definition")
   (comment :initarg :comment
            :accessor parsed-comment
            :initform nil
            :documentation "Associated comment if any"))
  (:documentation "A parsed IL instruction with source info"))

(defmethod print-object ((pinstr parsed-instruction) stream)
  (print-unreadable-object (pinstr stream :type t)
    (format stream "~A~{ ~A~} @~D"
            (parsed-opcode pinstr)
            (parsed-params pinstr)
            (parsed-line-number pinstr))))

;;; ============================================================
;;; Network and Subroutine Structures
;;; ============================================================

(defclass parsed-network ()
  ((number :initarg :number
           :accessor network-number
           :documentation "Network number")
   (instructions :initarg :instructions
                 :accessor network-instructions
                 :initform nil
                 :documentation "List of parsed instructions")
   (comments :initarg :comments
             :accessor network-comments
             :initform nil
             :documentation "Comments preceding the network"))
  (:documentation "A parsed network/rung"))

(defmethod print-object ((net parsed-network) stream)
  (print-unreadable-object (net stream :type t)
    (format stream "~D (~D instructions)"
            (network-number net)
            (length (network-instructions net)))))

(defclass parsed-subroutine ()
  ((name :initarg :name
         :accessor subroutine-name
         :documentation "Subroutine name")
   (networks :initarg :networks
             :accessor subroutine-networks
             :initform nil
             :documentation "List of networks in subroutine")
   (line-number :initarg :line-number
                :accessor subroutine-line-number
                :initform 0
                :documentation "Line where subroutine starts"))
  (:documentation "A parsed subroutine"))

(defmethod print-object ((sbr parsed-subroutine) stream)
  (print-unreadable-object (sbr stream :type t)
    (format stream "~A (~D networks)"
            (subroutine-name sbr)
            (length (subroutine-networks sbr)))))

;;; ============================================================
;;; Parsed Program Structure
;;; ============================================================

(defclass parsed-program ()
  ((main-networks :initarg :main-networks
                  :accessor program-main-networks
                  :initform nil
                  :documentation "Networks in main program")
   (subroutines :initarg :subroutines
                :accessor program-subroutines
                :initform (make-hash-table :test 'equal)
                :documentation "Hash table of subroutines by name")
   (errors :initarg :errors
           :accessor program-errors
           :initform nil
           :documentation "List of parse errors")
   (warnings :initarg :warnings
             :accessor program-warnings
             :initform nil
             :documentation "List of parse warnings"))
  (:documentation "Complete parsed IL program"))

;;; ============================================================
;;; IL Parser Class
;;; ============================================================

(defclass il-parser ()
  ((source :initarg :source
           :accessor parser-source
           :documentation "Source text (string or list of lines)")
   (lines :accessor parser-lines
          :initform nil
          :documentation "Source split into lines")
   (current-line :accessor parser-current-line
                 :initform 0
                 :documentation "Current line index")
   (errors :accessor parser-errors
           :initform nil
           :documentation "List of parse errors")
   (warnings :accessor parser-warnings
             :initform nil
             :documentation "List of warnings")
   (pending-comments :accessor parser-pending-comments
                     :initform nil
                     :documentation "Comments waiting to attach to next construct"))
  (:documentation "IL program parser"))

(defun make-il-parser (&key source)
  "Create a new IL parser"
  (make-instance 'il-parser :source source))

;;; ============================================================
;;; Tokenization
;;; ============================================================

(defun split-into-lines (source)
  "Split source string into lines, handling different line endings"
  (if (listp source)
      source
      (cl-ppcre:split "\\r?\\n" source)))

(defun trim-whitespace (str)
  "Remove leading and trailing whitespace"
  (string-trim '(#\Space #\Tab #\Return #\Newline) str))

(defun comment-line-p (line)
  "Check if line is a comment (starts with //)"
  (let ((trimmed (trim-whitespace line)))
    (and (>= (length trimmed) 2)
         (string= "//" (subseq trimmed 0 2)))))

(defun blank-line-p (line)
  "Check if line is blank"
  (zerop (length (trim-whitespace line))))

(defun extract-comment (line)
  "Extract comment text from a comment line"
  (let ((trimmed (trim-whitespace line)))
    (if (and (>= (length trimmed) 2)
             (string= "//" (subseq trimmed 0 2)))
        (trim-whitespace (subseq trimmed 2))
        nil)))

(defun tokenize-line (line)
  "Tokenize a line into opcode and parameters.
   Handles quoted strings and preserves expression structure."
  (let* ((trimmed (trim-whitespace line))
         (tokens nil)
         (current-token (make-array 0 :element-type 'character
                                      :adjustable t :fill-pointer 0))
         (in-quotes nil)
         (in-parens 0)
         (i 0)
         (len (length trimmed)))

    (labels ((push-token ()
               (when (> (length current-token) 0)
                 (push (copy-seq current-token) tokens)
                 (setf (fill-pointer current-token) 0)))

             (add-char (c)
               (vector-push-extend c current-token)))

      (loop while (< i len)
            for c = (char trimmed i)
            do (cond
                 ;; Handle quoted strings
                 ((char= c #\")
                  (if in-quotes
                      (progn
                        (add-char c)
                        (setf in-quotes nil))
                      (progn
                        (push-token)
                        (add-char c)
                        (setf in-quotes t))))

                 ;; Inside quotes - take everything
                 (in-quotes
                  (add-char c))

                 ;; Track parentheses depth
                 ((char= c #\()
                  (add-char c)
                  (incf in-parens))

                 ((char= c #\))
                  (add-char c)
                  (decf in-parens))

                 ;; Whitespace outside quotes/parens splits tokens
                 ((and (member c '(#\Space #\Tab))
                       (zerop in-parens))
                  (push-token))

                 ;; Regular character
                 (t (add-char c)))

               (incf i))

      ;; Don't forget last token
      (push-token)

      (nreverse tokens))))

(defun parse-math-expression (tokens)
  "Combine tokens after MATHDEC/MATHHEX dest and flags into expression string"
  ;; Tokens: (dest flags expr-part1 expr-part2 ...)
  ;; Return: (dest flags "expr-part1 expr-part2 ...")
  (if (>= (length tokens) 3)
      (list (first tokens)
            (second tokens)
            (format nil "~{~A~^ ~}" (cddr tokens)))
      tokens))

;;; ============================================================
;;; Line Parsing
;;; ============================================================

(defun parse-instruction-line (line line-number)
  "Parse a single instruction line into a parsed-instruction object"
  (let* ((tokens (tokenize-line line))
         (opcode (when tokens (string-upcase (first tokens))))
         (params (rest tokens)))

    (unless opcode
      (return-from parse-instruction-line nil))

    ;; Look up instruction definition
    (let ((instr-def (find-instruction opcode)))

      ;; Special handling for MATHDEC/MATHHEX - combine expression
      (when (and instr-def
                 (member opcode '("MATHDEC" "MATHHEX") :test #'string=)
                 (>= (length params) 2))
        (setf params (parse-math-expression params)))

      ;; Create parsed instruction
      (make-instance 'parsed-instruction
                     :opcode opcode
                     :params params
                     :line-number line-number
                     :instruction-def instr-def))))

;;; ============================================================
;;; Main Parser Methods
;;; ============================================================

(defmethod initialize-parser ((parser il-parser))
  "Initialize parser state from source"
  (setf (parser-lines parser) (split-into-lines (parser-source parser)))
  (setf (parser-current-line parser) 0)
  (setf (parser-errors parser) nil)
  (setf (parser-warnings parser) nil)
  (setf (parser-pending-comments parser) nil))

(defmethod peek-line ((parser il-parser))
  "Look at current line without advancing"
  (let ((lines (parser-lines parser))
        (idx (parser-current-line parser)))
    (when (< idx (length lines))
      (nth idx lines))))

(defmethod next-line ((parser il-parser))
  "Get current line and advance"
  (let ((line (peek-line parser)))
    (when line
      (incf (parser-current-line parser)))
    line))

(defmethod current-line-number ((parser il-parser))
  "Get 1-based current line number"
  (1+ (parser-current-line parser)))

(defmethod add-error ((parser il-parser) message &optional line-text)
  "Add a parse error"
  (push (make-condition 'plc-parse-error
                        :message message
                        :line-number (current-line-number parser)
                        :line-text line-text)
        (parser-errors parser)))

(defmethod add-warning ((parser il-parser) message)
  "Add a parse warning"
  (push (cons (current-line-number parser) message)
        (parser-warnings parser)))

(defmethod collect-comments ((parser il-parser))
  "Collect consecutive comment lines"
  (loop while (and (peek-line parser)
                   (comment-line-p (peek-line parser)))
        collect (extract-comment (next-line parser))))

(defmethod skip-blank-lines ((parser il-parser))
  "Skip blank lines"
  (loop while (and (peek-line parser)
                   (blank-line-p (peek-line parser)))
        do (next-line parser)))

(defmethod parse-network ((parser il-parser) network-number)
  "Parse a network's instructions until next NETWORK, SBR, or end"
  (let ((instructions nil)
        (comments (parser-pending-comments parser)))
    (setf (parser-pending-comments parser) nil)

    (loop
      ;; Skip blank lines
      (skip-blank-lines parser)

      ;; Check for end of input
      (unless (peek-line parser)
        (return))

      (let* ((line (peek-line parser))
             (trimmed (trim-whitespace line)))

        (cond
          ;; Comment line - collect and attach to next instruction
          ((comment-line-p line)
           (let ((comment-text (extract-comment (next-line parser))))
             (setf (parser-pending-comments parser)
                   (append (parser-pending-comments parser) (list comment-text)))))

          ;; Blank line
          ((blank-line-p line)
           (next-line parser))

          ;; NETWORK or SBR - end current network
          ((or (cl-ppcre:scan "^(?i)NETWORK\\s" trimmed)
               (cl-ppcre:scan "^(?i)SBR\\s" trimmed))
           (return))

          ;; Instruction line
          (t
           (let* ((line-num (current-line-number parser))
                  (parsed (parse-instruction-line (next-line parser) line-num)))

             (cond
               ((null parsed)
                ;; Empty line after trimming
                nil)

               ((null (parsed-instruction-def parsed))
                ;; Unknown instruction
                (add-error parser
                          (format nil "Unknown instruction: ~A" (parsed-opcode parsed))
                          line))

               (t
                ;; Valid instruction - attach pending comments
                (when (parser-pending-comments parser)
                  (setf (parsed-comment parsed)
                        (format nil "~{~A~^~%~}" (parser-pending-comments parser)))
                  (setf (parser-pending-comments parser) nil))

                ;; Validate parameter count
                (let ((instr-def (parsed-instruction-def parsed)))
                  (multiple-value-bind (valid error-msg)
                      (validate-instruction instr-def (parsed-params parsed))
                    (unless valid
                      (add-warning parser error-msg))))

                (push parsed instructions))))))))

    ;; Create network object
    (make-instance 'parsed-network
                   :number network-number
                   :instructions (nreverse instructions)
                   :comments comments)))

(defmethod parse-subroutine ((parser il-parser) name line-number)
  "Parse a subroutine definition"
  (let ((networks nil)
        (current-network-num nil))

    (loop
      ;; Skip blank lines
      (skip-blank-lines parser)

      ;; Collect any comments
      (let ((comments (collect-comments parser)))
        (when comments
          (setf (parser-pending-comments parser)
                (append (parser-pending-comments parser) comments))))

      ;; Check for end
      (unless (peek-line parser)
        (return))

      (let* ((line (peek-line parser))
             (trimmed (trim-whitespace line)))

        (cond
          ;; Another SBR - end this subroutine
          ((cl-ppcre:scan "^(?i)SBR\\s" trimmed)
           (return))

          ;; NETWORK declaration
          ((cl-ppcre:scan "^(?i)NETWORK\\s+(\\d+)" trimmed)
           ;; Save current network if any
           (when current-network-num
             ;; Network was empty, shouldn't happen but handle it
             nil)

           ;; Parse network number
           (multiple-value-bind (match regs)
               (cl-ppcre:scan-to-strings "^(?i)NETWORK\\s+(\\d+)" trimmed)
             (declare (ignore match))
             (setf current-network-num (parse-integer (aref regs 0)))
             (next-line parser)

             ;; Parse network contents
             (let ((net (parse-network parser current-network-num)))
               (push net networks)
               (setf current-network-num nil))))

          ;; Something else before first NETWORK
          (t
           (add-warning parser
                       (format nil "Instruction outside NETWORK in subroutine ~A" name))
           (next-line parser)))))

    ;; Create subroutine object
    (make-instance 'parsed-subroutine
                   :name name
                   :networks (nreverse networks)
                   :line-number line-number)))

(defmethod parse-program ((parser il-parser))
  "Parse the complete IL program"
  (initialize-parser parser)

  (let ((main-networks nil)
        (subroutines (make-hash-table :test 'equal))
        (in-main t))

    (loop
      ;; Skip blank lines
      (skip-blank-lines parser)

      ;; Collect comments
      (let ((comments (collect-comments parser)))
        (when comments
          (setf (parser-pending-comments parser)
                (append (parser-pending-comments parser) comments))))

      ;; Skip more blank lines
      (skip-blank-lines parser)

      ;; Check for end
      (unless (peek-line parser)
        (return))

      (let* ((line (peek-line parser))
             (trimmed (trim-whitespace line)))

        (cond
          ;; SBR declaration - start subroutine
          ((cl-ppcre:scan "^(?i)SBR\\s+(\\S+)" trimmed)
           (setf in-main nil)
           (multiple-value-bind (match regs)
               (cl-ppcre:scan-to-strings "^(?i)SBR\\s+(\\S+)" trimmed)
             (declare (ignore match))
             (let* ((sbr-name (aref regs 0))
                    (line-num (current-line-number parser)))
               (next-line parser)
               (let ((sbr (parse-subroutine parser sbr-name line-num)))
                 (setf (gethash sbr-name subroutines) sbr)))))

          ;; NETWORK declaration in main
          ((cl-ppcre:scan "^(?i)NETWORK\\s+(\\d+)" trimmed)
           (multiple-value-bind (match regs)
               (cl-ppcre:scan-to-strings "^(?i)NETWORK\\s+(\\d+)" trimmed)
             (declare (ignore match))
             (let ((net-num (parse-integer (aref regs 0))))
               (next-line parser)
               (let ((net (parse-network parser net-num)))
                 (when in-main
                   (push net main-networks))))))

          ;; Something else at top level
          (t
           (add-warning parser (format nil "Unexpected content at top level: ~A" trimmed))
           (next-line parser)))))

    ;; Build result
    (make-instance 'parsed-program
                   :main-networks (nreverse main-networks)
                   :subroutines subroutines
                   :errors (nreverse (parser-errors parser))
                   :warnings (nreverse (parser-warnings parser)))))

;;; ============================================================
;;; Utility Functions
;;; ============================================================

(defun parse-il-file (pathname)
  "Parse an IL file and return parsed program"
  (let* ((source (alexandria:read-file-into-string pathname))
         (parser (make-il-parser :source source)))
    (parse-program parser)))

(defun parse-il-string (source)
  "Parse an IL string and return parsed program"
  (let ((parser (make-il-parser :source source)))
    (parse-program parser)))

;;; ============================================================
;;; Accessor Functions (for package exports)
;;; ============================================================

(defun parser-networks (parser)
  "Get networks from a parsed program via parser"
  (declare (ignore parser))
  (error "Use parse-program to get a parsed-program, then access program-main-networks"))

(defun parser-subroutines (parser)
  "Get subroutines from a parsed program via parser"
  (declare (ignore parser))
  (error "Use parse-program to get a parsed-program, then access program-subroutines"))

;;; ============================================================
;;; Debug/Inspection Functions
;;; ============================================================

(defun print-parsed-program (program &optional (stream t))
  "Print a summary of a parsed program"
  (format stream "~%=== Parsed Program Summary ===~%")

  ;; Errors
  (let ((errors (program-errors program)))
    (when errors
      (format stream "~%ERRORS (~D):~%" (length errors))
      (dolist (err errors)
        (format stream "  Line ~D: ~A~%"
                (parse-error-line err)
                (parse-error-message err)))))

  ;; Warnings
  (let ((warnings (program-warnings program)))
    (when warnings
      (format stream "~%WARNINGS (~D):~%" (length warnings))
      (dolist (warn warnings)
        (format stream "  Line ~D: ~A~%" (car warn) (cdr warn)))))

  ;; Main program
  (format stream "~%MAIN PROGRAM:~%")
  (format stream "  Networks: ~D~%" (length (program-main-networks program)))
  (dolist (net (program-main-networks program))
    (format stream "    Network ~D: ~D instructions~%"
            (network-number net)
            (length (network-instructions net))))

  ;; Subroutines
  (let ((sbr-count (hash-table-count (program-subroutines program))))
    (format stream "~%SUBROUTINES (~D):~%" sbr-count)
    (maphash (lambda (name sbr)
               (format stream "  ~A: ~D networks~%"
                       name (length (subroutine-networks sbr))))
             (program-subroutines program))))

(defun print-network-detail (network &optional (stream t))
  "Print detailed view of a network"
  (format stream "Network ~D:~%" (network-number network))
  (dolist (instr (network-instructions network))
    (format stream "  ~4D: ~A~{ ~A~}~%"
            (parsed-line-number instr)
            (parsed-opcode instr)
            (parsed-params instr))))

(defun count-instructions (program)
  "Count total instructions in a parsed program"
  (let ((count 0))
    ;; Main program
    (dolist (net (program-main-networks program))
      (incf count (length (network-instructions net))))
    ;; Subroutines
    (maphash (lambda (name sbr)
               (declare (ignore name))
               (dolist (net (subroutine-networks sbr))
                 (incf count (length (network-instructions net)))))
             (program-subroutines program))
    count))

;;; End of parser.lisp
