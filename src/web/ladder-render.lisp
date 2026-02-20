;;;; src/web/ladder-render.lisp
;;;;
;;;; IL to Ladder Diagram Conversion
;;;; Converts parsed IL networks to ladder cell matrix format for visualization

(in-package #:mblogic-clog-web)

;;; ============================================================
;;; Ladder Symbol Mapping
;;; ============================================================

(defparameter *ladsymb-to-svg*
              '((:contact-no . "noc") ; Normally open contact
                                     (:contact-nc . "ncc") ; Normally closed contact
                                     (:contact-pd . "nocpd") ; Positive differential (rising edge) contact
                                     (:contact-nd . "nocnd") ; Negative differential (falling edge) contact
                                     (:coil . "out") ; Standard output coil
                                     (:coil-set . "set") ; Set (latch) coil
                                     (:coil-reset . "rst") ; Reset (unlatch) coil
                                     (:coil-pd . "pd") ; Pulse coil
                                     (:branch-end . nil) ; Branch end - no symbol, handled structurally
                                     (:compare . "compare") ; Comparison block
                                     (:timer . "tmr") ; Timer block
                                     (:counter . "cntu") ; Counter block
                                     (:math . "mathdec") ; Math block
                                     (:copy . "copy") ; Copy instruction
                                     (:cpyblk . "cpyblk") ; Block copy
                                     (:fill . "fill") ; Fill instruction
                                     (:pack . "pack") ; Pack bits
                                     (:unpack . "unpack") ; Unpack bits
                                     (:shfrg . "shfrg") ; Shift register
                                     (:find . "findeq") ; Search instruction
                                     (:sum . "sum") ; Sum instruction
                                     (:call . "call") ; Subroutine call
                                     (:return . "rt") ; Return
                                     (:return-cond . "rtc") ; Conditional return
                                     (:end . "end") ; End
                                     (:end-cond . "endc") ; Conditional end
                                     (:for . "for") ; For loop start
                                     (:next . "next")) ; For loop end
              "Mapping from instruction :ladsymb to SVG symbol names")

(defun ladsymb-to-svg-symbol (ladsymb opcode)
  "Convert instruction ladsymb keyword to SVG symbol name.
   Uses opcode for more specific mapping when needed."
  (let ((svg-sym (cdr (assoc ladsymb *ladsymb-to-svg*))))
    ;; Handle special cases where opcode matters
    (cond
     ;; Comparisons - map to specific JS symbols
     ((eq ladsymb :compare)
       (cond
        ((member opcode '("STRE" "ANDE" "ORE") :test #'string-equal) "compeq")
        ((member opcode '("STRNE" "ANDNE" "ORNE") :test #'string-equal) "compneq")
        ((member opcode '("STRGT" "ANDGT" "ORGT") :test #'string-equal) "compgt")
        ((member opcode '("STRLT" "ANDLT" "ORLT") :test #'string-equal) "complt")
        ((member opcode '("STRGE" "ANDGE" "ORGE") :test #'string-equal) "compge")
        ((member opcode '("STRLE" "ANDLE" "ORLE") :test #'string-equal) "comple")
        (t "compeq"))) ; Default comparison
     ;; Timers
     ((and (eq ladsymb :timer) (string-equal opcode "TMR")) "tmr")
     ((and (eq ladsymb :timer) (string-equal opcode "TMRA")) "tmra")
     ((and (eq ladsymb :timer) (string-equal opcode "TMROFF")) "tmroff")
     ;; Counters
     ((and (eq ladsymb :counter) (string-equal opcode "CNTU")) "cntu")
     ((and (eq ladsymb :counter) (string-equal opcode "CNTD")) "cntd")
     ((and (eq ladsymb :counter) (string-equal opcode "UDC")) "udc")
     ;; Find instructions
     ((eq ladsymb :find)
       (cond
        ((member opcode '("FINDEQ" "FINDIEQ") :test #'string-equal) "findeq")
        ((member opcode '("FINDNE" "FINDINE") :test #'string-equal) "findne")
        ((member opcode '("FINDGT" "FINDIGT") :test #'string-equal) "findgt")
        ((member opcode '("FINDLT" "FINDILT") :test #'string-equal) "findlt")
        ((member opcode '("FINDGE" "FINDIGE") :test #'string-equal) "findge")
        ((member opcode '("FINDLE" "FINDILE") :test #'string-equal) "findle")
        (t "findeq")))
     ;; Default
     (t (or svg-sym "il"))))) ; "il" = raw IL display fallback

;;; ============================================================
;;; Ladder Cell Structure
;;; ============================================================

(defstruct ladder-cell
  "A single cell in the ladder diagram matrix"
  (type nil) ; :contact, :coil, :block, :hline, :vline, :branch, :empty
  (symbol nil) ; SVG symbol name
  (address nil) ; PLC address (string) or nil
  (addresses nil) ; List of addresses for multi-address instructions
  (value nil) ; Display value or preset
  (opcode nil) ; Original opcode for reference
  (params nil) ; Original parameters
  (row 0) ; Row position in matrix
  (col 0) ; Column position in matrix
  (monitor-type nil)) ; :bool, :word, :timer, :counter for live monitoring

(defstruct ladder-rung
  "A complete ladder rung (network)"
  (number 0) ; Network number
  (cells nil) ; 2D array or list of ladder-cell
  (rows 1) ; Number of rows (for branches)
  (cols 0) ; Number of columns
  (addresses nil) ; All addresses for monitoring
  (comment nil) ; Associated comment
  (il-fallback nil) ; If t, display raw IL instead
  (branches nil) ; List of (row start-col merge-col) for input branch rendering
  (output-branches nil)); List of (col rows...) for parallel output coils

(defstruct ladder-program
  "Complete ladder program structure"
  (name "main") ; Subroutine name
  (rungs nil) ; List of ladder-rung
  (addresses nil)) ; All unique addresses

;;; ============================================================
;;; Branch Connector Types (Python-compatible)
;;; ============================================================
;;; These match the Python MBLogic matrixdata format exactly.
;;; Branch connectors are explicit cells in the matrix, not computed at render time.

;; Branch connector symbol names - use Python-internal names during matrix construction.
;; These match the Python PLCLadder.py names exactly.
;; Mapping to JS-compatible names happens at output time in cl-symbol-to-js-symbol.
(defparameter *branch-ttr* "branchttr" "Top-right corner: ┐ - top of branch fork")
(defparameter *branch-tr* "branchtr" "Middle-right T: ┤ - middle rows of branch fork")
(defparameter *branch-r* "branchr" "Bottom-right corner: ┘ - bottom of branch fork")
(defparameter *vbar-r* "vbarr" "Vertical bar right: │ - no junction (right side)")
(defparameter *branch-ttl* "branchttl" "Top-left corner: ┌ - top of merge/close")
(defparameter *branch-tl* "branchtl" "Middle-left T: ├ - middle rows of merge")
(defparameter *branch-l* "branchl" "Bottom-left corner: └ - bottom of merge/close")
(defparameter *vbar-l* "vbarl" "Vertical bar left: │ - no junction (left side)")
(defparameter *hbar* "hbar" "Horizontal bar: ─ - wire segment")

;; List of all branch/connector symbols for identification
;; Matches Python: self._Branches
(defparameter *branch-symbols*
              (list *branch-ttr* *branch-tr* *branch-r* *vbar-r*
                    *branch-ttl* *branch-tl* *branch-l* *vbar-l* *hbar*)
              "All branch connector symbol names")

;; Vertical branch symbols (excludes horizontal)
;; Matches Python: self._VertBranches
(defparameter *vertical-branch-symbols*
              (list *branch-ttr* *branch-tr* *branch-r* *vbar-r*
                    *branch-ttl* *branch-tl* *branch-l* *vbar-l*)
              "Vertical branch connector symbols")

(defun branch-symbol-p (symbol)
  "Check if SYMBOL is a branch connector symbol"
  (member symbol *branch-symbols* :test #'string-equal))

(defun vertical-branch-symbol-p (symbol)
  "Check if SYMBOL is a vertical branch connector symbol"
  (member symbol *vertical-branch-symbols* :test #'string-equal))

;;; ============================================================
;;; Branch Cell Constructors
;;; ============================================================

(defun make-branch-cell (symbol &key (row 0) (col 0))
  "Create a branch connector cell with the given SYMBOL at ROW, COL.
   Branch cells have no address and monitor type 'none'."
  (make-ladder-cell
   :type :branch
   :symbol symbol
   :address nil
   :addresses nil
   :row row
   :col col
   :monitor-type nil))

(defun make-hbar-cell (&key (row 0) (col 0))
  "Create a horizontal bar cell"
  (make-branch-cell *hbar* :row row :col col))

(defun make-vbar-l-cell (&key (row 0) (col 0))
  "Create a vertical bar (left side) cell"
  (make-branch-cell *vbar-l* :row row :col col))

(defun make-branch-l-cell (&key (row 0) (col 0))
  "Create a bottom-left corner cell: └"
  (make-branch-cell *branch-l* :row row :col col))

(defun make-branch-tl-cell (&key (row 0) (col 0))
  "Create a middle-left T cell: ├"
  (make-branch-cell *branch-tl* :row row :col col))

(defun make-branch-ttl-cell (&key (row 0) (col 0))
  "Create a top-left corner cell: ┌"
  (make-branch-cell *branch-ttl* :row row :col col))

(defun make-branch-tr-cell (&key (row 0) (col 0))
  "Create a middle-right T cell: ┤"
  (make-branch-cell *branch-tr* :row row :col col))

(defun make-branch-ttr-cell (&key (row 0) (col 0))
  "Create a top-right corner cell: ┐"
  (make-branch-cell *branch-ttr* :row row :col col))

(defun make-branch-r-cell (&key (row 0) (col 0))
  "Create a bottom-right corner cell: ┘"
  (make-branch-cell *branch-r* :row row :col col))

(defun make-vbar-r-cell (&key (row 0) (col 0))
  "Create a vertical bar (right side) cell"
  (make-branch-cell *vbar-r* :row row :col col))

;;; ============================================================
;;; Instruction Classification
;;; ============================================================

(defun contact-instruction-p (opcode)
  "Check if opcode is a contact (input) instruction"
  (member opcode '("STR" "STRN" "AND" "ANDN" "OR" "ORN"
                         "STRPD" "STRND" "ANDPD" "ANDND" "ORPD" "ORND")
          :test #'string-equal))

(defun store-instruction-p (opcode)
  "Check if opcode is a STR (store/start new logic block) instruction"
  (member opcode '("STR" "STRN" "STRPD" "STRND"
                         "STRE" "STRNE" "STRGT" "STRLT" "STRGE" "STRLE")
          :test #'string-equal))

(defun and-instruction-p (opcode)
  "Check if opcode is an AND instruction (continues current row)"
  (member opcode '("AND" "ANDN" "ANDPD" "ANDND"
                         "ANDE" "ANDNE" "ANDGT" "ANDLT" "ANDGE" "ANDLE")
          :test #'string-equal))

(defun or-instruction-p (opcode)
  "Check if opcode is an OR instruction (creates parallel branch)"
  (member opcode '("OR" "ORN" "ORPD" "ORND"
                        "ORE" "ORNE" "ORGT" "ORLT" "ORGE" "ORLE")
          :test #'string-equal))

(defun orstr-instruction-p (opcode)
  "Check if opcode is ORSTR (merge parallel blocks)"
  (string-equal opcode "ORSTR"))

(defun continuation-instruction-p (opcode)
  "Check if opcode is a continuation instruction (comparison/pulse that continues current row).
   These instructions should be appended to the continuation row after nested branches.
   Includes: STRE, STRNE, STRGT, STRLT, STRGE, STRLE, ORPD, ORND, ANDGT, ANDLT, ANDGE, ANDLE"
  (member opcode '("STRE" "STRNE" "STRGT" "STRLT" "STRGE" "STRLE"
                          "ORPD" "ORND"
                          "ANDGT" "ANDLT" "ANDGE" "ANDLE")
          :test #'string-equal))

(defun andstr-instruction-p (opcode)
  "Check if opcode is ANDSTR (merge series blocks)"
  (string-equal opcode "ANDSTR"))

(defun coil-instruction-p (opcode)
  "Check if opcode is a coil (output) instruction"
  (member opcode '("OUT" "SET" "RST" "PD") :test #'string-equal))

(defun branch-start-p (opcode)
  "Check if instruction starts a parallel branch"
  (member opcode '("OR" "ORN" "ORPD" "ORND"
                        "ORE" "ORNE" "ORGT" "ORLT" "ORGE" "ORLE")
          :test #'string-equal))

(defun branch-end-p (opcode)
  "Check if instruction ends branches"
  (member opcode '("ANDSTR" "ORSTR") :test #'string-equal))

(defun block-instruction-p (opcode)
  "Check if instruction renders as a block"
  (member opcode '("TMR" "TMRA" "TMROFF" "CNTU" "CNTD" "UDC"
                         "COPY" "CPYBLK" "FILL" "PACK" "UNPACK" "SHFRG"
                         "MATHDEC" "MATHHEX" "SUM"
                         "FINDEQ" "FINDNE" "FINDGT" "FINDLT" "FINDGE" "FINDLE"
                         "FINDIEQ" "FINDINE" "FINDIGT" "FINDILT" "FINDIGE" "FINDILE"
                         "STRE" "STRNE" "STRGT" "STRLT" "STRGE" "STRLE"
                         "ANDE" "ANDNE" "ANDGT" "ANDLT" "ANDGE" "ANDLE"
                         "ORE" "ORNE" "ORGT" "ORLT" "ORGE" "ORLE"
                         "CALL" "FOR")
          :test #'string-equal))

(defun control-instruction-p (opcode)
  "Check if instruction is a control flow instruction"
  (member opcode '("END" "ENDC" "RT" "RTC" "NEXT") :test #'string-equal))

(defun output-block-instruction-p (opcode)
  "Check if instruction is a block that belongs in the output column.
   These are NOT comparison instructions (which modify the logic stack like contacts)."
  (member opcode '("TMR" "TMRA" "TMROFF" "CNTU" "CNTD" "UDC"
                         "COPY" "CPYBLK" "FILL" "PACK" "UNPACK" "SHFRG"
                         "MATHDEC" "MATHHEX" "SUM"
                         "FINDEQ" "FINDNE" "FINDGT" "FINDLT" "FINDGE" "FINDLE"
                         "FINDIEQ" "FINDINE" "FINDIGT" "FINDILT" "FINDIGE" "FINDILE"
                         "CALL" "FOR")
          :test #'string-equal))

(defun rungtype-for-output (opcode)
  "Return the rungtype string for an output block instruction.
   Double rungs have 2 input rows, triple have 3."
  (cond
   ((member opcode '("CNTU" "CNTD" "TMRA") :test #'string-equal) "double")
   ((member opcode '("UDC" "SHFRG") :test #'string-equal) "triple")
   (t "single")))

;;; ============================================================
;;; Address Extraction
;;; ============================================================

(defun extract-addresses (instruction)
  "Extract all monitorable addresses from a parsed instruction"
  (let ((opcode (mblogic-cl:parsed-opcode instruction))
        (params (mblogic-cl:parsed-params instruction))
        (addresses nil))
    (cond
     ;; Contact instructions - single boolean address
     ((contact-instruction-p opcode)
       (when (and params (mblogic-cl:bool-addr-p (first params)))
             (push (first params) addresses)))

     ;; Coil instructions - multiple boolean addresses
     ((coil-instruction-p opcode)
       (dolist (p params)
         (when (mblogic-cl:bool-addr-p p)
               (push p addresses))))

     ;; Timer - timer address and preset
     ((member opcode '("TMR" "TMRA" "TMROFF") :test #'string-equal)
       (when params
             (let ((timer-addr (first params)))
               ;; Timer bit: T1, Timer data: TD1
               (when (cl-ppcre:scan "^T[0-9]+$" timer-addr)
                     (push timer-addr addresses)
                     (push (format nil "TD~A"
                             (subseq timer-addr 1))
                           addresses)))))

     ;; Counter - counter address
     ((member opcode '("CNTU" "CNTD" "UDC") :test #'string-equal)
       (when params
             (let ((ctr-addr (first params)))
               (when (cl-ppcre:scan "^CT[0-9]+$" ctr-addr)
                     (push ctr-addr addresses)
                     (push (format nil "CTD~A"
                             (subseq ctr-addr 2))
                           addresses)))))

     ;; Compare instructions - all params (addresses AND literals for display)
     ((member opcode '("STRE" "STRNE" "STRGT" "STRLT" "STRGE" "STRLE"
                              "ANDE" "ANDNE" "ANDGT" "ANDLT" "ANDGE" "ANDLE"
                              "ORE" "ORNE" "ORGT" "ORLT" "ORGE" "ORLE")
              :test #'string-equal)
       (dolist (p params)
         (push p addresses)))

     ;; COPY - source and dest
     ((string-equal opcode "COPY")
       (dolist (p params)
         (when (mblogic-cl:any-addr-p p)
               (push p addresses))))

     ;; Math - destination
     ((member opcode '("MATHDEC" "MATHHEX") :test #'string-equal)
       (when (and params (mblogic-cl:any-addr-p (first params)))
             (push (first params) addresses))))

    (nreverse addresses)))

(defun get-monitor-type (instruction)
  "Determine the monitor type for an instruction"
  (let ((instr-def (mblogic-cl:parsed-instruction-def instruction)))
    (when instr-def
          (mblogic-cl:instruction-monitor instr-def))))

;;; ============================================================
;;; Instruction to Cell Conversion
;;; ============================================================

(defun instruction-to-cell (instruction col)
  "Convert a parsed instruction to a ladder cell"
  (let* ((opcode (mblogic-cl:parsed-opcode instruction))
         (params (mblogic-cl:parsed-params instruction))
         (instr-def (mblogic-cl:parsed-instruction-def instruction))
         (ladsymb (when instr-def (mblogic-cl:instruction-ladsymb instr-def)))
         (svg-symbol (ladsymb-to-svg-symbol ladsymb opcode))
         (addresses (extract-addresses instruction)))

    (make-ladder-cell
     :type (cond
            ((contact-instruction-p opcode) :contact)
            ((coil-instruction-p opcode) :coil)
            ((block-instruction-p opcode) :block)
            ((control-instruction-p opcode) :control)
            ((branch-end-p opcode) :branch-end)
            (t :unknown))
     :symbol svg-symbol
     :address (first addresses)
     :addresses addresses
     :opcode opcode
     :params params
     :col col
     :monitor-type (get-monitor-type instruction))))

;;; ============================================================
;;; Matrix Operations (Python-compatible algorithm)
;;; ============================================================
;;; The ladder matrix is stored as a list of rows: ((row0-cells) (row1-cells) ...)
;;; Each row is a list of cells (or nil for empty positions).
;;; This matches the Python PLCLadder.py implementation.

(defun matrix-width (matrix)
  "Return the width (max column count) of the matrix"
  (if (null matrix)
      0
      (reduce #'max matrix :key #'length :initial-value 0)))

(defun matrix-height (matrix)
  "Return the height (row count) of the matrix"
  (length matrix))

(defun copy-cell (cell)
  "Create a shallow copy of a ladder cell"
  (when cell
        (make-ladder-cell
         :type (ladder-cell-type cell)
         :symbol (ladder-cell-symbol cell)
         :address (ladder-cell-address cell)
         :addresses (copy-list (ladder-cell-addresses cell))
         :value (ladder-cell-value cell)
         :opcode (ladder-cell-opcode cell)
         :params (copy-list (ladder-cell-params cell))
         :row (ladder-cell-row cell)
         :col (ladder-cell-col cell)
         :monitor-type (ladder-cell-monitor-type cell))))

(defun append-cell-to-matrix (cell matrix)
  "Append CELL to the first row of MATRIX, padding other rows with nil.
   Returns the modified matrix."
  (let ((cell-copy (copy-cell cell)))
    ;; Add to first row
    (setf (first matrix) (append (first matrix) (list cell-copy)))
    ;; Pad other rows with nil to keep rectangular
    (dolist (row (rest matrix))
      (nconc row (list nil)))
    matrix))

(defun append-cell-to-row (cell matrix target-row)
  "Append CELL to a specific TARGET-ROW of MATRIX, padding other rows with hbar.
   Returns the modified matrix."
  (let* ((cell-copy (copy-cell cell))
         (height (matrix-height matrix))
         (target-row (min target-row (1- height))))
    ;; Add to target row
    (setf (nth target-row matrix) (append (nth target-row matrix) (list cell-copy)))
    ;; Pad other rows with hbar to keep rectangular
    (dotimes (i height)
      (unless (= i target-row)
        (let ((row (nth i matrix)))
          (nconc row (list (make-hbar-cell))))))
    matrix))

(defun append-cell-to-last-row (cell matrix)
  "Append CELL to the LAST row of MATRIX, padding other rows with hbar.
   Returns the modified matrix."
  (let* ((cell-copy (copy-cell cell))
         (height (matrix-height matrix))
         (last-row-idx (1- height))
         (last-row (nth last-row-idx matrix)))
    ;; Add to last row
    (setf last-row (append last-row (list cell-copy)))
    (setf (nth last-row-idx matrix) last-row)
    ;; Pad other rows with hbar to keep rectangular
    (dotimes (i last-row-idx)
      (let ((row (nth i matrix)))
        (nconc row (list (make-hbar-cell)))))
    matrix))

(defun has-fork-column-p (matrix)
  "Check if matrix already has a fork connector column at column 0.
   Returns T if the first cell of the first row is a vertical branch connector."
  (and matrix
       (first matrix)
       (first (first matrix))
       (vertical-branch-symbol-p (ladder-cell-symbol (first (first matrix))))))

(defun merge-matrix-below (original-matrix new-matrix)
  "Merge NEW-MATRIX below ORIGINAL-MATRIX (for OR/ORSTR parallel connections).
   Matches Python PLCLadder.MergeBelow algorithm exactly:
   - Smart padding per row based on last cell content
   - Row 0 always gets hbar padding
   - Other rows get nil padding if last cell is nil or vertical branch, else hbar
   Returns the merged matrix."
  (let* ((original-width (matrix-width original-matrix))
         (new-width (matrix-width new-matrix)))

    (cond
     ;; Original is wider - pad new-matrix rows
     ((> original-width new-width)
       (dotimes (i (length new-matrix))
         (let ((row (nth i new-matrix))
               (diff (- original-width new-width)))
           (cond
            ;; Row 0: pad with hbar
            ((= i 0)
              (dotimes (_ diff)
                (nconc row (list (make-hbar-cell)))))
            ;; Last cell is nil: pad with nil
            ((null (first (last row)))
              (dotimes (_ diff)
                (nconc row (list nil))))
            ;; Last cell is vertical branch: pad with nil
            ((and (first (last row))
                  (vertical-branch-symbol-p (ladder-cell-symbol (first (last row)))))
              (dotimes (_ diff)
                (nconc row (list nil))))
            ;; Otherwise: pad with hbar
            (t
              (dotimes (_ diff)
                (nconc row (list (make-hbar-cell)))))))))

     ;; New is wider - pad original-matrix rows
     ((> new-width original-width)
       (dotimes (i (length original-matrix))
         (let ((row (nth i original-matrix))
               (diff (- new-width original-width)))
           (cond
            ;; Row 0: pad with hbar
            ((= i 0)
              (dotimes (_ diff)
                (nconc row (list (make-hbar-cell)))))
            ;; Last cell is nil: pad with nil
            ((null (first (last row)))
              (dotimes (_ diff)
                (nconc row (list nil))))
            ;; Last cell is vertical branch: pad with nil
            ((and (first (last row))
                  (vertical-branch-symbol-p (ladder-cell-symbol (first (last row)))))
              (dotimes (_ diff)
                (nconc row (list nil))))
            ;; Otherwise: pad with hbar
            (t
              (dotimes (_ diff)
                (nconc row (list (make-hbar-cell))))))))))

    ;; Merge: append new-matrix rows to original-matrix
    (nconc original-matrix new-matrix)
    original-matrix))

(defun merge-matrix-right (original-matrix new-matrix)
  "Merge NEW-MATRIX to the right of ORIGINAL-MATRIX (for ANDSTR series connection).
   Matches Python PLCLadder._MergeRight algorithm exactly:
   - When new-matrix has multiple rows, INSERT left-side connectors into new-matrix
     BEFORE merging (branchttr at top, branchtr in middle, branchr at bottom)
   - Handle height differences by padding BOTH matrices to same height
   - Width equalization with hbar padding
   Returns the merged matrix."
  (let* ((original-height (matrix-height original-matrix))
         (new-height (matrix-height new-matrix))
         ;; Work with fresh copies to avoid modifying originals
         (orig-matrix (mapcar #'copy-list original-matrix))
         (new-mx (mapcar #'copy-list new-matrix)))

    ;; Step 1: Insert left-side branch connectors into new-matrix if it has multiple rows
    (when (> new-height 1)
          (dotimes (row-idx new-height)
            (let ((row (nth row-idx new-mx)))
              ;; Insert branch connector at beginning of each row
              (setf (nth row-idx new-mx)
                (cons (make-branch-tr-cell) row))))
          ;; Set first row to branchttr and last row to branchr
          (setf (ladder-cell-symbol (first (first new-mx))) *branch-ttr*)
          (setf (ladder-cell-symbol (first (car (last new-mx)))) *branch-r*))

    ;; Step 2: Equalize widths with hbar padding
    (let ((max-width (max (matrix-width orig-matrix) (matrix-width new-mx))))
      (dolist (row orig-matrix)
        (let ((row-width (length row)))
          (dotimes (i (- max-width row-width))
            (nconc row (list (make-hbar-cell))))))
      (dolist (row new-mx)
        (let ((row-width (length row)))
          (dotimes (i (- max-width row-width))
            (nconc row (list (make-hbar-cell)))))))

    ;; Step 3: Equalize heights by padding shorter matrix with nil rows
    (let ((orig-width (matrix-width orig-matrix))
          (new-width (matrix-width new-mx)))
      (cond
       ((> original-height new-height)
         ;; Pad new-matrix with nil rows
         (dotimes (i (- original-height new-height))
           (setf new-mx (append new-mx (list (make-list new-width :initial-element nil))))))
       ((> new-height original-height)
         ;; Pad original-matrix with nil rows
         (dotimes (i (- new-height original-height))
           (setf orig-matrix (append orig-matrix (list (make-list orig-width :initial-element nil))))))))

    ;; Step 4: Merge by appending new-matrix rows to original-matrix rows
    (loop for orig-row in orig-matrix
          for new-row in new-mx
          do (nconc orig-row new-row))

    orig-matrix))

(defun close-branch-block (matrix)
  "Close the RIGHT side of a branch block (for OR/ORSTR) by adding LEFT-side connectors.
   Matches Python PLCLadder.CloseBlock algorithm exactly:
   - Determines wideinstr flag (whether last non-nil cell in any row is not a branch)
   - Determines lastrow (last row with a non-nil last cell)
   - Adds appropriate connectors per row based on current last cell
   Returns the modified matrix."
  (let* ((height (matrix-height matrix))
         (wideinstr nil)
         (lastrow 0))

    ;; Calculate wideinstr and lastrow
    (dotimes (i height)
      (let ((row (nth i matrix)))
        (when (>= (length row) 1)
              (let ((last-cell (first (last row))))
                (when (and last-cell
                           (not (branch-symbol-p (ladder-cell-symbol last-cell))))
                      (setf wideinstr t))
                (when last-cell
                      (setf lastrow i))))))

    ;; Process each row to add left-side connectors
    (dotimes (i height)
      (let ((row (nth i matrix))
            (last-cell (first (last (nth i matrix)))))
        (cond
         ;; Row is beyond lastrow with content
         ((> i lastrow)
           (when wideinstr
                 (nconc row (list nil))))

         ;; Last cell is nil
         ((null last-cell)
           (when (not wideinstr)
                 (setf (nth i matrix) (butlast row)))
           (nconc (nth i matrix) (list (make-vbar-l-cell))))

         ;; Last cell is hbar
         ((and last-cell (string-equal (ladder-cell-symbol last-cell) *hbar*))
           (if wideinstr
               ;; Append branchtl after hbar
               (nconc row (list (make-branch-tl-cell)))
               ;; Replace hbar with branchtl
               (progn
                (setf (nth i matrix) (butlast row))
                (nconc (nth i matrix) (list (make-branch-tl-cell))))))

         ;; Last cell is NOT a branch (instruction)
         ((and last-cell (not (branch-symbol-p (ladder-cell-symbol last-cell))))
           (if wideinstr
               ;; Append branchtl after instruction
               (nconc row (list (make-branch-tl-cell)))
               ;; Replace instruction with branchtl
               (progn
                (setf (nth i matrix) (butlast row))
                (nconc (nth i matrix) (list (make-branch-tl-cell))))))

         ;; Last cell is branchl
         ((and last-cell (string-equal (ladder-cell-symbol last-cell) *branch-l*))
           (when (not wideinstr)
                 ;; Keep as is - no change needed
                 nil))

         ;; Last cell is another branch symbol
         ((and last-cell (branch-symbol-p (ladder-cell-symbol last-cell)))
           (when (not wideinstr)
                 ;; Keep as is - no change needed
                 nil))

         ;; Default case
         (t
           (when (not wideinstr)
                 (setf (nth i matrix) (butlast row)))
           (nconc (nth i matrix) (list (make-vbar-l-cell)))))))

    ;; Add final corner connectors: branchttl at top, branchl at bottom
    (let ((top-row (first matrix))
          (bottom-row (nth lastrow matrix)))
      ;; Top row: pop last cell, append branchttl
      (setf (first matrix) (butlast top-row))
      (nconc (first matrix) (list (make-branch-ttl-cell)))
      ;; Bottom row: pop last cell, append branchl
      (setf (nth lastrow matrix) (butlast bottom-row))
      (nconc (nth lastrow matrix) (list (make-branch-l-cell))))

    matrix))

(defun add-orstr-junction (matrix top-height)
  "Add junction connectors at the ORSTR merge point.
   TOP-HEIGHT is the number of rows from the top matrix (before merge).
   This adds vertical connections between the bottom of the top section
   and the top of the bottom section at the rightmost column.
   Result: top-bottom-row gets branchtl (├), bottom-top-row gets branchr (┘)
   Returns the modified matrix."
  (let ((width (matrix-width matrix))
        (height (matrix-height matrix)))
    (when (and (> width 0) (> height top-height) (> top-height 0))
          (let* ((junction-col (1- width))
                 (top-bottom-row (1- top-height))
                 (bottom-top-row top-height)
                 (top-row (nth top-bottom-row matrix))
                 (bottom-row (nth bottom-top-row matrix)))
            ;; Top section bottom row: convert to branchtl (├) - connects up, down, left
            (when (and top-row (>= (length top-row) width))
                  (let ((top-cell (nth junction-col top-row)))
                    (when top-cell
                          (setf (ladder-cell-symbol top-cell) *branch-tl*))))
            ;; Bottom section top row: convert to branchr (┘) - connects up, left
            (when (and bottom-row (>= (length bottom-row) width))
                  (let ((bottom-cell (nth junction-col bottom-row)))
                    (when bottom-cell
                          (setf (ladder-cell-symbol bottom-cell) *branch-r*))))))
    matrix))

;;; ============================================================
;;; Network to Ladder Rung Conversion
;;; ============================================================

(defun multi-address-coil-p (opcode params)
  "Check if this is a coil instruction with multiple output addresses"
  (and (coil-instruction-p opcode)
       (> (length params) 1)))

(defun make-coil-cell (opcode addr col row &optional is-range)
  "Create a single coil cell for one address.
   IS-RANGE indicates if this is part of a range address output."
  (let ((symbol (cond
                 ((string-equal opcode "OUT") (if is-range "out2" "out"))
                 ((string-equal opcode "SET") (if is-range "set2" "set"))
                 ((string-equal opcode "RST") (if is-range "rst2" "rst"))
                 ((string-equal opcode "PD") (if is-range "pd2" "pd"))
                 (t "out"))))
    (make-ladder-cell
     :type :coil
     :symbol symbol
     :address addr
     :addresses (list addr)
     :opcode opcode
     :params (list addr)
     :row row
     :col col
     :monitor-type :bool)))

(defun network-to-ladder-rung (network)
  "Convert a parsed network to a ladder rung structure using matrix-based algorithm.
   This produces Python-compatible matrixdata with explicit branch connector cells.

   Algorithm (matches Python PLCLadder.py):
   - Matrix is a list of rows: ((row0-cells) (row1-cells) ...)
   - matrixstack holds matrices for nested blocks
   - STR: push current matrix, start new
   - AND: append cell to current row
   - OR: create new row matrix, merge below, close block (add right connectors)
   - ORSTR: pop stack, merge below, close block
   - ANDSTR: pop stack, merge right (add left connectors)
   - Outputs are handled separately after inputs"
  (let ((instructions (mblogic-cl:network-instructions network))
        (all-addresses nil)
        ;; Input matrix processing
        (current-matrix (list (list))) ; Start with one empty row
        (matrix-stack (list)) ; Stack for STR blocks
        ;; Output collection
        (output-cells nil)
        ;; First STR flag - don't push empty matrix on first STR
        (first-store t))

    ;; Separate inputs from outputs
    (let ((inputs nil)
          (outputs nil))
      (dolist (instr instructions)
        (let ((opcode (mblogic-cl:parsed-opcode instr)))
          (cond
           ((coil-instruction-p opcode)
             (push instr outputs))
           ((control-instruction-p opcode)
             (push instr outputs))
           ((output-block-instruction-p opcode)
             (push instr outputs))
           (t
             (push instr inputs)))))
      (setf inputs (nreverse inputs))
      (setf outputs (nreverse outputs))

      ;; Process input instructions using matrix algorithm (matches Python exactly)
      (dolist (instr inputs)
        (let* ((opcode (mblogic-cl:parsed-opcode instr))
               (cell (instruction-to-cell instr 0)))

          ;; Collect addresses
          (dolist (addr (ladder-cell-addresses cell))
            (pushnew addr all-addresses :test #'string-equal))

          (cond
           ;; STR instruction - ALWAYS push current and start new (matches Python)
           ((store-instruction-p opcode)
             (push current-matrix matrix-stack)
             (setf current-matrix (list (list)))
             (setf current-matrix (append-cell-to-matrix cell current-matrix)))

           ;; AND instruction - append to current row
           ((and-instruction-p opcode)
             (setf current-matrix (append-cell-to-matrix cell current-matrix)))

           ;; OR instruction - create parallel branch below, then close block
           ;; Matches Python: merge below, then add right-side closing connectors
           ((or-instruction-p opcode)
             (let ((new-matrix (list (list))))
               (setf new-matrix (append-cell-to-matrix cell new-matrix))
               (setf current-matrix (merge-matrix-below current-matrix new-matrix))
               (setf current-matrix (close-branch-block current-matrix))))

           ;; ORSTR - pop and merge below, then add right-side closing connectors
           ;; Matches Python: merge below, then close-branch-block
           ((orstr-instruction-p opcode)
             (when matrix-stack
                   (let ((old-matrix (pop matrix-stack)))
                     (setf current-matrix (merge-matrix-below old-matrix current-matrix))
                     (setf current-matrix (close-branch-block current-matrix)))))

           ;; ANDSTR - pop and merge right with left-side connectors
           ((andstr-instruction-p opcode)
             (when matrix-stack
                   (let ((old-matrix (pop matrix-stack)))
                     (setf current-matrix (merge-matrix-right old-matrix current-matrix)))))

           ;; Other instructions (comparisons, etc.) - treat as AND
           (t
             (setf current-matrix (append-cell-to-matrix cell current-matrix))))))

      ;; Handle multi-row rungs (double/triple) vs single-row rungs
      ;; Matches Python: _InputsToMatrixSingle for single, _InputsToMatrixMulti for multi
      (let ((stack-len (length matrix-stack)))

        (cond
         ;; Single-row rung: stack should have exactly 1 entry (initial empty matrix)
         ((= stack-len 1)
           ;; Nothing to do - current-matrix already contains the result
           nil)

         ;; Double rung (2 parallel inputs): stack has 2 entries
         ;; In CL, push adds to FRONT, so stack[0] is most recent (opposite of Python append)
         ((= stack-len 2)
           (let ((last-matrix (nth 0 matrix-stack))) ; Most recent push (X1)
             (setf current-matrix (append last-matrix current-matrix))))

         ;; Triple rung (3 parallel inputs): stack has 3 entries  
         ;; stack[0] = X2, stack[1] = X1, stack[2] = initial empty
         ((= stack-len 3)
           (let ((x2-matrix (nth 0 matrix-stack)) ; X2 (pushed by STR X2)
                                                 (x1-matrix (nth 1 matrix-stack))) ; X1 (pushed by STR X1)
             (setf current-matrix (append (append x1-matrix x2-matrix) current-matrix))))

         ;; Any other stack size is invalid
         (t
           (warn "Invalid IL program structure: matrix stack has ~D entries" stack-len))))

      ;; Process output instructions
      ;; First pass: collect all output cells with sequential row numbers
      (let ((output-row 0))
        (dolist (instr outputs)
          (let* ((opcode (mblogic-cl:parsed-opcode instr))
                 (params (mblogic-cl:parsed-params instr)))
            (cond
             ;; Coil with potentially multiple addresses
             ((coil-instruction-p opcode)
               ;; Check if this is a range address (2 params for coil = range)
               (let ((is-range (> (length params) 1)))
                 (dolist (addr params)
                   (when (mblogic-cl:bool-addr-p addr)
                         ;; Column 1 to leave room for branch connector at column 0
                         (let ((cell (make-coil-cell opcode addr 1 output-row is-range)))
                           (push cell output-cells)
                           (pushnew addr all-addresses :test #'string-equal)
                           (incf output-row))))))
             ;; Output block instructions (TMR, CNTU, COPY, MATHDEC, etc.)
             ((output-block-instruction-p opcode)
               (let* ((instr-def (mblogic-cl:parsed-instruction-def instr))
                      (ladsymb (when instr-def (mblogic-cl:instruction-ladsymb instr-def)))
                      (svg-symbol (ladsymb-to-svg-symbol ladsymb opcode))
                      ;; All params go into addresses (matches demodata.js format)
                      (addr-list (if params (copy-list params) (list "")))
                      (cell (make-ladder-cell
                             :type :coil
                             :symbol svg-symbol
                             :address (first params)
                             :addresses addr-list
                             :opcode opcode
                             :params params
                             :row output-row
                             :col 1
                             :monitor-type (when instr-def
                                                 (mblogic-cl:instruction-monitor instr-def)))))
                 (push cell output-cells)
                 ;; Track PLC addresses for monitoring
                 (dolist (p params)
                   (when (mblogic-cl:any-addr-p p)
                         (pushnew p all-addresses :test #'string-equal)))
                 (incf output-row)))
             ;; Control instructions (END, RT, etc.)
             (t
               (let ((cell (instruction-to-cell instr 0)))
                 (setf (ladder-cell-row cell) output-row)
                 (setf (ladder-cell-col cell) 1)
                 (push cell output-cells)
                 (dolist (addr (ladder-cell-addresses cell))
                   (pushnew addr all-addresses :test #'string-equal))
                 (incf output-row)))))

          ;; Note: JS handles parallel output rendering automatically based on
          ;; having multiple outputeditN entries - no branch connectors needed
        )

        ;; Convert matrix to flat cell list with correct row/col positions
        ;; Fill nil cells with hbar for horizontal wire connections
        (let ((input-cells nil)
              (matrix-height (matrix-height current-matrix))
              (matrix-width (matrix-width current-matrix)))
          (loop for row in current-matrix
                for row-idx from 0
                do (loop for cell in row
                         for col-idx from 0
                         do (cond
                             ;; Real cell - set position
                             (cell
                               (setf (ladder-cell-row cell) row-idx)
                               (setf (ladder-cell-col cell) col-idx)
                               (push cell input-cells))
                             ;; Nil in row 0 - fill with hbar for wire continuity
                             ((= row-idx 0)
                               (let ((hbar (make-hbar-cell :row row-idx :col col-idx)))
                                 (push hbar input-cells)))
                             ;; Nil in branch row - complex rules:
                             ;; 1. Don't fill gaps between right-side (└├┌) and left-side (┘┤┐) connectors
                             ;; 2. Fill gaps between non-branch cells for wire continuity
                             ((let ((has-earlier-non-branch nil)
                                    (has-later-cell nil)
                                    (has-right-connector-before nil)
                                    (has-left-connector-after nil))
                                ;; Check earlier cells
                                (loop for earlier-col from 0 below col-idx
                                      for earlier-cell = (nth earlier-col row)
                                      when earlier-cell
                                      do (let ((sym (ladder-cell-symbol earlier-cell)))
                                           (when (not (branch-symbol-p sym))
                                             (setf has-earlier-non-branch t))
                                           (when (member sym (list *branch-l* *branch-tl* *branch-ttl*)
                                                         :test #'string-equal)
                                             (setf has-right-connector-before t))))
                                ;; Check later cells
                                (loop for later-col from (1+ col-idx) below (length row)
                                      for later-cell = (nth later-col row)
                                      when later-cell
                                      do (setf has-later-cell t)
                                         (let ((sym (ladder-cell-symbol later-cell)))
                                           (when (member sym (list *branch-r* *branch-tr* *branch-ttr*)
                                                         :test #'string-equal)
                                             (setf has-left-connector-after t))))
                                ;; Fill if: has earlier non-branch AND later cell AND NOT in gap
                                (and has-earlier-non-branch
                                     has-later-cell
                                     (not (and has-right-connector-before has-left-connector-after))))
                               (let ((hbar (make-hbar-cell :row row-idx :col col-idx)))
                                 (push hbar input-cells))))))

          ;; Build the rung (no longer needs branch metadata - it's in the cells)
          (make-ladder-rung
           :number (mblogic-cl:network-number network)
           :cells (append (nreverse input-cells) (nreverse output-cells))
           :rows (max matrix-height (length output-cells) 1)
           :cols matrix-width
           :addresses (nreverse all-addresses)
           :comment (first (mblogic-cl:network-comments network))
           :branches nil ; No longer needed - explicit cells
           :output-branches nil)))))) ; No longer needed - explicit cells

(defun fix-orphaned-fork-connectors (matrix)
  "Post-process matrix to fix orphaned fork connectors from nested branches.
   Now that branch connector symbols use JS-compatible names (brancht instead of
   branchttr), this function is a no-op. Kept for API compatibility.
   Returns the unmodified matrix."
  matrix)

;;; ============================================================
;;; Program/Subroutine to Ladder Conversion
;;; ============================================================

(defun networks-to-ladder (networks name)
  "Convert a list of networks to a ladder program structure"
  (let ((rungs nil)
        (all-addresses nil))

    (dolist (net networks)
      (let ((rung (network-to-ladder-rung net)))
        (push rung rungs)
        (dolist (addr (ladder-rung-addresses rung))
          (pushnew addr all-addresses :test #'string-equal))))

    (make-ladder-program
     :name name
     :rungs (nreverse rungs)
     :addresses (sort (copy-list all-addresses) #'string<))))

(defun program-to-ladder (parsed-program &optional (name "main"))
  "Convert a parsed program to ladder diagram structure.
   NAME specifies which subroutine (or 'main' for main program)."
  (if (string-equal name "main")
      (networks-to-ladder (mblogic-cl:program-main-networks parsed-program) "main")
      (let ((sbr (gethash name (mblogic-cl:program-subroutines parsed-program))))
        (when sbr
              (networks-to-ladder (mblogic-cl:subroutine-networks sbr) name)))))

(defun list-subroutine-names (parsed-program)
  "Return list of all subroutine names in a parsed program"
  (let ((names '("main")))
    (maphash (lambda (name sbr)
               (declare (ignore sbr))
               (push name names))
             (mblogic-cl:program-subroutines parsed-program))
    (sort names #'string<)))

;;; ============================================================
;;; Cell to JSON-ready Plist Conversion (Python-compatible matrixdata format)
;;; ============================================================

(defun format-monitor-info (cell)
  "Format monitor info for a cell in Python-compatible format.
   Returns a list like (\"bool\" \"X1\") or (\"none\")."
  (let ((monitor-type (ladder-cell-monitor-type cell))
        (addr (ladder-cell-address cell)))
    (cond
     ;; Boolean monitoring
     ((and (eq monitor-type :bool) addr)
       (list "bool" addr))
     ;; No monitoring (for branch connectors, etc.)
     (t
       (list "none")))))

(defun cell-to-matrixdata (cell)
  "Convert a ladder cell to Python-compatible matrixdata format.
   Format: {type, row, col, addr, value, monitor}"
  (list :type (if (member (ladder-cell-type cell) '(:coil :output-branch :control))
                  "outp"
                  "inp")
        :row (ladder-cell-row cell)
        :col (ladder-cell-col cell)
        :addr (let ((addrs (ladder-cell-addresses cell)))
                (if addrs addrs (list ""))) ; Empty string in list for no addresses
        :value (ladder-cell-symbol cell)
        :monitor (format-monitor-info cell)))

;; Keep old function for backwards compatibility during transition
(defun cell-to-plist (cell)
  "Convert a ladder cell to a plist for JSON serialization (legacy format)"
  (list :type (string-downcase (symbol-name (ladder-cell-type cell)))
        :symbol (ladder-cell-symbol cell)
        :addr (ladder-cell-address cell)
        :addrs (ladder-cell-addresses cell)
        :opcode (ladder-cell-opcode cell)
        :params (ladder-cell-params cell)
        :row (ladder-cell-row cell)
        :col (ladder-cell-col cell)
        :monitor (when (ladder-cell-monitor-type cell)
                       (string-downcase (symbol-name (ladder-cell-monitor-type cell))))))

(defun rung-to-matrixdata (rung)
  "Convert a ladder rung to Python-compatible format.
   Format: {rungnum, rungtype, comment, ildata, matrixdata}"
  (list :rungnum (ladder-rung-number rung)
        :rungtype (determine-rungtype rung)
        :comment (or (ladder-rung-comment rung) "")
        :ildata #() ; TODO: capture original IL if needed
        :matrixdata (mapcar #'cell-to-matrixdata (ladder-rung-cells rung))))

;; Keep old function for backwards compatibility
(defun rung-to-plist (rung)
  "Convert a ladder rung to a plist for JSON serialization (legacy format)"
  (list :rungnum (ladder-rung-number rung)
        :rows (ladder-rung-rows rung)
        :cols (ladder-rung-cols rung)
        :comment (ladder-rung-comment rung)
        :addrs (ladder-rung-addresses rung)
        :cells (mapcar #'cell-to-plist (ladder-rung-cells rung))
        :branches nil ; No longer used
        :output-branches nil)) ; No longer used

(defun ladder-program-to-matrixdata (ladder-prog)
  "Convert a ladder program to Python-compatible format.
   Format: {subrname: {subrdata, subrcomments, signature}}"
  (list :subrdata (mapcar #'rung-to-matrixdata (ladder-program-rungs ladder-prog))
        :subrcomments ""
        :signature "")) ; TODO: compute MD5 hash if needed

;; Keep old function for backwards compatibility
(defun ladder-program-to-plist (ladder-prog)
  "Convert a ladder program to a plist for JSON serialization (legacy format)"
  (list :subrname (ladder-program-name ladder-prog)
        :addresses (ladder-program-addresses ladder-prog)
        :subrdata (mapcar #'rung-to-plist (ladder-program-rungs ladder-prog))))

;;; ============================================================
;;; JavaScript-Compatible Format Conversion (demodata.js format)
;;; ============================================================
;;; These functions produce output compatible with the MBLogic ladtest
;;; JavaScript files (ladsubrdisplib.js, ladeditlib.js, etc.)

#| (defun cl-symbol-to-js-symbol (symbol)
  "Map CL branch symbols to JavaScript-compatible symbol names.
   JS only has: brancht, branchl, branchr, branchtl, branchtr, branchtu, branchx, vbar, hbar"
  (cond
    ((null symbol) "")
    ;; Top corners (fork start or merge end)
    ((string-equal symbol "branchttr") "brancht")  ; Top-right fork start: ┐
    ((string-equal symbol "branchttl") "brancht")  ; Top-left merge end: ┌
    ;; Middle T-junctions
    ((string-equal symbol "branchtr") "branchtr")  ; Middle-right fork: ┤ (pass through)
    ((string-equal symbol "branchtl") "branchtl")  ; Middle-left merge: ├ (pass through)
    ;; Bottom corners (fork end or merge start)
    ((string-equal symbol "branchr") "branchr")    ; Bottom-right fork end: ┘ (pass through)
    ((string-equal symbol "branchl") "branchl")    ; Bottom-left merge start: └ (pass through)
    ;; Special junctions
    ((string-equal symbol "branchtu") "branchtu")  ; Top continuation from right: ┴
    ((string-equal symbol "branchx") "branchx")    ; Cross junction: ╳
    ;; Vertical bars (no junction)
    ((string-equal symbol "vbarr") "vbar")
    ((string-equal symbol "vbarl") "vbar")
    (t symbol)))  ; All others pass through unchanged |#

(defun cl-symbol-to-js-symbol (symbol)
  "Map CL branch symbols to JavaScript-compatible symbol names.
   JS only has: brancht, branchl, branchr, branchtl, branchtr, branchtu, branchx, vbar, hbar"
  (cond
   ((null symbol) "")
   ;; Top corners (fork start or merge end)
   ((string-equal symbol "branchttr") "branchttr") ; Top-right fork start: ┐
   ((string-equal symbol "branchttl") "branchttl") ; Top-left merge end: ┌
   ;; Middle T-junctions
   ((string-equal symbol "branchtr") "branchtr") ; Middle-right fork: ┤ (pass through)
   ((string-equal symbol "branchtl") "branchtl") ; Middle-left merge: ├ (pass through)
   ;; Bottom corners (fork end or merge start)
   ((string-equal symbol "branchr") "branchr") ; Bottom-right fork end: ┘ (pass through)
   ((string-equal symbol "branchl") "branchl") ; Bottom-left merge start: └ (pass through)
   ;; Special junctions
   ((string-equal symbol "branchtu") "branchtu") ; Top continuation from right: ┴
   ((string-equal symbol "branchx") "branchx") ; Cross junction: ╳
   ;; Vertical bars (no junction)
   ((string-equal symbol "vbarr") "vbar")
   ((string-equal symbol "vbarl") "vbar")
   (t symbol))) ; All others pass through unchanged    

(defun cell-to-js-format (cell)
  "Convert ladder cell to JS format as alist: ((value . \"noc\") (addr . (\"X1\")))"
  (let ((symbol (ladder-cell-symbol cell)) ; Use symbol directly - all symbols now defined in JS ladsymbols.js
                                          (addrs (ladder-cell-addresses cell)))
    (list (cons :value symbol)
          (cons :addr (if addrs addrs (list "")))))) ; Branch symbols get [""]

(defun determine-rungtype (rung)
  "Determine rungtype based on output block instruction type.
   The rungtype is driven by the output block instruction (if any):
   - CNTU/CNTD/TMRA → double (2 input rows)
   - UDC/SHFRG → triple (3 input rows)
   - Everything else → single
   Verifies the actual input row count fits the constraint."
  (let ((cells (ladder-rung-cells rung)))
    (if (null cells)
        "empty"
        ;; Find the first output block instruction's opcode
        (let ((block-opcode nil)
              (max-input-row 0))
          (dolist (cell cells)
            (let ((opcode (ladder-cell-opcode cell)))
              (cond
               ;; Track input rows (non-output cells)
               ((not (member (ladder-cell-type cell) '(:coil :control :output-branch)))
                 (setf max-input-row (max max-input-row (ladder-cell-row cell))))
               ;; Find first output block instruction
               ((and (null block-opcode)
                     opcode
                     (output-block-instruction-p opcode))
                 (setf block-opcode opcode)))))
          (if block-opcode
              ;; Get base rungtype from the output block instruction
              (let ((base-type (rungtype-for-output block-opcode)))
                ;; Verify input row count fits the constraint
                (cond
                 ((and (string= base-type "triple") (<= max-input-row 2)) "triple")
                 ((and (string= base-type "double") (<= max-input-row 1)) "double")
                 ((string= base-type "single") "single")
                 ;; Fall back to single if rows overflow
                 (t "single")))
              ;; No output block instruction → always single
              "single")))))

(defun rung-to-js-format (rung rung-index)
  "Convert rung to JS format with matrixdata as object (not array).
   Keys are 'inputeditRC' for input cells and 'outputeditN' for output cells.
   Returns an alist for proper JSON encoding.
   Note: Filters cells to match JavaScript constraints (max 3 input rows, 8 output rows)."
  (let ((matrixdata-alist nil))
    ;; First determine rungtype to know the constraints
    (let* ((rungtype (determine-rungtype rung))
           ;; JavaScript constraints based on MatrixParams in ladsymbols.js
           (max-input-row (cond ((string= rungtype "single") 7)
                                ((string= rungtype "double") 1)
                                ((string= rungtype "triple") 2)
                                (t 7)))
           (max-output-row (cond ((string= rungtype "single") 7)
                                 ((string= rungtype "double") 0)
                                 ((string= rungtype "triple") 0)
                                 (t 7)))
           (max-input-col 7)) ; JS key format only supports single-digit columns (0-7)

      ;; Build matrixdata as alist with inputeditRC/outputeditN keys
      ;; Filter cells to only those within JavaScript constraints
      (dolist (cell (ladder-rung-cells rung))
        (let* ((type (ladder-cell-type cell))
               (row (ladder-cell-row cell))
               (col (ladder-cell-col cell)))
          ;; Only include cells within JavaScript's supported matrix dimensions
          (when (if (member type '(:coil :control :output-branch))
                    (<= row max-output-row)
                    (and (<= row max-input-row) (<= col max-input-col)))
                (let ((key (if (member type '(:coil :control :output-branch))
                               (format nil "outputedit~D" row)
                               (format nil "inputedit~D~D" row col))))
                  (push (cons key (cell-to-js-format cell)) matrixdata-alist)))))

      ;; Return rung structure as alist for proper JSON encoding
      ;; Include reference field for JavaScript rung tracking (required by ladsubrdata.js)
      (list (cons :matrixdata (nreverse matrixdata-alist))
            (cons :rungtype rungtype)
            (cons :ildata ()) ; Empty IL data array (required by JavaScript)
            (cons :comment (or (ladder-rung-comment rung) ""))
            (cons :reference rung-index)))))

(defun ladder-program-to-js-format (ladder-prog)
  "Convert ladder program to full JS-compatible format for demodata.js.
   Returns an alist for proper JSON encoding."
  (let ((rungs-js nil)
        (idx 0))
    (dolist (rung (ladder-program-rungs ladder-prog))
      (push (rung-to-js-format rung idx) rungs-js)
      (incf idx))
    (list (cons :subroutinename (ladder-program-name ladder-prog))
          (cons :subrcomments "")
          (cons :signature 0)
          (cons :rungdata (nreverse rungs-js)))))

;;; End of ladder-render.lisp
