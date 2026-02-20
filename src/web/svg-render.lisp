;;;; src/web/svg-render.lisp
;;;;
;;;; Server-side SVG generation for ladder diagram display.
;;;; Generates SVG strings from ladder-rung data structures,
;;;; referencing SVG prototype symbols defined in static/laddersvgdefs.html.

(in-package #:mblogic-clog-web)

;;; ============================================================
;;; SVG Prototype Loading
;;; ============================================================

(defvar *svg-prototypes-html* nil
  "Cached HTML string containing the SVG prototype definitions.")

(defun load-svg-prototypes (static-directory)
  "Load the SVG prototype HTML from static/laddersvgdefs.html.
   Sets *svg-prototypes-html* and returns the string."
  (let ((path (merge-pathnames "laddersvgdefs.html" static-directory)))
    (if (probe-file path)
        (setf *svg-prototypes-html* (uiop:read-file-string path))
        (progn
          (warn "SVG prototypes file not found: ~A" path)
          (setf *svg-prototypes-html* "<!-- SVG prototypes not found -->")))
    *svg-prototypes-html*))

(defun get-svg-prototypes-html ()
  "Return the SVG prototype HTML string."
  (or *svg-prototypes-html* "<!-- SVG prototypes not loaded -->"))

;;; ============================================================
;;; Symbol ID Mapping
;;; ============================================================

;;; Maps ladder-render.lisp symbol names to HTML SVG prototype IDs.
;;; The HTML prototypes are defined in static/laddersvgdefs.html.
(defparameter *svg-id-map*
  '(;; Input contacts
    ("noc"      . "inputno")
    ("ncc"      . "inputnc")
    ("nocpd"    . "inputnopd")
    ("nocnd"    . "inputnond")
    ("nccpd"    . "inputnopd")   ; No NC-edge prototype; reuse NO-edge shape
    ("nccnd"    . "inputnond")   ; No NC-edge prototype; reuse NO-edge shape
    ;; Comparison contacts
    ("compeq"   . "inputcompeq")
    ("compneq"  . "inputcompneq")
    ("compgt"   . "inputcompgt")
    ("complt"   . "inputcomplt")
    ("compge"   . "inputcompge")
    ("comple"   . "inputcomple")
    ;; Output coils
    ("out"      . "outputout")
    ("set"      . "outputset")
    ("rst"      . "outputreset")
    ("pd"       . "outputpd")
    ;; Range address output coils (same prototypes as single coils)
    ("out2"     . "outputout")
    ("set2"     . "outputset")
    ("rst2"     . "outputreset")
    ("pd2"      . "outputpd")
    ;; Timers
    ("tmr"      . "timertmr")
    ("tmra"     . "timertmra")
    ("tmroff"   . "timertmroff")
    ;; Counters
    ("cntu"     . "timercntu")
    ("cntd"     . "timercntd")
    ("udc"      . "timerudc")
    ;; Program control
    ("call"     . "progcontrolcall")
    ("rt"       . "progcontrolrt")
    ("rtc"      . "progcontrolrtc")
    ("end"      . "progcontrolend")
    ("endc"     . "progcontrolendc")
    ("for"      . "progcontrolfor")
    ("next"     . "progcontrolnext")
    ;; Data operations
    ("copy"     . "copy")
    ("cpyblk"   . "cpyblk")
    ("fill"     . "fill")
    ("pack"     . "pack")
    ("unpack"   . "unpack")
    ("shfrg"    . "shfrg")
    ;; Find/search
    ("findeq"   . "findeq")
    ("findne"   . "findne")
    ("findgt"   . "findgt")
    ("findlt"   . "findlt")
    ("findge"   . "findge")
    ("findle"   . "findle")
    ("findieq"  . "findieq")
    ("findine"  . "findine")
    ("findigt"  . "findigt")
    ("findige"  . "findige")
    ("findilt"  . "findilt")
    ("findile"  . "findile")
    ;; Math/sum
    ("mathdec"  . "mathdec")
    ("mathhex"  . "mathhex")
    ("sum"      . "sum")
    ;; Branch connectors (names already match HTML IDs)
    ("branchttr" . "branchttr")
    ("branchtr"  . "branchtr")
    ("branchr"   . "branchr")
    ("vbarr"     . "vbarr")
    ("branchttl" . "branchttl")
    ("branchtl"  . "branchtl")
    ("branchl"   . "branchl")
    ("vbarl"     . "vbarl")
    ("hbar"      . "hbar")
    ;; Empty / fallback
    ("empty"    . "none")
    ("il"       . "none"))
  "Mapping from ladder-render symbol names to HTML SVG prototype IDs.")

(defun get-svg-id (symbol-name)
  "Return the HTML SVG prototype ID for a given ladder symbol name.
   Returns nil if no mapping found (cell will render as empty)."
  (when symbol-name
    (cdr (assoc symbol-name *svg-id-map* :test #'string-equal))))

;;; ============================================================
;;; Cell Geometry
;;; ============================================================

(defconstant +cell-w+ 75
  "Width of a standard cell (contact/coil) in pixels.")

(defconstant +cell-h+ 75
  "Height step between rows in pixels.")

(defconstant +addr-margin+ 20
  "Pixels above the wire for address text.")

(defconstant +sym-top+ 35
  "Pixels above the wire that symbol occupies (bounding rect y=-35).")

(defconstant +sym-bot+ 35
  "Pixels below the wire that symbol occupies (bounding rect y+35).")

(defun cell-x (col)
  "X coordinate of the wire entry point for cell at COL."
  (* col +cell-w+))

(defun cell-y (row)
  "Y coordinate of the wire level for cell at ROW."
  (* row +cell-h+))

;;; ============================================================
;;; Monitoring CSS Class
;;; ============================================================

(defun monitor-class (cell address-values)
  "Return CSS class string for a cell based on its monitoring state.
   Returns 'MB_ladderon' if the address is true, 'MB_ladderoff' otherwise.
   Returns empty string for cells with no boolean monitoring."
  (let ((monitor-type (ladder-cell-monitor-type cell))
        (addr (ladder-cell-address cell)))
    (if (and (eq monitor-type :bool) addr address-values)
        (let ((entry (gethash addr address-values)))
          (if (and entry (car entry))
              "MB_ladderon"
              "MB_ladderoff"))
        "")))  ; No monitoring class for non-boolean or unmonitored cells

;;; ============================================================
;;; SVG Rung Rendering
;;; ============================================================

(defun rung-max-col (rung)
  "Return the maximum column index used by any cell in RUNG."
  (let ((cells (ladder-rung-cells rung)))
    (if cells
        (reduce #'max cells :key #'ladder-cell-col :initial-value 0)
        0)))

(defun render-rung-svg (rung &optional address-values fixed-width)
  "Generate SVG string for a ladder rung.
   RUNG is a ladder-rung struct.
   ADDRESS-VALUES is an optional hash-table of address -> (value . type).
   FIXED-WIDTH, if provided, overrides the per-rung computed SVG width so
   all rungs in a program share the same canvas width.
   Returns an SVG string with the rung's ladder diagram."
  (let* ((rows (ladder-rung-rows rung))
         (cells (ladder-rung-cells rung))
         ;; Width: use fixed-width if supplied, otherwise compute from this rung's cells
         ;; Output zone is 225px wide (to accommodate wide blocks like COPY, TMR, etc.)
         (svg-width (or fixed-width
                        (+ (* (+ (rung-max-col rung) 1) +cell-w+) 225)))
         ;; Height: account for multi-row blocks (counters, timers, shift registers)
         ;; that are 2-3 rows tall (150px or 225px) instead of the standard 75px
         (block-height-rows (lambda (sym)
                              (cond
                                ((member sym '("cntu" "cntd" "tmra") :test #'string-equal) 2)
                                ((member sym '("udc" "shfrg") :test #'string-equal) 3)
                                (t 1))))
         (max-row-extent (if cells
                             (loop for cell in cells
                                   maximize (+ (ladder-cell-row cell)
                                               (funcall block-height-rows
                                                        (ladder-cell-symbol cell))))
                             1))
         (view-top (- (+ +sym-top+ +addr-margin+)))
         (view-height (+ +addr-margin+ +sym-top+
                         (* (max 1 max-row-extent) +cell-h+)
                         +sym-bot+))
         ;; Column index where the coil zone begins (= contact matrix width)
         (global-max-cols (/ (- svg-width 225) +cell-w+))
         ;; This rung's own contact zone width
         (rung-cols (ladder-rung-cols rung))
         ;; Output-zone cells (coils and control instructions) for wire routing
         (coil-cells (remove-if-not (lambda (c)
                                      (member (ladder-cell-type c) '(:coil :control)))
                                    cells))
         (max-coil-row (if coil-cells
                           (reduce #'max coil-cells :key #'ladder-cell-row :initial-value 0)
                           -1)))
    (with-output-to-string (s)
      ;; SVG element
      (format s "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 ~D ~D ~D\">"
              view-top svg-width view-height)

      ;; Inline styles — ensures correct rendering regardless of external CSS
      (format s "<style>rect{fill:none;stroke:black;stroke-width:1}line{stroke:black;stroke-width:2}circle{fill:black;stroke:black;stroke-width:2}ellipse{fill:none;stroke:black;stroke-width:2}text{font-family:Arial,sans-serif;font-size:12px;fill:black}.MB_ladderon line,.MB_ladderon circle,.MB_ladderon ellipse{stroke:red;stroke-width:2}.MB_ladderoff line,.MB_ladderoff circle,.MB_ladderoff ellipse{stroke:blue;stroke-width:0.5}</style>")

      ;; Left power rail
      (let ((rail-top (- +sym-top+))
            (rail-bot (+ (* (1- (max 1 rows)) +cell-h+) +sym-bot+)))
        (format s "<line x1=\"0\" y1=\"~D\" x2=\"0\" y2=\"~D\" stroke=\"black\" stroke-width=\"3\"/>"
                rail-top rail-bot))

      ;; Wire extension: row-0 wire from end of contact zone to coil zone
      (when (and coil-cells (< rung-cols global-max-cols))
        (format s "<line x1=\"~D\" y1=\"0\" x2=\"~D\" y2=\"0\" stroke=\"black\" stroke-width=\"2\"/>"
                (cell-x rung-cols) (cell-x global-max-cols)))

      ;; Vertical coil bus: connects stacked coils on their left side
      (when (> max-coil-row 0)
        (format s "<line x1=\"~D\" y1=\"0\" x2=\"~D\" y2=\"~D\" stroke=\"black\" stroke-width=\"2\"/>"
                (cell-x global-max-cols) (cell-x global-max-cols) (cell-y max-coil-row)))

      ;; Render each cell
      (dolist (cell cells)
        (let* ((sym (ladder-cell-symbol cell))
               (svg-id (get-svg-id sym))
               (row (ladder-cell-row cell))
               (col (ladder-cell-col cell))
               ;; Output-zone cells (coils + control) render at far-right; others at their column
               (x (if (member (ladder-cell-type cell) '(:coil :control))
                      (cell-x global-max-cols)
                      (cell-x col)))
               (y (cell-y row))
               (css-class (if address-values (monitor-class cell address-values) ""))
               (addr (ladder-cell-address cell))
               (addrs (ladder-cell-addresses cell)))

          ;; Render the symbol via <use> with optional monitoring class
          (when svg-id
            (if (string/= css-class "")
                (format s "<g class=\"~A\"><use href=\"#~A\" transform=\"translate(~D,~D)\"/></g>"
                        css-class svg-id x y)
                (format s "<use href=\"#~A\" transform=\"translate(~D,~D)\"/>"
                        svg-id x y)))

          ;; Address text above the cell (for contacts and coils)
          (let ((display-addr (or addr (first addrs))))
            (when display-addr
              (format s "<text x=\"~D\" y=\"~D\" text-anchor=\"middle\" font-size=\"12\" font-family=\"Arial\">~A</text>"
                      (+ x 37) (- y +addr-margin+) (escape-svg-text display-addr))))))

      (format s "</svg>"))))

(defun escape-svg-text (text)
  "Escape special characters for SVG text content."
  (when text
    (with-output-to-string (s)
      (loop for ch across text do
        (case ch
          (#\& (write-string "&amp;" s))
          (#\< (write-string "&lt;" s))
          (#\> (write-string "&gt;" s))
          (t (write-char ch s)))))))

;;; ============================================================
;;; Full Program SVG
;;; ============================================================

(defun render-ladder-program-html (ladder-prog &optional address-values)
  "Generate HTML string for all rungs in a ladder program.
   Each rung gets a rung-number label, optional comment, and SVG diagram."
  (when ladder-prog
    (with-output-to-string (s)
      (let ((rungs (ladder-program-rungs ladder-prog)))
        (if (null rungs)
            (format s "<p>No rungs in this subroutine.</p>")
            ;; Compute uniform width from the widest rung in the program
            (let* ((max-col (reduce #'max rungs :key #'rung-max-col :initial-value 0))
                   (uniform-width (+ (* (+ max-col 1) +cell-w+) 225)))
              (dolist (rung rungs)
                ;; Rung number header
                (format s "<p class=\"rungnumberarea\">Rung ~D</p>"
                        (ladder-rung-number rung))
                ;; Comment (if any)
                (let ((comment (ladder-rung-comment rung)))
                  (when (and comment (> (length comment) 0))
                    (format s "<p class=\"commentarea\">~A</p>"
                            (escape-svg-text comment))))
                ;; Rung SVG — all at uniform width
                (write-string (render-rung-svg rung address-values uniform-width) s))))))))

;;; End of svg-render.lisp
