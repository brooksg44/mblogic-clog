# How `parser.lisp` and `ladder-render.lisp` Work Together

## Overview

These two files form the core data pipeline for turning a text IL (Instruction List) program into a visual ladder diagram. The flow is strictly one-way:

```
IL text file
    ↓  parse-il-file (parser.lisp)
parsed-program
    ↓  program-to-ladder (ladder-render.lisp)
ladder-program
    ↓  ladder-program-to-js-format (ladder-render.lisp)
JSON sent to browser
```

The packages are separate: `parser.lisp` lives in `#:mblogic-clog`; `ladder-render.lisp` lives in `#:mblogic-clog-web`. The renderer reaches into the parser's package by prefixing symbols: `mblogic-clog:parsed-opcode`, `mblogic-clog:network-instructions`, etc.

---

## Stage 1 — Parsing (`parser.lisp`)

### What it produces

Three nested CLOS classes that mirror the source file's structure:

```
parsed-program
  ├── main-networks  → list of parsed-network
  └── subroutines    → hash-table name → parsed-subroutine
                           └── networks → list of parsed-network
                                              └── instructions → list of parsed-instruction
```

### Key classes

| Class | Slots you care about |
|---|---|
| `parsed-instruction` | `parsed-opcode` (string), `parsed-params` (list of strings), `parsed-instruction-def` (lookup result), `parsed-comment` |
| `parsed-network` | `network-number` (integer), `network-instructions` (list of `parsed-instruction`), `network-comments` |
| `parsed-subroutine` | `subroutine-name`, `subroutine-networks` |
| `parsed-program` | `program-main-networks`, `program-subroutines` (hash-table), `program-errors`, `program-warnings` |

### Entry points

```lisp
;; From a file:
(mblogic-clog:parse-il-file "path/to/prog.txt")  ; → parsed-program

;; From a string (useful for tests):
(mblogic-clog:parse-il-string "NETWORK 1\nSTR X1\nOUT Y1")  ; → parsed-program
```

### What `parsed-instruction-def` is

Each `parsed-instruction` holds a back-reference to the instruction definition found by `find-instruction`. The renderer uses two slots from this definition:

- `mblogic-clog:instruction-ladsymb` — a keyword like `:contact-no`, `:coil`, `:timer`
- `mblogic-clog:instruction-monitor` — a keyword like `:bool`, `:timer`, `:counter`

If `parsed-instruction-def` is `nil` the opcode was not recognised; the parser records an error.

---

## Stage 2 — Rendering (`ladder-render.lisp`)

### What it produces

Three structs that represent the visual layout:

```
ladder-program
  └── rungs → list of ladder-rung
                   └── cells → list of ladder-cell
```

### Key structs

| Struct | Slots you care about |
|---|---|
| `ladder-cell` | `type` (`:contact :coil :block :branch …`), `symbol` (SVG name string), `address`, `addresses`, `row`, `col`, `monitor-type` |
| `ladder-rung` | `number`, `cells`, `rows`, `cols`, `addresses`, `comment` |
| `ladder-program` | `name`, `rungs`, `addresses` |

### Entry points

```lisp
;; Convert a whole parsed-program (main or named subroutine):
(mblogic-clog-web:program-to-ladder parsed-program "main")    ; → ladder-program
(mblogic-clog-web:program-to-ladder parsed-program "SUBR1")   ; → ladder-program

;; Then serialise for the browser:
(mblogic-clog-web:ladder-program-to-js-format ladder-prog)    ; → alist → JSON
```

---

## The Connection Points

The renderer reads exactly four things from the parser's objects:

### 1. Traversing the program structure

```lisp
;; In program-to-ladder:
(mblogic-clog:program-main-networks parsed-program)
(mblogic-clog:program-subroutines  parsed-program)  ; hash-table
(mblogic-clog:subroutine-networks  sbr)
```

### 2. Reading each network

```lisp
;; In network-to-ladder-rung:
(mblogic-clog:network-instructions network)  ; → list of parsed-instruction
(mblogic-clog:network-number       network)
(mblogic-clog:network-comments     network)
```

### 3. Reading each instruction

```lisp
;; In instruction-to-cell and extract-addresses:
(mblogic-clog:parsed-opcode          instr)   ; "STR", "AND", "OUT", ...
(mblogic-clog:parsed-params          instr)   ; '("X1") or '("T1" "100")
(mblogic-clog:parsed-instruction-def instr)   ; → instruction definition object
```

### 4. Reading the instruction definition

```lisp
;; In instruction-to-cell and get-monitor-type:
(mblogic-clog:instruction-ladsymb instr-def)  ; :contact-no, :coil, :timer, ...
(mblogic-clog:instruction-monitor instr-def)  ; :bool, :timer, :counter, nil
```

---

## The Matrix Algorithm (the hard part)

`network-to-ladder-rung` converts a flat list of `parsed-instruction` objects into a 2-D cell matrix. The matrix is a list of rows: `((row0-cells) (row1-cells) …)`.

The algorithm mirrors the original Python `PLCLadder.py`:

| IL opcode | Matrix operation |
|---|---|
| `STR` | Push current matrix onto `matrix-stack`, start a fresh one-row matrix, append this cell |
| `AND` | Append cell to row 0 of current matrix |
| `OR` | Create a new one-row matrix with this cell, `merge-matrix-below`, then `close-branch-block` (adds right-side connectors ┐┤┘) |
| `ORSTR` | Pop stack, `merge-matrix-below`, then `close-branch-block` |
| `ANDSTR` | Pop stack, `merge-matrix-right` (inserts left-side connectors ┌├└) |
| Coils/blocks | Collected separately, placed in output columns after inputs |

`close-branch-block` and `merge-matrix-right` insert explicit `ladder-cell` objects with branch connector symbols (`branchttr`, `branchtl`, `branchr`, etc.) directly into the matrix. No post-processing is needed; the cells carry all the shape information the browser needs.

---

## The Symbol Translation Chain

An instruction's visual appearance flows through three transformations:

```
parsed-instruction-def
    .instruction-ladsymb  →  :contact-no
         ↓  ladsymb-to-svg-symbol
    SVG symbol string     →  "noc"  (or opcode-specific: "compeq", "tmra", …)
         ↓  cell-to-js-format / cl-symbol-to-js-symbol
    JS symbol string      →  "noc"  (branch symbols get remapped: "vbarr" → "vbar")
```

The final JS symbol string is what the browser's `ladsymbols.js` uses to pick the right SVG graphic.

---

## JSON Output Shape

`ladder-program-to-js-format` returns a nested alist. The critical inner structure per rung is:

```
{
  "rungtype": "single" | "double" | "triple",
  "comment":  "...",
  "reference": 0,
  "ildata":   [],
  "matrixdata": {
    "inputedit00":  { "value": "noc",  "addr": ["X1"] },
    "inputedit01":  { "value": "noc",  "addr": ["X2"] },
    "outputedit0":  { "value": "out",  "addr": ["Y1"] }
  }
}
```

- Input cells use keys `inputeditRC` where R = row, C = col.
- Output cells use keys `outputeditN` where N = output row.
- `rungtype` tells the JS renderer how many input rows to expect (`single`=1, `double`=2, `triple`=3).

---

## Tracing a Single Rung End-to-End

Given this IL text:

```
NETWORK 1
STR X1
AND X2
OUT Y1
```

1. **Parser** produces one `parsed-network` with three `parsed-instruction` objects: STR/X1, AND/X2, OUT/Y1.
2. **`network-to-ladder-rung`** separates inputs (STR, AND) from outputs (OUT).
3. STR X1 → push empty matrix, start new matrix, append contact cell at (row=0, col=0).
4. AND X2 → append contact cell at (row=0, col=1).
5. OUT Y1 → coil cell placed at (row=0, col=1) in the output list.
6. Matrix flattens to a cell list; row/col positions are stamped on each cell.
7. **`rung-to-js-format`** emits keys `inputedit00`, `inputedit01`, `outputedit0`.
