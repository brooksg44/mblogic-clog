# MBLogic-CLOG

A Common Lisp soft PLC (Programmable Logic Controller) with a live ladder diagram monitor built on [CLOG](https://github.com/rabbibotton/clog) (Common Lisp Omnificent GUI).

This is a port of the [MBLogic](http://mblogic.sourceforge.net/) PLC system, originally written in Python, reimplemented in Common Lisp with a WebSocket-based web interface replacing the original static HTML/JavaScript frontend.

## Features

- **Instruction List (IL) compiler and interpreter** — parses and executes IEC 61131-3 IL programs
- **Ladder diagram visualization** — server-side SVG rendering of ladder logic rungs
- **Live monitoring** — real-time display of PLC address states with color-coded contacts and coils (red = energized, blue = de-energized)
- **PLC control** — Start, Stop, and Single Scan buttons in the browser
- **Multiple subroutines** — dropdown selector to view any subroutine in the program
- **CLOG-based UI** — all page content is built server-side over WebSocket; no static HTML/JS required for the interface

## Requirements

- [SBCL](http://www.sbcl.org/) (Steel Bank Common Lisp)
- [Quicklisp](https://www.quicklisp.org/)
- CLOG and its dependencies (loaded automatically via Quicklisp)

## Quick Start

### Run with the demo program

```bash
sbcl --load start-web-server-demo.lisp
```

Then open http://localhost:8080 in your browser.

### Run with your own IL program

Edit `start-web-server.lisp` to point to your IL file, then:

```bash
sbcl --load start-web-server.lisp
```

## Project Structure

```
mblogic-clog.asd          — ASDF system definitions
start-web-server.lisp     — Startup script (test program)
start-web-server-demo.lisp — Startup script (demo program)
src/
  package.lisp            — mblogic-cl package definition
  data-table.lisp         — PLC data table (booleans, words, floats)
  instructions.lisp       — IL instruction set
  parser.lisp             — IL source file parser
  compiler.lisp           — IL to Lisp compiler
  interpreter.lisp        — Scan-based PLC interpreter
  math-lib.lisp           — Math expression support
  timer-counter.lisp      — Timer and counter instructions
  table-ops.lisp          — Block data operations
  web/
    package.lisp          — mblogic-clog-web package definition
    ladder-render.lisp    — IL → ladder diagram data structures
    plc-data.lisp         — PLC data access functions
    svg-render.lisp       — Ladder structs → SVG strings
    server.lisp           — CLOG web server and page builder
static/
  css/ladder.css          — Ladder display styles
  laddersvgdefs.html      — SVG symbol prototype library
test/
  demodataprog.txt        — Demo IL program
  plcprog.txt             — Test IL program
  test-*.lisp             — Test suites
```

## IL Program Example

```
NETWORK 1
(* Simple motor start/stop *)
LD   X1        (* Start button *)
OR   Y1        (* Seal-in *)
ANDN X2        (* Stop button *)
ST   Y1        (* Motor output *)
```

## Architecture

The web layer uses CLOG's WebSocket-based DOM manipulation. When a browser connects, `on-new-window` builds the entire page programmatically — injecting CSS, SVG prototype definitions, the control panel, status display, and ladder diagram area. A per-connection background thread polls the PLC state and re-renders the SVG ladder with live address values.

SVG symbols are defined once in `static/laddersvgdefs.html` and injected into a hidden `<div>` on the page. Each rung SVG uses `<use href="#symbol-id">` to reference them, with inline `<style>` blocks for correct rendering across browsers.

## License

GPL-3.0 — see the [MBLogic project](http://mblogic.sourceforge.net/) for original license terms.
