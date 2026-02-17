;;;; src/web/server.lisp
;;;;
;;;; CLOG-based Web Server
;;;; Provides a server-rendered ladder diagram visualization and PLC control UI.
;;;; Replaces static HTML/JS/CSS files with a programmatically built CLOG page.

(in-package #:mblogic-clog-web)

;;; ============================================================
;;; Global State
;;; ============================================================

(defvar *plc-interpreter* nil
  "The PLC interpreter being served.")

(defvar *plc-thread* nil
  "Background thread running the PLC interpreter.")

(defvar *plc-lock* (bt:make-lock "plc-lock")
  "Lock for thread-safe PLC operations.")

(defvar *static-directory* nil
  "Directory for static files (CSS, SVG defs).")

;;; ============================================================
;;; Static Directory Detection
;;; ============================================================

(defun find-static-directory ()
  "Find the static files directory relative to this source file."
  (let* ((this-file (or *compile-file-truename* *load-truename*))
         (src-web-dir (when this-file (directory-namestring this-file)))
         (project-root (when src-web-dir
                         (merge-pathnames "../../" src-web-dir)))
         (static-dir (when project-root
                       (merge-pathnames "static/" project-root))))
    (if (and static-dir (probe-file static-dir))
        static-dir
        (let ((cwd-static (merge-pathnames "static/" (uiop:getcwd))))
          (when (probe-file cwd-static) cwd-static)))))

(defun initialize-static-directory ()
  "Initialize the static directory path."
  (unless *static-directory*
    (setf *static-directory* (find-static-directory))))

;;; ============================================================
;;; PLC Thread Management
;;; ============================================================

(defun start-plc-thread ()
  "Start the PLC interpreter in a background thread."
  (bt:with-lock-held (*plc-lock*)
    (when (and *plc-interpreter* (not *plc-thread*))
      (setf *plc-thread*
            (bt:make-thread
             (lambda ()
               (handler-case
                   (mblogic-cl:run-continuous *plc-interpreter*
                                              :target-scan-time 10)
                 (error (e)
                   (format *error-output* "PLC thread error: ~A~%" e))))
             :name "plc-interpreter"))
      t)))

(defun stop-plc-thread ()
  "Stop the PLC interpreter thread."
  (bt:with-lock-held (*plc-lock*)
    (when *plc-interpreter*
      (mblogic-cl:stop-interpreter *plc-interpreter*))
    (when *plc-thread*
      (sleep 0.1)
      (setf *plc-thread* nil))
    t))

(defun plc-thread-running-p ()
  "Check if PLC thread is running."
  (and *plc-thread*
       *plc-interpreter*
       (mblogic-cl:interpreter-running-p *plc-interpreter*)))

(defun step-plc ()
  "Execute a single PLC scan."
  (bt:with-lock-held (*plc-lock*)
    (when *plc-interpreter*
      (mblogic-cl:step-scan *plc-interpreter*)
      t)))

;;; ============================================================
;;; Page CSS (inline style)
;;; ============================================================

(defun ladder-css ()
  "Return the ladder diagram CSS as a string, loaded from static/css/ladder.css."
  (when *static-directory*
    (let ((css-path (merge-pathnames "css/ladder.css" *static-directory*)))
      (when (probe-file css-path)
        (uiop:read-file-string css-path)))))

(defun monitoring-css ()
  "CSS for SVG ladder display, monitoring colors, and status panel."
  "
/* SVG ladder display */
#staticrunglist { margin-top: 20px; }
#staticrunglist svg {
    display: block;
    width: 100%;
    max-width: 1200px;
    margin: 10px 0;
    border: 2px solid #333;
    background-color: white;
}
#staticrunglist svg line {
    stroke: black;
    stroke-width: 2;
}
#staticrunglist svg circle {
    stroke: black;
    stroke-width: 2;
    fill: black;
}
#staticrunglist svg rect {
    stroke: black;
    stroke-width: 1;
    fill: none;
}
#staticrunglist svg ellipse {
    stroke: black;
    stroke-width: 2;
    fill: none;
}
#staticrunglist svg text {
    font-family: Arial, sans-serif;
    font-size: 12px;
    fill: black;
    text-anchor: middle;
}
/* Monitoring color classes */
.MB_ladderon line, .MB_ladderon polyline, .MB_ladderon circle, .MB_ladderon ellipse {
    stroke: red;
    stroke-width: 2.0px;
}
.MB_ladderoff line, .MB_ladderoff polyline, .MB_ladderoff circle, .MB_ladderoff ellipse {
    stroke: blue;
    stroke-width: 0.5px;
}
/* Status panel */
.status-panel {
    display: flex;
    gap: 20px;
    margin: 10px 0;
    padding: 10px;
    background-color: #f9f9f9;
    border: 1px solid #ddd;
    border-radius: 5px;
}
.status-item { display: flex; flex-direction: column; }
.status-label { font-size: 12px; color: #666; }
.status-value { font-size: 16px; font-weight: bold; font-family: monospace; }
.status-value.running { color: green; }
.status-value.stopped { color: #cc0000; }
")

;;; ============================================================
;;; Per-Connection State
;;; ============================================================

(defstruct connection-state
  "State for a single browser connection (CLOG window)."
  (body nil)
  (subroutine-name "main")
  (monitoring-p nil)
  (monitor-thread nil)
  (status-running nil)     ; CLOG span element
  (status-scan-count nil)  ; CLOG span element
  (status-scan-time nil)   ; CLOG span element
  (ladder-div nil)         ; CLOG div element for ladder display
  (subr-select nil))       ; CLOG select element

;;; ============================================================
;;; Display Update Functions
;;; ============================================================

(defun update-status-display (conn-state)
  "Update the status panel with current PLC statistics."
  (let ((stats (get-statistics-data *plc-interpreter*)))
    (let ((running (getf stats :running))
          (scan-count (getf stats :scan-count))
          (scan-time (getf stats :scan-time)))
      ;; Update running status
      (let ((status-el (connection-state-status-running conn-state)))
        (when status-el
          (setf (clog:text status-el)
                (if running "Running" "Stopped"))
          (if running
              (progn
                (clog:remove-class status-el "stopped")
                (clog:add-class status-el "running"))
              (progn
                (clog:remove-class status-el "running")
                (clog:add-class status-el "stopped")))))
      ;; Update scan count
      (let ((count-el (connection-state-status-scan-count conn-state)))
        (when count-el
          (setf (clog:text count-el) (format nil "~D" scan-count))))
      ;; Update scan time
      (let ((time-el (connection-state-status-scan-time conn-state)))
        (when time-el
          (setf (clog:text time-el)
                (format nil "~,2F ms" (or scan-time 0.0))))))))

(defun update-ladder-display (conn-state &optional force-reload)
  "Refresh the ladder diagram display with current PLC state.
   If monitoring is active or FORCE-RELOAD is true, re-render with data values."
  (declare (ignore force-reload))
  (let ((ladder-div (connection-state-ladder-div conn-state))
        (subrname (connection-state-subroutine-name conn-state)))
    (when (and ladder-div *plc-interpreter*)
      (let* ((program (mblogic-cl:interpreter-program *plc-interpreter*))
             (source (when program (mblogic-cl:program-source program))))
        (when source
          (let ((ladder (program-to-ladder source (or subrname "main"))))
            (when ladder
              ;; Get current address values for monitoring
              (let ((address-values
                      (when (connection-state-monitoring-p conn-state)
                        (get-address-values-map
                         *plc-interpreter*
                         (ladder-program-addresses ladder)))))
                (setf (clog:inner-html ladder-div)
                      (or (render-ladder-program-html ladder address-values)
                          "<p>No rungs to display.</p>"))))))))))

(defun monitor-loop (conn-state)
  "Background monitoring loop - updates status and ladder display every second."
  (loop
    while (connection-state-monitoring-p conn-state)
    do (handler-case
           (progn
             (update-status-display conn-state)
             (update-ladder-display conn-state))
         (error (e)
           (format *error-output* "Monitor loop error: ~A~%" e)))
       (sleep 1)))

(defun start-connection-monitor (conn-state)
  "Start the background monitoring thread for this connection."
  (unless (connection-state-monitor-thread conn-state)
    (setf (connection-state-monitor-thread conn-state)
          (bt:make-thread
           (lambda () (monitor-loop conn-state))
           :name "clog-monitor"))))

(defun stop-connection-monitor (conn-state)
  "Stop the background monitoring thread for this connection."
  (setf (connection-state-monitoring-p conn-state) nil)
  (setf (connection-state-monitor-thread conn-state) nil))

;;; ============================================================
;;; Page Builder
;;; ============================================================

(defun populate-subroutine-select (select-el)
  "Fill the subroutine <select> with available subroutine names."
  (let ((names (get-subroutine-names *plc-interpreter*)))
    ;; Remove existing options
    (setf (clog:inner-html select-el) "")
    ;; Add one option per subroutine
    (dolist (name names)
      (clog:add-select-option select-el name name))))

(defun on-new-window (body)
  "Called by CLOG for each new browser connection.
   Builds the complete ladder monitor page and sets up all event handlers."

  ;; --- Page title and CSS ---
  (setf (clog:title (clog:html-document body)) "MBLogic-CL Ladder Monitor")

  ;; Load ladder CSS
  (let ((css (ladder-css))
        (doc (clog:html-document body)))
    (when css
      (let ((style-block (clog:create-style-block (clog:head-element doc) :content css)))
        (declare (ignore style-block))))

    ;; Add monitoring CSS
    (clog:create-style-block (clog:head-element doc) :content (monitoring-css)))

  ;; --- Page heading ---
  (clog:create-child body "<h1>MBLogic-CL Ladder Monitor</h1>")

  ;; --- Control panel ---
  (let* ((panel (clog:create-child body
                  "<div class=\"control-panel\"></div>"))

         ;; Buttons
         (btn-monitor (clog:create-button panel :content "Monitor"))
         (btn-start   (clog:create-button panel :content "Start"))
         (btn-stop    (clog:create-button panel :content "Stop"))
         (btn-step    (clog:create-button panel :content "Single Scan"))

         ;; Subroutine selector
         (subr-wrapper (clog:create-child panel
                         "<div class=\"subroutine-selector\"></div>"))
         (subr-select (progn
                        (clog:create-child subr-wrapper
                          "<label>Subroutine:</label>")
                        (clog:create-select subr-wrapper))))
    (clog:add-class btn-monitor "btn-monitor")
    (clog:add-class btn-start "btn-start")
    (clog:add-class btn-stop "btn-stop")
    (clog:add-class btn-step "btn-step")

    ;; --- Status panel ---
    (let* ((status-panel (clog:create-child body
                           "<div class=\"status-panel\"></div>"))

           ;; Status: Running/Stopped
           (stat-item1 (clog:create-child status-panel
                         "<div class=\"status-item\"></div>"))
           (span-running (progn
                           (clog:create-child stat-item1
                             "<span class=\"status-label\">Status</span>")
                           (clog:create-span stat-item1)))

           ;; Scan count
           (stat-item2 (clog:create-child status-panel
                         "<div class=\"status-item\"></div>"))
           (span-count (progn
                         (clog:create-child stat-item2
                           "<span class=\"status-label\">Scan Count</span>")
                         (clog:create-span stat-item2)))

           ;; Scan time
           (stat-item3 (clog:create-child status-panel
                         "<div class=\"status-item\"></div>"))
           (span-time (progn
                        (clog:create-child stat-item3
                          "<span class=\"status-label\">Scan Time</span>")
                        (clog:create-span stat-item3))))


      (clog:add-class span-running "status-value stopped")
      (setf (clog:text span-running) "Stopped")
      (clog:add-class span-count "status-value")
      (setf (clog:text span-count) "0")
      (clog:add-class span-time "status-value")
      (setf (clog:text span-time) "0.00 ms")

      ;; --- SVG Prototypes (hidden) ---
      (let ((proto-div (clog:create-child body
                         "<div id=\"ladderprototypes\" style=\"display:none\"></div>")))
        (setf (clog:inner-html proto-div) (get-svg-prototypes-html)))

      ;; --- Ladder display area ---
      (let ((ladder-div (clog:create-child body "<div id=\"staticrunglist\"></div>")))
        (setf (clog:inner-html ladder-div) "<div class=\"loading\">Loading program...</div>")

        ;; --- Connection state ---
        (let ((conn-state (make-connection-state
                           :body body
                           :status-running span-running
                           :status-scan-count span-count
                           :status-scan-time span-time
                           :ladder-div ladder-div
                           :subr-select subr-select)))

          ;; Store state in CLOG connection data for cleanup on disconnect
          (setf (clog:connection-data-item body "conn-state") conn-state)

          ;; --- Populate subroutine selector ---
          (populate-subroutine-select subr-select)

          ;; --- Sync initial subroutine name from selector (first available) ---
          (let ((first-name (first (get-subroutine-names *plc-interpreter*))))
            (when first-name
              (setf (connection-state-subroutine-name conn-state) first-name)))

          ;; --- Initial ladder load ---
          (update-ladder-display conn-state t)

          ;; ============================================================
          ;; Event Handlers
          ;; ============================================================

          ;; Monitor button: toggle live monitoring
          (clog:set-on-click btn-monitor
            (lambda (obj)
              (declare (ignore obj))
              (if (connection-state-monitoring-p conn-state)
                  ;; Turn monitoring off
                  (progn
                    (stop-connection-monitor conn-state)
                    (setf (clog:text btn-monitor) "Monitor"))
                  ;; Turn monitoring on
                  (progn
                    (setf (connection-state-monitoring-p conn-state) t)
                    (start-connection-monitor conn-state)
                    (setf (clog:text btn-monitor) "Stop Monitor")))))

          ;; Start button: start PLC continuous execution
          (clog:set-on-click btn-start
            (lambda (obj)
              (declare (ignore obj))
              (if (plc-thread-running-p)
                  (format t "PLC already running~%")
                  (start-plc-thread))
              (update-status-display conn-state)))

          ;; Stop button: stop PLC execution
          (clog:set-on-click btn-stop
            (lambda (obj)
              (declare (ignore obj))
              (stop-plc-thread)
              (update-status-display conn-state)))

          ;; Step button: single scan (only when stopped)
          (clog:set-on-click btn-step
            (lambda (obj)
              (declare (ignore obj))
              (if (plc-thread-running-p)
                  (format t "Cannot step while running - stop first~%")
                  (progn
                    (step-plc)
                    (update-status-display conn-state)
                    (update-ladder-display conn-state t)))))

          ;; Subroutine select: reload ladder when selection changes
          (clog:set-on-change subr-select
            (lambda (obj)
              (declare (ignore obj))
              (let ((selected (clog:value subr-select)))
                (setf (connection-state-subroutine-name conn-state) selected)
                (update-ladder-display conn-state t))))

          ;; Handle disconnect: stop monitoring thread
          (clog:set-on-before-unload body
            (lambda (obj)
              (declare (ignore obj))
              (stop-connection-monitor conn-state))))))))

;;; ============================================================
;;; Server Management
;;; ============================================================

(defun start-web-server (&key (port 8080) interpreter)
  "Start the CLOG web server.
   PORT: HTTP port to listen on (default 8080)
   INTERPRETER: PLC interpreter instance to serve"
  (setf *plc-interpreter* interpreter)
  (initialize-static-directory)

  ;; Load SVG prototypes for injection into pages
  (when *static-directory*
    (load-svg-prototypes *static-directory*))

  ;; Initialize CLOG
  (clog:initialize #'on-new-window
                   :port port
                   :static-root (or *static-directory*
                                    (merge-pathnames "static/"
                                                     (uiop:getcwd))))

  (format t "~%MBLogic-CL CLOG Server started on port ~D~%" port)
  (format t "Open http://localhost:~D in your browser~%" port)
  (when *static-directory*
    (format t "Static files from: ~A~%" *static-directory*))
  port)

(defun stop-web-server ()
  "Stop the web server and PLC thread."
  (stop-plc-thread)
  (clog:shutdown)
  (format t "Web server stopped~%")
  nil)

(defun web-server-running-p ()
  "Check if the web server is running."
  ;; CLOG doesn't expose a clean running-p; check if initialized
  (not (null *plc-interpreter*)))

;;; ============================================================
;;; Convenience Functions
;;; ============================================================

(defun quick-start (il-source &key (port 8080))
  "Quickly start a web server with an IL program.
   IL-SOURCE: IL program string, pathname, or file path string"
  (let* ((is-file (or (pathnamep il-source)
                      (and (stringp il-source)
                           (probe-file il-source))))
         (compiled (if is-file
                       (mblogic-cl:compile-il-file (if (stringp il-source)
                                                        (pathname il-source)
                                                        il-source))
                       (mblogic-cl:compile-il-string il-source)))
         (interp (mblogic-cl:make-plc-interpreter :program compiled)))
    (start-web-server :port port :interpreter interp)))

;;; End of server.lisp
