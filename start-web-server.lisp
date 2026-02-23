;;;; start-web-server.lisp
;;;; Start the MBLogic CLOG web server with ladder visualization

;; Load quicklisp
(load "~/quicklisp/setup.lisp")

;; Determine base directory based on platform
(defparameter *mblogic-base-dir*
  (pathname
   (if (or (member :windows *features*)
           (member :win32 *features*))
       "D:/common-lisp/mblogic-clog/"
       (merge-pathnames "common-lisp/mblogic-clog/" (user-homedir-pathname)))))

;; Add local system directory
(push *mblogic-base-dir* asdf:*central-registry*)

;; Load the systems
(format t "~%Loading MBLogic systems...~%")
(ql:quickload :mblogic-clog :silent t)
(ql:quickload :mblogic-clog/web :silent t)

;; Set static directory
(setf mblogic-clog-web:*static-directory*
      (merge-pathnames "static/" *mblogic-base-dir*))

;; Parse and load the test program
(format t "~%Parsing IL program...~%")
(defparameter *parsed-prog*
  (mblogic-clog:parse-il-file
   (namestring (merge-pathnames "test/plcprog.txt" *mblogic-base-dir*))))

;; Compile the program
(format t "Compiling program...~%")
(defparameter *compiler* (mblogic-clog:make-il-compiler))
(defparameter *compiled-prog*
  (mblogic-clog:compile-program *compiler* *parsed-prog*))

;; Create interpreter with compiled program
(defparameter *interp*
  (mblogic-clog:make-plc-interpreter :program *compiled-prog*))

;; Start web server with interpreter
(format t "~%Starting CLOG web server on port 8080...~%")
(mblogic-clog-web:start-web-server :port 8080 :interpreter *interp*)

(format t "~%~%")
(format t "====================================================~%")
(format t "  MBLogic CLOG Web Server Running~%")
(format t "====================================================~%")
(format t "~%")
(format t "  Ladder Diagram Viewer: http://localhost:8080~%")
(format t "~%")
(format t "  Available Subroutines:~%")
(maphash (lambda (name sbr)
           (declare (ignore sbr))
           (format t "    - ~A~%" name))
         (mblogic-clog:program-subroutines *parsed-prog*))
(format t "~%")
(format t "  Press Ctrl+C to stop the server~%")
(format t "~%")
(format t "====================================================~%")
(format t "~%")

;; Keep server running
(handler-case
    (loop (sleep 1))
  (sb-sys:interactive-interrupt ()
    (format t "~&Stopping server...~%")
    (uiop:quit)))
