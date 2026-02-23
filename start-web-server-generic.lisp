;;;; start-web-server-generic.lisp
;;;; Start the MBLogic CLOG web server with an IL file specified via *il-file*.
;;;; Called by start-mblogic-clog.sh with:
;;;;   sbcl --eval "(defvar *il-file* \"path/to/prog.txt\")" --load this-file.lisp

;; Load ocicl
(load "~/.local/share/ocicl/ocicl-runtime.lisp")

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
(asdf:load-system :mblogic-clog)
(asdf:load-system :mblogic-clog/web)

;; Set static directory
(setf mblogic-clog-web:*static-directory*
      (merge-pathnames "static/" *mblogic-base-dir*))

;; *il-file* must be set via --eval before loading this script
(unless (boundp '*il-file*)
  (defvar *il-file*
    (namestring (merge-pathnames "test/demodataprog.txt" *mblogic-base-dir*))))

;; Parse and load the program
(format t "~%Parsing IL program: ~A~%" *il-file*)
(defparameter *parsed-prog*
  (mblogic-cl:parse-il-file *il-file*))

;; Compile the program
(format t "Compiling program...~%")
(defparameter *compiler* (mblogic-cl:make-il-compiler))
(defparameter *compiled-prog*
  (mblogic-cl:compile-program *compiler* *parsed-prog*))

;; Create interpreter with compiled program
(defparameter *interp*
  (mblogic-cl:make-plc-interpreter :program *compiled-prog*))

;; Start web server with interpreter
(format t "~%Starting CLOG web server on port 8080...~%")
(mblogic-clog-web:start-web-server :port 8080 :interpreter *interp*)

(format t "~%~%")
(format t "====================================================~%")
(format t "  MBLogic CLOG Web Server Running~%")
(format t "====================================================~%")
(format t "~%")
(format t "  Program:  ~A~%" *il-file*)
(format t "  Ladder Diagram Viewer: http://localhost:8080~%")
(format t "~%")
(format t "  Available Subroutines:~%")
(let ((main-nets (mblogic-cl:program-main-networks *parsed-prog*)))
  (when main-nets
    (format t "    - main~%")))
(maphash (lambda (name sbr)
           (declare (ignore sbr))
           (format t "    - ~A~%" name))
         (mblogic-cl:program-subroutines *parsed-prog*))
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
