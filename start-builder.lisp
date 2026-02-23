;;;; start-builder.lisp - Launch CLOG Builder for mblogic-clog
;;;;
;;;; CLOG Builder runs on port 8081 (default port 8080 is used by main app)
;;;; Open http://localhost:8081 in your browser after starting

(require :asdf)

;; Load the builder system (includes clog/tools)
(asdf:load-system :mblogic-clog/builder)

;; Start CLOG Builder on port 8081
(format t "~%Starting CLOG Builder on port 8081...~%")
(format t "Open http://localhost:8081 in your browser~%~%")

(clog-tools:clog-builder :port 8081 :project :mblogic-clog/builder)
