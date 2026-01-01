;;; Minimal Basic Server example from 40ants-mcp README, ready to run as a script.
;;; Run manually to see a working server:
;;;   sbcl --noinform --disable-debugger --script tests/readme-basic-server.lisp

(require 'asdf)
(unless (find-package :ql)
  (load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
(ql:quickload '(:40ants-mcp) :silent t)

(defpackage #:my-mcp-server
  (:use #:cl)
  (:import-from #:openrpc-server
                #:define-api)
  (:import-from #:40ants-mcp/tools
                #:define-tool)
  (:import-from #:40ants-mcp/content/text
                #:text-content)
  (:import-from #:40ants-mcp/server/definition
                #:start-server)
  (:import-from #:serapeum
                #:soft-list-of))

(in-package #:my-mcp-server)

(openrpc-server:define-api (my-tools :title "My MCP Tools"))

(40ants-mcp/tools:define-tool (my-tools greet) (|name|)
  (:summary "Generate a greeting message.")
  (:param |name| string "Name to greet.")
  (:result (soft-list-of text-content))
  (list (make-instance 'text-content
                       :text (format nil "Hello, ~A!" |name|))))

(defun read-port-from-env ()
  (let ((port-str (uiop:getenv "MCP_PORT")))
    (if (and port-str (plusp (length port-str)))
        (parse-integer port-str)
        8085)))

(defun run-basic-server ()
  (start-server my-tools
                :transport :http
                :port (read-port-from-env)))

;; When run as a script, start the server and keep it alive.
(when (or *load-pathname* *load-truename*)
  (run-basic-server)
  (loop (sleep 1)))
