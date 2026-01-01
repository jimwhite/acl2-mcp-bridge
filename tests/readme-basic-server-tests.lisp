(in-package #:acl2-mcp-bridge/tests)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (def-suite readme-basic-suite))
(in-suite readme-basic-suite)

(defun %pick-port ()
  (+ 20000 (random 10000)))

(defun %stop-proc (proc &key (timeout 2))
  (when proc
    (ignore-errors (uiop:terminate-process proc :abort t))
    (loop repeat (ceiling (* 10 timeout))
          until (not (uiop:process-alive-p proc))
          do (sleep 0.1))
    (ignore-errors (uiop:wait-process proc))))

(defun %launch-readme-server (&key (startup-wait 12))
  "Start the README basic server over HTTP on a random port. Returns (values proc port)."
  (let* ((port (%pick-port))
         (script (namestring (merge-pathnames "tests/readme-basic-server.lisp"
                                               (uiop:getcwd))))
         ;; Use /usr/bin/env to set MCP_PORT, discard output to avoid blocking
         (proc (uiop:launch-program
                (list "/usr/bin/env" (format nil "MCP_PORT=~D" port)
                      "sbcl" "--noinform" "--disable-debugger" "--script" script)
                :output nil
                :error-output nil)))
    ;; Wait for server to start (quicklisp load takes time)
    (sleep startup-wait)
    (values proc port)))

(defun %http-post (port payload)
  "Send HTTP POST with PAYLOAD JSON to /mcp using curl. Returns body string."
  (uiop:run-program
   (list "curl" "-s" "-X" "POST"
         (format nil "http://127.0.0.1:~D/mcp" port)
         "-H" "Content-Type: application/json"
         "-d" payload)
   :output :string))

(test readme-tools-list-http
  (multiple-value-bind (proc port) (%launch-readme-server)
    (unwind-protect
         (let* ((resp (%http-post port
                                  "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/list\"}")))
           (is (stringp resp))
           (is (search "\"greet\"" resp)))
      (%stop-proc proc))))

(test readme-tools-call-greet-http
  (multiple-value-bind (proc port) (%launch-readme-server)
    (unwind-protect
         (let* ((payload "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"tools/call\",\"params\":{\"name\":\"greet\",\"arguments\":{\"name\":\"Alice\"}}}")
                (resp (%http-post port payload)))
           (is (stringp resp))
           (is (search "Hello, Alice!" resp)))
      (%stop-proc proc))))
