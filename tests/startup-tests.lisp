(in-package #:acl2-mcp-bridge/tests)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (def-suite startup-suite))
(in-suite startup-suite)

(defmacro with-redefs (bindings &body body)
  "Temporarily override function definitions for testing."
  (let ((old-fns (gensym "OLD-FNS")))
    `(let ((,old-fns (list ,@(mapcar (lambda (binding)
                                       `(list ',(car binding)
                                              (symbol-function ',(car binding))))
                                     bindings))))
       (unwind-protect
            (progn
              ,@(mapcar (lambda (binding)
                          `(setf (symbol-function ',(car binding)) ,(cadr binding)))
                        bindings)
              ,@body)
         (dolist (entry ,old-fns)
           (setf (symbol-function (first entry)) (second entry)))))))

(test start-server-bridge-dispatch
  (let ((*bridge-server* nil)
        (*mcp-server* nil)
        (bridge-port nil))
    (with-redefs ((acl2-mcp-bridge::start-bridge-server
                   (lambda (&key port) (setf bridge-port port) (list :bridge port)))
                  (initialize-acl2-interface
                   (lambda (&optional _) (error "ACL2 init should not run for bridge"))))
      (let ((server (start-server :protocol :bridge :port 1234)))
        (is (equal 1234 bridge-port))
        (is (equal '(:bridge 1234) server))
        (is (equal server *bridge-server*))))))

(test start-server-mcp-dispatch
  (let ((*bridge-server* nil)
        (*mcp-server* nil)
        (init-path nil)
        (args nil))
        (with-redefs ((initialize-acl2-interface
                       (lambda (&optional path) (setf init-path path) path))
                      (acl2-mcp-bridge::start-mcp-server
                       (lambda (&key transport host port)
                         (setf args (list :transport transport :host host :port port))
                         (list :mcp transport host port))))
      (let ((server (start-server :protocol :mcp
                                  :transport :http
                                  :host "0.0.0.0"
                                  :port 8085
                                  :acl2-path "/tmp/acl2")))
        (is (equal "/tmp/acl2" init-path))
        (is (equal '(:transport :http :host "0.0.0.0" :port 8085) args))
        (is (equal '(:mcp :http "0.0.0.0" 8085) server))
        (is (equal server *mcp-server*))))))

(test start-both-dispatch
  (let ((*bridge-server* nil)
        (*mcp-server* nil)
        (bridge-calls 0)
        (mcp-calls 0))
    (with-redefs ((acl2-mcp-bridge::start-bridge-server
                   (lambda (&key port)
                     (incf bridge-calls)
                     (list :bridge port)))
                  (initialize-acl2-interface
                   (lambda (&optional path) (declare (ignore path)) nil))
                  (acl2-mcp-bridge::start-mcp-server
                   (lambda (&key transport host port)
                     (declare (ignore host port))
                     (incf mcp-calls)
                     (list :mcp transport))))
      (multiple-value-bind (bridge mcp)
          (start-both :bridge-port 1111 :mcp-transport :stdio :acl2-path "acl2-bin")
        (is (= 1 bridge-calls))
        (is (= 1 mcp-calls))
        (is (equal '(:bridge 1111) bridge))
        (is (equal '(:mcp :stdio) mcp))
        (is (equal bridge *bridge-server*))
        (is (equal mcp *mcp-server*))))))

(test stop-server-bridge-only
  (let ((*bridge-server* :bridge)
    (bridge-closed nil))
    (with-redefs ((usocket:socket-close
                   (lambda (socket) (setf bridge-closed socket))))
  (stop-server :protocol :bridge)
  (is (equal :bridge bridge-closed))
  (is (null *bridge-server*)))))

(test stop-server-mcp-errors
  (let ((*mcp-server* :mcp))
    (signals error (stop-server :protocol :mcp))
    (signals error (stop-server :protocol :all))))

(test cl-session-start-stop
  (let ((*cl-sessions* (make-hash-table :test 'equal)))
    (let ((id (start-cl-session)))
      (is (stringp id))
      (is (get-cl-session id))
      (is (stop-cl-session id))
      (is (null (get-cl-session id))))))

(test acl2-session-start-stop
  (let ((*acl2-sessions* (make-hash-table :test 'equal)))
    (let ((id (start-acl2-session)))
      (is (stringp id))
      (is (get-acl2-session id))
      (is (stop-acl2-session id))
      (is (null (get-acl2-session id))))))

(test start-mcp-server-builds-apis
  ;; Ensure APIs are registered and start-loop is invoked.
  (let ((exposed '())
        (loop-called nil))
    (with-redefs ((jsonrpc:make-server (lambda () :server))
                  (jsonrpc:expose (lambda (srv name thunk)
                                    (declare (ignore srv thunk))
                                    (push name exposed)))
                  (start-loop (lambda (transport handler)
                                (setf loop-called (list transport handler)))))
      (let ((result (acl2-mcp-bridge::start-mcp-server :transport :http :host "0.0.0.0" :port 4242)))
        (is (not (null loop-called)))
        (is (typep result '40ants-mcp/http-transport:http-transport))
        (is (member "list_sessions" exposed :test #'string=))
        (is (member "eval_cl" exposed :test #'string=))))))

(test eval-cl-http-roundtrip
  ;; Start MCP server in a child SBCL with minimal args, wait for port, then POST once.
  (let* ((port (+ 20000 (random 10000)))
         (payload "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"eval_cl\",\"params\":{\"code\":\"'(1 2 3 4)\"}}")
         (req (format nil "POST /mcp HTTP/1.1~c~chost: 127.0.0.1~c~ccontent-type: application/json~c~ccontent-length: ~d~c~c~%~a"
                      #\Return #\Linefeed #\Return #\Linefeed #\Return #\Linefeed (length payload) #\Return #\Linefeed payload))
         (body "")
         (cmd (list "sbcl" "--noinform" "--disable-debugger"
                    "--eval" "(require 'asdf)"
                    "--eval" "(asdf:load-system :acl2-mcp-bridge)"
                    "--eval" (format nil "(acl2-mcp-bridge:start-server :protocol :mcp :transport :http :port ~D)" port)
                    "--eval" "(format t \"READY\") (finish-output) (loop (sleep 1))")))
    (let ((proc (uiop:launch-program cmd :output :stream :error-output :stream)))
      (unwind-protect
           (progn
             ;; Wait for HTTP port to accept connections (up to ~5s).
             (loop repeat 50
                   thereis (progn
                             (unless (uiop:process-alive-p proc)
                               (uiop:wait-process proc)
                               (error "MCP server process exited early. stdout: ~A stderr: ~A"
                                      (if (uiop:process-info-output proc) (uiop:slurp-stream-string (uiop:process-info-output proc)) "")
                                      (if (uiop:process-info-error-output proc) (uiop:slurp-stream-string (uiop:process-info-error-output proc)) "")))
                             (handler-case
                                 (usocket:with-client-socket (sock stream "127.0.0.1" port
                                                                  :element-type 'character
                                                                  :timeout 0.2)
                                   (declare (ignore sock stream))
                                   t)
                               (usocket:connection-refused-error () (sleep 0.1) nil)
                               (usocket:timeout-error () (sleep 0.1) nil)))
                   finally (error "MCP server did not start"))
             ;; Send request and read body.
             (handler-case
                 (usocket:with-client-socket (sock stream "127.0.0.1" port
                                                  :element-type 'character
                                                  :timeout 5)
                   (write-string req stream)
                   (finish-output stream)
                   (let ((content-length 0))
                     (loop for line = (read-line stream nil nil)
                           while (and line (not (string= line ""))) do
                             (when (alexandria:starts-with-subseq "Content-Length:" line :test #'char-equal)
                               (setf content-length
                                     (parse-integer (string-trim '(#\Space #\Tab)
                                                                 (subseq line 15))))))
                     (setf body (with-output-to-string (s)
                                  (dotimes (_ content-length)
                                    (let ((ch (read-char stream nil nil)))
                                      (when ch (write-char ch s)))))))
               (usocket:connection-refused-error ()
                 (error "HTTP transport refused connection"))))
        (ignore-errors (uiop:terminate-process proc :abort t))
        (uiop:wait-process proc)))
    (is (stringp body))
    (is (search "1 2 3 4" body)))))
