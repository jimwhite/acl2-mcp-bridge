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

(defun %launch-readme-server (&key (timeout 15))
  "Start the README basic server over HTTP on a random port. Returns (values proc port)."
  (let* ((port (%pick-port))
         (script (namestring (merge-pathnames "tests/readme-basic-server.lisp"
                                               (uiop:getcwd))))
         ;; Use shell to set MCP_PORT and run sbcl
         (proc (uiop:launch-program
                (format nil "MCP_PORT=~D sbcl --noinform --disable-debugger --script ~A" port script)
                :output :stream
                :error-output :stream)))
    ;; Wait for HTTP port to become ready
    (labels ((ready-p ()
               (handler-case
                   (usocket:with-client-socket (sock stream "127.0.0.1" port
                                                    :element-type 'character
                                                    :timeout 0.1)
                     (declare (ignore sock stream))
                     t)
                 (error () nil))))
      (loop repeat (ceiling (* 10 timeout))
            when (ready-p) return (values proc port)
            do (sleep 0.1)
            finally
              (%stop-proc proc)
              (error "README server failed to start within ~A seconds." timeout)))))

(defun %http-post (port payload &key (timeout 5))
  "Send a raw HTTP POST with PAYLOAD JSON to /mcp and return body as string."
  (let* ((len (length payload))
         (req (format nil "POST /mcp HTTP/1.1~c~chost: 127.0.0.1~c~ccontent-type: application/json~c~ccontent-length: ~d~c~c~%~a"
                       #\Return #\Linefeed #\Return #\Linefeed #\Return #\Linefeed len #\Return #\Linefeed payload))
         (body ""))
    (handler-case
        (usocket:with-client-socket (sock stream "127.0.0.1" port
                                         :element-type 'character
                                         :timeout timeout)
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
                             (when ch (write-char ch s))))))))
        (error ()
          (error "HTTP connection refused")))
    body))

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
