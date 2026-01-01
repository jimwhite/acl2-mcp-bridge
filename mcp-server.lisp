(in-package #:acl2-mcp-bridge)


(defun start-mcp-server (&key (transport :stdio) host port)
  "Start the MCP server and return the transport instance.

HOST is accepted for interface compatibility but ignored. PORT is forwarded for
HTTP transport. We register tool methods manually to avoid 40ants-mcp's missing
aggregation across API collections."
  (declare (ignore host))
  (let* ((apis (list acl2-api cl-api bridge-api))
         (rpc-server (make-server)))
    ;; Register all methods from each API into the JSON-RPC server.
    (dolist (api apis)
      (maphash (lambda (name method-info)
                 (expose rpc-server name (method-thunk method-info)))
               (openrpc-server:api-methods api)))

    (log:info "MCP API methods"
              (mapcar (lambda (api)
                        (let ((names '()))
                          (maphash (lambda (k v) (declare (ignore v)) (push k names))
                                   (openrpc-server:api-methods api))
                          names))
                      apis))

    (let ((transport-instance (ecase transport
                                (:stdio (make-instance 'stdio-transport))
                                (:http (make-instance 'http-transport :port port)))))
      (start-loop transport-instance
                  (lambda (message)
                    (cond
                      ;; Empty body -> JSON-RPC parse error
                      ((or (null message)
                           (and (stringp message) (string= message "")))
                       (log:error "Empty MCP message received; returning parse error")
                       "{\"jsonrpc\":\"2.0\",\"error\":{\"code\":-32700,\"message\":\"Parse error (empty body)\"},\"id\":null}")
                      (t
                       (let ((response
                               (handler-case
                                   (handle-message rpc-server message)
                                 (error (e)
                                   (log:error "Unhandled MCP handler error: ~A" e)
                                   (format nil
                                           "{\\\"jsonrpc\\\":\\\"2.0\\\",\\\"error\\\":{\\\"code\\\":-32000,\\\"message\\\":\\\"~A\\\"},\\\"id\\\":null}"
                                           e)))))
                         (or response
                             "{\"jsonrpc\":\"2.0\",\"error\":{\"code\":-32700,\"message\":\"Parse error (empty body)\"},\"id\":null}"))))))
      transport-instance)))

;; Note: 40ants-mcp has no public stop API; stopping requires terminating the process.
