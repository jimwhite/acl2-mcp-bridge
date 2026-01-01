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

(test eval-cl-jsonrpc-roundtrip
  ;; Simulate the HTTP handler path: build a JSON-RPC server, expose methods, and dispatch eval_cl.
  (let* ((apis (list acl2-api cl-api bridge-api))
         (rpc-server (jsonrpc:make-server)))
    (dolist (api apis)
      (maphash (lambda (name method-info)
                 (jsonrpc:expose rpc-server name (openrpc-server/method:method-thunk method-info)))
               (openrpc-server:api-methods api)))
    (let* ((payload "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"eval_cl\",\"params\":{\"code\":\"'(1 2 3 4)\"}}")
           (response (40ants-mcp/server/definition:handle-message rpc-server payload)))
      (is (stringp response))
      (is (search "1 2 3 4" response)))))
