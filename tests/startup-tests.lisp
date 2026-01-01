(in-package #:acl2-mcp-bridge/tests)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (def-suite startup-suite))
(in-suite startup-suite)

(test start-server-bridge-dispatch
  (let ((*bridge-server* nil)
        (*mcp-server* nil)
        (bridge-port nil))
    (letf (((symbol-function 'acl2-mcp-bridge::start-bridge-server)
            (lambda (&key port) (setf bridge-port port) (list :bridge port)))
           ((symbol-function 'initialize-acl2-interface)
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
        (letf (((symbol-function 'initialize-acl2-interface)
            (lambda (&optional path) (setf init-path path) path))
          ((symbol-function 'acl2-mcp-bridge::start-mcp-server)
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
    (letf (((symbol-function 'acl2-mcp-bridge::start-bridge-server)
            (lambda (&key port)
              (incf bridge-calls)
              (list :bridge port)))
           ((symbol-function 'initialize-acl2-interface)
            (lambda (&optional path) (declare (ignore path)) nil))
           ((symbol-function 'acl2-mcp-bridge::start-mcp-server)
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
    (letf (((symbol-function 'usocket:socket-close)
    (lambda (socket) (setf bridge-closed socket))))
  (stop-server :protocol :bridge)
  (is (equal :bridge bridge-closed))
  (is (null *bridge-server*)))))

(test stop-server-mcp-errors
  (let ((*mcp-server* :mcp))
    (signals error (stop-server :protocol :mcp))
    (signals error (stop-server :protocol :all))))
