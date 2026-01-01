(in-package #:acl2-mcp-bridge)

;; ACL2 Bridge Protocol Implementation
;; The Bridge uses a simple text-based message format suitable for any language

(defun start-bridge-server (&key (port *bridge-port*) (address *bridge-listen-address*))
  "Start the ACL2 Bridge protocol server.

   This maintains backward compatibility with existing Bridge clients
   (Ruby, Python, Node, etc.)."
  (log:info "Starting ACL2 Bridge on ~A:~A" address port)

  (let ((server (usocket:socket-listen address port 
                                        :reuseaddress t
                                        :element-type 'character)))
    (bt:make-thread
     (lambda ()
       (unwind-protect
           (handle-bridge-connections server)
         (usocket:socket-close server)))
     :name "acl2-bridge-listener")
    server))

(defun handle-bridge-connections (server)
  "Accept and handle Bridge protocol connections."
  (loop
    (handler-case
      (let ((client (usocket:socket-accept server)))
        (bt:make-thread
         (lambda ()
           (handle-bridge-client client))
         :name (format nil "bridge-client-~A" (acl2-session-id (create-acl2-session)))))
      (error (e)
        (log:error "Bridge connection error: ~A" e)))))

(defun handle-bridge-client (socket)
  "Handle a single Bridge protocol client."
  (let ((session (create-acl2-session))
        (stream (usocket:socket-stream socket)))
    (unwind-protect
        (progn
          ;; Send HELLO message
          (send-bridge-message stream "HELLO" (acl2-session-id session))

          ;; Main read-eval-print loop
          (loop
            (send-bridge-message stream "READY" "")

            (let* ((msg (read-bridge-message stream))
                   (cmd-type (car msg))
                   (content (cdr msg)))
              (cond
                ((string= cmd-type "LISP")
                 (handle-bridge-lisp-command session content stream))
                ((string= cmd-type "ACL2")
                 (handle-bridge-acl2-command session content stream))
                ((string= cmd-type "BYE")
                 (return))
                (t
                 (send-bridge-error stream "Unknown command type"))))))
      (usocket:socket-close socket))))

(defun handle-bridge-lisp-command (session content stream)
  "Handle (LISP code) - evaluate Common Lisp in session context."
  ;; Get or create CL session for this bridge client
  (let ((*current-session* (get-or-create-session (acl2-session-id session))))
    (multiple-value-bind (result error-p error-msg)
        (cl-eval content)
      (if error-p
          (send-bridge-error stream error-msg)
          (send-bridge-return stream (format nil "~S" result))))))

(defun handle-bridge-acl2-command (session content stream)
  "Handle (ACL2 form) - evaluate ACL2 via nld/ld."
  (multiple-value-bind (result error-p error-msg)
      (acl2-eval content :session-id (acl2-session-id session) :main-thread-p t)
    (if error-p
      (send-bridge-error stream error-msg)
      (send-bridge-return stream (format nil "~S" result)))))
