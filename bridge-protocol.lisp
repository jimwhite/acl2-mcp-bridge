(in-package #:acl2-mcp-bridge)

;;; ============================================================================
;;; ACL2 Bridge Protocol Implementation
;;;
;;; This is a portable (SBCL-compatible) reimplementation of the ACL2 Bridge
;;; protocol from centaur/bridge. The original only works on CCL.
;;;
;;; Protocol: Simple text messages - "TYPE LEN\nCONTENT\n"
;;; Command types: LISP, LISP_MV, JSON, JSON_MV
;;; Response types: ACL2_BRIDGE_HELLO, READY, RETURN, ERROR, STDOUT
;;; ============================================================================

;;; ---------------------------------------------------------------------------
;;; BRIDGE package macros
;;;
;;; Python clients (acl2_bridge) wrap all commands in (bridge::in-main-thread ...)
;;; For our single-threaded Bridge server, these macros just execute the forms.
;;; ---------------------------------------------------------------------------

(defmacro bridge::in-main-thread (&rest forms)
  "Execute FORMS. In our implementation, all Bridge commands run synchronously
in a dedicated thread, so this just wraps the forms in a progn."
  `(progn ,@forms))

(defmacro bridge::try-in-main-thread (&rest forms)
  "Same as in-main-thread for our implementation."
  `(progn ,@forms))

;;; ---------------------------------------------------------------------------
;;; Bridge Output Stream
;;;
;;; Wraps output so that all printed output gets sent as STDOUT messages.
;;; This lets clients distinguish output from return values.
;;; ---------------------------------------------------------------------------

(defclass bridge-output-stream (trivial-gray-streams:fundamental-character-output-stream)
  ((underlying-stream :initarg :stream :reader underlying-stream)
   (message-type :initarg :message-type :initform "STDOUT" :reader message-type)
   (buffer :initform (make-array 1024 :element-type 'character :fill-pointer 0 :adjustable t)
           :accessor stream-buffer))
  (:documentation "Output stream that wraps writes as Bridge protocol messages."))

(defmethod trivial-gray-streams:stream-write-char ((stream bridge-output-stream) char)
  (vector-push-extend char (stream-buffer stream))
  ;; Flush on newline
  (when (char= char #\Newline)
    (bridge-flush-output stream))
  char)

(defmethod trivial-gray-streams:stream-write-string ((stream bridge-output-stream) string &optional start end)
  (let ((start (or start 0))
        (end (or end (length string))))
    (loop for i from start below end
          do (vector-push-extend (char string i) (stream-buffer stream))))
  ;; Flush on newline at end
  (when (and (> (length string) 0) 
             (char= (char string (1- (or end (length string)))) #\Newline))
    (bridge-flush-output stream))
  string)

(defmethod trivial-gray-streams:stream-force-output ((stream bridge-output-stream))
  (bridge-flush-output stream))

(defmethod trivial-gray-streams:stream-finish-output ((stream bridge-output-stream))
  (bridge-flush-output stream)
  (force-output (underlying-stream stream)))

(defun bridge-flush-output (stream)
  "Flush buffered output as a Bridge message."
  (let ((buffer (stream-buffer stream)))
    (when (> (fill-pointer buffer) 0)
      (send-bridge-message (underlying-stream stream) 
                           (message-type stream) 
                           (copy-seq buffer))
      (setf (fill-pointer buffer) 0))))

;;; ---------------------------------------------------------------------------
;;; JSON Encoding (simplified - for ACL2 objects)
;;; ---------------------------------------------------------------------------

(defun bridge-json-encode (obj)
  "Encode an ACL2/Lisp object as JSON string.
Handles: strings, numbers, symbols, cons cells (as arrays), nil (as null)."
  (with-output-to-string (s)
    (bridge-json-encode-to obj s)))

(defun bridge-json-encode-to (obj stream)
  "Encode OBJ as JSON to STREAM."
  (typecase obj
    (null (write-string "null" stream))
    ((eql t) (write-string "true" stream))
    (string 
     (write-char #\" stream)
     (loop for c across obj do
       (case c
         (#\" (write-string "\\\"" stream))
         (#\\ (write-string "\\\\" stream))
         (#\Newline (write-string "\\n" stream))
         (#\Return (write-string "\\r" stream))
         (#\Tab (write-string "\\t" stream))
         (t (write-char c stream))))
     (write-char #\" stream))
    (integer (format stream "~D" obj))
    (float (format stream "~F" obj))
    (ratio (format stream "~F" (float obj)))
    (symbol 
     ;; Symbols become strings
     (bridge-json-encode-to (symbol-name obj) stream))
    (cons
     ;; Check if it looks like an alist (association list)
     (if (and (consp (car obj)) (atom (caar obj)))
         ;; Encode as JSON object
         (progn
           (write-char #\{ stream)
           (loop for (pair . rest) on obj
                 for first = t then nil
                 when (consp pair) do
                   (unless first (write-string ", " stream))
                   (bridge-json-encode-to (string (car pair)) stream)
                   (write-string ": " stream)
                   (bridge-json-encode-to (cdr pair) stream))
           (write-char #\} stream))
         ;; Encode as JSON array
         (progn
           (write-char #\[ stream)
           (loop for (item . rest) on obj
                 for first = t then nil
                 do (unless first (write-string ", " stream))
                    (bridge-json-encode-to item stream))
           (write-char #\] stream))))
    (vector
     (write-char #\[ stream)
     (loop for i from 0 below (length obj)
           for first = t then nil
           do (unless first (write-string ", " stream))
              (bridge-json-encode-to (aref obj i) stream))
     (write-char #\] stream))
    (hash-table
     (write-char #\{ stream)
     (let ((first t))
       (maphash (lambda (k v)
                  (unless first (write-string ", " stream))
                  (setf first nil)
                  (bridge-json-encode-to (string k) stream)
                  (write-string ": " stream)
                  (bridge-json-encode-to v stream))
                obj))
     (write-char #\} stream))
    (t 
     ;; Fall back to prin1
     (bridge-json-encode-to (prin1-to-string obj) stream))))

;;; ---------------------------------------------------------------------------
;;; Bridge Protocol Server
;;; ---------------------------------------------------------------------------

(defvar *bridge-worker-count* 0
  "Counter for naming bridge worker threads.")

(defun start-bridge-server (&key (port *bridge-port*) (address *bridge-listen-address*))
  "Start the ACL2 Bridge protocol server.

   This is a portable reimplementation compatible with the centaur/bridge
   protocol. Works with Python, Ruby, Node.js bridge clients."
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
          (let ((worker-name (format nil "bridge-worker-~A" (incf *bridge-worker-count*))))
            (bt:make-thread
             (lambda ()
               (handle-bridge-client client worker-name))
             :name worker-name)))
      (error (e)
        (log:error "Bridge connection error: ~A" e)))))

(defun handle-bridge-client (socket worker-name)
  "Handle a single Bridge protocol client.
Worker-name is sent in the hello message for client identification."
  (let ((stream (usocket:socket-stream socket)))
    (unwind-protect
        (handler-case
            (progn
              ;; Send HELLO message with worker name (like original bridge)
              (send-bridge-message stream "ACL2_BRIDGE_HELLO" worker-name)
              (force-output stream)
              
              ;; Main read-eval-print loop
              (loop
                (send-bridge-message stream "READY" "")
                (force-output stream)
                
                (multiple-value-bind (cmd-type content)
                    (read-bridge-command stream)
                  (when (eq cmd-type :eof)
                    (log:info "Client disconnected")
                    (return))
                  
                  (handle-bridge-command cmd-type content stream))))
          (error (e)
            (log:error "Bridge client error: ~A" e)
            (ignore-errors
              (send-bridge-message stream "ERROR" (format nil "~A" e))
              (force-output stream))))
      (ignore-errors (usocket:socket-close socket)))))

;;; ---------------------------------------------------------------------------
;;; Command Handling
;;; ---------------------------------------------------------------------------

(defun handle-bridge-command (cmd-type content stream)
  "Handle a single bridge command. CMD-TYPE is a keyword (:LISP, :JSON, etc)."
  (let* ((ostream (make-instance 'bridge-output-stream :stream stream))
         (*standard-output* ostream)
         (*error-output* ostream)
         (*trace-output* ostream))
    (handler-case
        (let* ((form (read-from-string content))
               ;; Bind STATE like the original bridge does
               (results (multiple-value-list
                         (eval `(let ((acl2::state acl2::*the-live-state*))
                                  (declare (ignorable acl2::state))
                                  ,form)))))
          ;; Flush any pending output
          (bridge-flush-output ostream)
          ;; Send return value based on command type
          (log:info "Sending return for cmd-type ~A, results=~A" cmd-type results)
          (send-bridge-return cmd-type results stream))
      (error (e)
        (log:error "Error during command: ~A" e)
        (bridge-flush-output ostream)
        (send-bridge-message stream "ERROR" 
                             (format nil "Error executing command:~%~A" e))))))

(defun send-bridge-return (cmd-type results stream)
  "Send return value encoded according to CMD-TYPE."
  (let ((content
          (case cmd-type
            (:lisp (prin1-to-string (car results)))
            (:lisp_mv (prin1-to-string results))
            (:json (bridge-json-encode (car results)))
            (:json_mv (bridge-json-encode results))
            (t (prin1-to-string (car results))))))
    (send-bridge-message stream "RETURN" content)))

;;; ---------------------------------------------------------------------------
;;; Message I/O
;;; ---------------------------------------------------------------------------

(defun send-bridge-message (stream type content)
  "Send a Bridge protocol message: TYPE LEN\\nCONTENT\\n"
  (let ((len (length content)))
    (write-string type stream)
    (write-char #\Space stream)
    (prin1 len stream)
    (write-char #\Newline stream)
    (write-string content stream)
    (write-char #\Newline stream)
    (force-output stream)))

(defun send-bridge-error (stream message)
  "Send an error message."
  (send-bridge-message stream "ERROR" message))

(defun read-bridge-command (stream)
  "Read a Bridge protocol command.
Returns (values cmd-type content) or (values :eof nil) on EOF."
  (let ((header-line (read-line stream nil nil)))
    (unless header-line
      (return-from read-bridge-command (values :eof nil)))
    
    ;; Parse "TYPE LEN"
    (let* ((space-pos (position #\Space header-line))
           (type-str (and space-pos (subseq header-line 0 space-pos)))
           (len-str (and space-pos (subseq header-line (1+ space-pos)))))
      
      (unless (and type-str len-str)
        (error "Invalid Bridge header: ~A" header-line))
      
      (let* ((len (parse-integer len-str :junk-allowed t)))
        (unless len
          (error "Invalid length in header: ~A" header-line))
        
        ;; Read content
        (let ((content (make-string len)))
          (let ((nread (read-sequence content stream)))
            (unless (= nread len)
              (error "Expected ~A chars but got ~A" len nread)))
          
          ;; Consume trailing newline
          (let ((newline (read-char stream nil nil)))
            (unless (and newline (char= newline #\Newline))
              (error "Expected newline after content")))
          
          (values (intern (string-upcase type-str) :keyword) content))))))

;;; ---------------------------------------------------------------------------
;;; Stop Server (for cleanup)
;;; ---------------------------------------------------------------------------

(defun stop-bridge-server ()
  "Stop any running bridge server threads."
  (dolist (thread (bt:all-threads))
    (when (or (search "acl2-bridge" (bt:thread-name thread))
              (search "bridge-worker" (bt:thread-name thread)))
      (log:info "Stopping bridge thread: ~A" (bt:thread-name thread))
      (ignore-errors (bt:destroy-thread thread)))))
