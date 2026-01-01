(require :asdf)
#-quicklisp (load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
(asdf:load-system :acl2-mcp-bridge)

;; Client A defines my-fn
(let ((acl2-mcp-bridge:*current-session* (acl2-mcp-bridge:get-or-create-session "client-A")))
  (acl2-mcp-bridge:cl-eval "(defun my-fn (x) (* x 2))")
  (format t "~%CLIENT-A: defined my-fn~%"))

;; Client B tries to call my-fn - should fail (not defined in their session)
(let ((acl2-mcp-bridge:*current-session* (acl2-mcp-bridge:get-or-create-session "client-B")))
  (multiple-value-bind (res err msg) (acl2-mcp-bridge:cl-eval "(my-fn 5)")
    (format t "CLIENT-B: err=~A msg=~A~%" err msg)))

;; Client A can still call it
(let ((acl2-mcp-bridge:*current-session* (acl2-mcp-bridge:get-session "client-A")))
  (multiple-value-bind (res err msg) (acl2-mcp-bridge:cl-eval "(my-fn 10)")
    (format t "CLIENT-A: result=~A~%" res)))

(format t "~%ALL-SESSIONS: ~A~%" (acl2-mcp-bridge:list-all-sessions))
