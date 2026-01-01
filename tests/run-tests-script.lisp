;;; Standalone test runner for sbcl --script
(require 'asdf)
(asdf:initialize-source-registry)

;; Load Quicklisp if present
(let* ((home (user-homedir-pathname))
       (ql-setup (when home (merge-pathnames "quicklisp/setup.lisp" home))))
  (when (and ql-setup (probe-file ql-setup))
    (load ql-setup)))

;; Quickload the test system (brings in code + FiveAM)
(cond
  ((find-package '#:ql)
   (ql:quickload :acl2-mcp-bridge/tests :silent t))
  (t
   (asdf:load-system :acl2-mcp-bridge/tests)))

(acl2-mcp-bridge/tests:run-tests)
(uiop:quit 0)
