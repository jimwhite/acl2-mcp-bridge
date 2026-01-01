;;; Configure ASDF to prefer our local vendor directories
(push (merge-pathnames "vendor/40ants-mcp/" 
                       (asdf:system-source-directory :acl2-mcp-bridge))
      asdf:*central-registry*)
