(cd "~/zebu")
(load "ZEBU-init.lisp")
(zb:zebu-compiler :compiled nil)
(setq zb::*grammar-debug* t)
(zb:read-parser "Program := KB-domain: [(-stmts KB-Sequence)];"
  		:grammar (zb:find-grammar "zebu-mg"))
