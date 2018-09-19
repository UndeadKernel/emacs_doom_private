;; -*- no-byte-compile: t; -*-
;;; private/boy/packages.el

(package! goto-last-change)
(package! resize-window)
(package! www-synonyms)
(package! ag)

(package! ob-ipython :recipe (:fetcher github :repo "fuxialexander/ob-ipython" :files ("*.el" "*.py")))
