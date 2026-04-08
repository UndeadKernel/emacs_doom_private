;;; +extra.el -*- lexical-binding: t; -*-

;; Show me where I made the last change in a document.
(use-package! goto-last-change
  :commands goto-last-change)

;; Resize windows interactively.
(use-package! resize-window
  :commands (resize-window))

;; (use-package! jupyter
;;   ;; :load-path ("~/src/emacs-jupyter" "~/src/emacs-zmq")
;;   :after org
;;   :init
;;   (setq jupyter-eval-use-overlays t))


(use-package! pacfiles-mode
  :commands (pacfiles pacfiles-start)
  :config
  (set-popup-rule! "^\\*pacfiles.*" :ignore t))

(when (not (boundp '+bibliography-notes-dir))
  (setq +bibliography-notes-dir "~/documents/bib/"))

(use-package! command-log-mode
  :commands global-command-log-mode
  :init
  (setq command-log-mode-auto-show t
        command-log-mode-open-log-turns-on-mode t
        command-log-mode-window-font-size 0
        command-log-mode-window-size 80)
  :config
  (setq clm/log-command-exceptions*
        (append clm/log-command-exceptions*
                '(+boy/up-scroll
                  +boy/down-scroll))))

;; This variable should go inside of :init, but doesn't get called at the right time.
(setq lsp-tailwindcss-add-on-mode t)
(use-package! lsp-tailwindcss
  :when (modulep! +lsp)
  :after lsp-mode
  :config
  (setq lsp-tailwindcss-emmet-completions (featurep 'emmet-mode))

  ;; Advice that uses a locally installed rustywind binary.
  (defun +rustywind-with-local-bin (orig-fun &rest args)
    (let ((lsp-tailwindcss-rustywind-command
           (concat (projectile-project-root)
                   "node_modules/.bin/rustywind")))
      (apply orig-fun args)))
  (advice-add #'lsp-tailwindcss-rustywind :around #'+rustywind-with-local-bin))

(set-docsets! '(web-mode css-mode rjsx-mode typescript-tsx-mode) :add "Tailwind_CSS")

(use-package! org-modern-indent
  :config
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

(use-package! ragmacs
  :after gptel
  :init
  (gptel-make-preset 'ragmacs
    :pre (lambda () (require 'ragmacs))
    :system
    "You are pair programming with the user in Emacs and on Emacs.
 
 Your job is to dive into Elisp code and understand the APIs and
 structure of elisp libraries and Emacs. Use the provided tools to do
 so, but do not make duplicate tool calls for information already
 available in the chat.
 
 <tone>
 1. Be terse and to the point. Speak directly.
 2. Explain your reasoning.
 3. Do not hedge or qualify.
 4. If you don't know, say you don't know.
 5. Do not offer unprompted advice or clarifications.
 6. Never apologize.
 7. Do not summarize your answers.
 </tone>
 
 <code_generation>
 When generating code:
 1. Always check that functions or variables you use in your code exist.
 2. Check their calling convention and function-arity before you use them.
 3. Write code that can be tested by evaluation, and offer to evaluate
 code using the `elisp_eval` tool.
 </code_generation>
 
 <formatting>
 1. When referring to code symbols (variables, functions, tags etc) enclose them in markdown quotes.
    Examples: `read_file`, `getResponse(url, callback)`
    Example: `<details>...</details>`
 2. If you use LaTeX notation, enclose math in \( and \), or \[ and \] delimiters.
 </formatting>"
    :tools '("introspection"))
  :config
  ;; (setq gptel-tools 
  ;;         (list ragmacs-manuals 
  ;;               ragmacs-symbol-manual-node
  ;;               ragmacs-manual-node-contents
  ;;               ragmacs-function-source
  ;;               ragmacs-variable-source))
  )

(use-package! mcp
  :after gptel
  :custom
  (mcp-hub-servers
   `(("filesystem" .
      (:command "npx"
       :args ("-y" "@modelcontextprotocol/server-filesystem" "~/")))
     ("fetch" . (:command "uvx" :args ("mcp-server-fetch")))
     ("git" . (:command "uvx" :args ("mcp-server-git")))
     ("markitdown" . (:command "markitdown-mcp"))
     ("word" .
      (:command "/mnt/c/Users/CARC/AppData/Roaming/Python/Python313/Scripts/word_mcp_server.exe"
       :env (:MCP_AUTHOR "Carlos G. Cordero" :MCP_AUTHOR_INITIALS "CGC")))))
  :config
  (require 'mcp-hub))
