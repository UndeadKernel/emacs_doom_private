;;; init.el -*- lexical-binding: t; -*-

;; Font setup
(setq
      doom-font (font-spec :family "Fira Mono" :size 14)
      doom-variable-pitch-font (font-spec :family "Fira Sans")
      doom-unicode-font (font-spec :family "DejaVu Sans Mono")
      doom-big-font (font-spec :family "Fira Mono" :size 19))

;; Prevents the unstyled mode-line flash at startup
(setq-default mode-line-format nil)


;; Select popup buffers by default
(defvar +popup-defaults
  (list :side   'bottom
        :height 0.16
        :width  40
        :quit   t
        :select t
        :ttl    5)
  "Default properties for popup rules defined with `set-popup-rule!'.")

(doom! :feature
      ;debugger          ; FIXME stepping through code, to help you add bugs
       eval              ; run code, run (also, repls)
      ;evil              ; come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       (lookup           ; helps you navigate your code and documentation
        +devdocs         ; ...on devdocs.io online
        +docsets)        ; ...or in Dash docsets locally
       snippets          ; my elves. They type so I don't have to
       spellcheck        ; tasing you for misspelling mispelling
       syntax-checker    ; tasing you for every semicolon you forget
       workspaces        ; tab emulation, persistence & separate workspaces

       :completion
       company           ; the ultimate code completion backend
       ivy               ; a search engine for love and life
      ;helm              ; the *other* search engine for love and life
      ;ido               ; the other *other* search engine...

       :ui
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       doom-modeline     ; a snazzy Atom-inspired mode-line
       doom-quit         ; DOOM quit-message prompts when you quit Emacs
       hl-todo           ; highlight TODO/FIXME/NOTE tags
       nav-flash         ; blink the current line after jumping
       neotree           ; a project drawer, like NERDTree for vim
      ;evil-goggles      ; display visual hints when editing in evil
      ;unicode           ; extended unicode support for various languages
      ;pretty-code       ; replace bits of code with pretty symbols
      ;tabbar            ; FIXME an (incomplete) tab bar for Emacs
       (popup            ; tame sudden yet inevitable temporary windows
        +all             ; catch all popups that start with an asterix
        +defaults)       ; default popup rules
       vc-gutter         ; vcs diff in the fringe
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       (window-select +switch-window)  ; visually switch windows

       :editor
      ;parinfer          ; turn lisp into python, sort of
       rotate-text       ; cycle region at point between text candidates

       :emacs
       dired             ; making dired pretty [functional]
       ediff             ; comparing files in Emacs
       electric          ; smarter, keyword-based electric-indent
       eshell            ; a consistent, cross-platform shell (WIP)
       imenu             ; an imenu sidebar and searchable code index
       term              ; terminals in Emacs
       vc                ; version-control and Emacs, sitting in a tree

       :tools
       ein
       gist              ; interacting with github gists
      ;macos             ; MacOS-specific commands
       make              ; run make tasks from Emacs
       magit
       password-store    ; password manager for nerds
       pdf               ; pdf enhancements
      ;rgb
       tmux              ; an API for interacting with tmux
       upload            ; map local to remote projects via ssh/ftp

       :lang
       ;; assembly          ; assembly for fun or debugging
       ;; cc                ; C/C++/Obj-C madness
       ;; crystal           ; ruby at the speed of c
       ;; clojure           ; java with a lisp
       ;; csharp            ; unity, .NET, and mono shenanigans
       ;; data              ; config/data formats
       ;; erlang            ; an elegant language for a more civilized age
       ;; elixir            ; erlang done right
       ;; elm               ; care for a cup of TEA?
       emacs-lisp        ; drown in parentheses
       ;; go                ; the hipster dialect
       ;; (haskell +intero) ; a language that's lazier than I am
       ;; hy                ; readability of scheme w/ speed of python
       ;; (java +meghanada) ; the poster child for carpal tunnel syndrome
       ;; javascript        ; all(hope(abandon(ye(who(enter(here))))))
       ;; julia             ; a better, faster MATLAB
       (latex +latexmk +pdf-tools)  ; writing papers in Emacs has never been so fun
       ;; ledger            ; an accounting system in Emacs
       ;; lua               ; one-based indices? one-based indices
       markdown          ; writing docs for people to ignore
       ;; ocaml             ; an objective camell
       ;; nix               ; I hereby declare "nix geht mehr!"
       (org              ; organize your plain life in plain text
        +attach          ; custom attachment system
        +babel           ; running code in org
        +capture         ; org-capture in and outside of Emacs
        +export          ; Exporting org to whatever you want
        +present         ; Emacs for presentations
        +publish
        +right-popup
        +ipython)        ; Emacs+Org as a static site generator
       ;; perl              ; write code no one else can comprehend
       ;; php               ; make php less awful to work with
       ;; plantuml          ; diagrams for confusing people more
       ;; purescript        ; javascript, but functional
       python            ; beautiful is better than ugly
       ;; rest              ; Emacs as a REST client
       ;; ruby              ; 1.step do {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       ;; rust              ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       ;; scala             ; java, but good
       sh                ; she sells (ba|z)sh shells on the C xor
       ;; swift             ; who asked for emoji variables?
       ;; typescript        ; javascript, but better
       ;; web               ; the tubes

       ;; Applications are complex and opinionated modules that transform Emacs
       ;; toward a specific purpose. They may have additional dependencies and
       ;; should be loaded late.
       :app
      ;email             ; emacs as an email client
      ;irc               ; how neckbeards socialize
      ;rss               ; emacs as an RSS reader
      ;twitter           ; twitter client https://twitter.com/vnought
      ;write             ; emacs as a word processor (latex + org + markdown)
      ;pacmanfiles
      pacfiles

       :collab
      ;floobits          ; peer programming for a price
      ;impatient-mode    ; show off code over HTTP
       )
