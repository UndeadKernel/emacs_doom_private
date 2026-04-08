;;; config.el --- The Configuration of the Boy -*- lexical-binding: t; -*-

;; ---------------------------------------------------------------------------------
;; Personal Configuration
;; ---------------------------------------------------------------------------------
;; Private things others should not see ;D
(load! "+private" nil t) ; do not complain if file does not exist
;; Personalized functions
(load! "+functions")
;; Anything that modifies the way popups spawn
(load! "+popups")
;; Theme related things
(load! "+themes")
;; Different config for different systems
(load! "+systems")
;; Patches of functions to fix other packages
(load! "+patches")
;; Configuration of vanilla emacs features
(load! "+vanilla")

;; ---------------------------------------------------------------------------------
;; Tools Modules Configuration
;; ---------------------------------------------------------------------------------
(load! "+tools")

;; ---------------------------------------------------------------------------------
;; Lang Modules Configuration
;; ---------------------------------------------------------------------------------
(load! "+lang")
(load! "+latex")
(load! "+org")
(load! "+checkers")

;; ---------------------------------------------------------------------------------
;; Config Modules Configuration
;; ---------------------------------------------------------------------------------
(load! "+default")

;; ---------------------------------------------------------------------------------
;; Completion Modules Configuration
;; ---------------------------------------------------------------------------------
(load! "+completion")

;; ---------------------------------------------------------------------------------
;; UI Modules Configuration
;; ---------------------------------------------------------------------------------
(load! "+ui")

;; ---------------------------------------------------------------------------------
;; Editor Modules Configuration
;; ---------------------------------------------------------------------------------
(load! "+editor")

;; ---------------------------------------------------------------------------------
;; Emacs Modules Configuration
;; ---------------------------------------------------------------------------------
(load! "+emacs")
;; Load personalized bindings
(load! "+bindings")

;; ---------------------------------------------------------------------------------
;; Term Modules Configuration
;; ---------------------------------------------------------------------------------
(load! "+term")

;; ---------------------------------------------------------------------------------
;; Custom Package Configuration
;; ---------------------------------------------------------------------------------
;; Configure all extra custom packages
(load! "+extra")
