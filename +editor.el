;;; +editor.el -*- lexical-binding: t; -*-

(after! tramp
  ;; Disable backups when editing files through Tramp.
  (setq +boy/disable-tramp-backups '("ssh" "sftp")) ;; disable 'ssh' and 'sftp'
  (setq backup-enable-predicate
        (lambda (fname)
          (and (normal-backup-enable-predicate fname)
               ;; Disable tramp backups for the given methods
               (not (and +boy/disable-tramp-backups
                         (or
                          (member "all" +boy/disable-tramp-backups)
                          (member (file-remote-p fname 'method) +boy/disable-tramp-backups))))))))

(after! objed
  ;; Never start objed automatically
  (pushnew! objed-keeper-commands 'org-cycle 'org-todo '+boy/down-scroll '+boy/up-scroll 'recenter-top-bottom))

;; always indent with tab
(setq-default tab-always-indent t)
