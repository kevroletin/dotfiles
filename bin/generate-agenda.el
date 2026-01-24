(package-initialize)

(require 'org)
(require 'org-agenda)

(add-to-list 'org-modules 'org-habit)

(setq org-agenda-files (list "~/org/personal"))

(setq org-capture-templates
      '(("t" "(W) New task" entry (file "~/org/work/refile.org")
         "* NEW %?\n  %i\n")
        ("n" "(W) Note" entry (file "~/org/work/refile.org")
         "* %?\n  %i\n")
        ("T" "(H) New task" entry (file "~/org/personal/refile.org")
         "* NEW %?\n  %i\n")
        ("N" "(H) Note" entry (file "~/org/personal/refile.org")
         "* %?\n  %i\n")))

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEW(n)"  "|" "DONE(d@/!)")
              (sequence "WAITING(w@/!)" "MAYBE(h@/!)" "|" "CANCELLED(c@/!)"))))

(setq org-todo-keyword-faces
      (quote (("TODO"      :foreground "green"  :weight bold)
              ("NEW"       :foreground "red"    :weight bold)
              ("DONE"      :foreground "gray"   :weight bold)
              ("WAITING"   :foreground "orange" :weight bold)
              ("MAYBE"     :foreground "yellow" :weight bold)
              ("CANCELLED" :foreground "gray"   :weight bold))))

;;
;; htmlize doesn't work properly in batch mode
;;
;; Solarized light
;; (custom-set-faces
;;  '(default ((t (:foreground "#586e75" :background "#fdf6e3")))))
;;
;; Solarized dark
(custom-set-faces
  '(default ((t (:foreground "#93a1a1" :background "#002b36")))))

(add-to-list 'load-path "/home/behemoth/bin/")
(require 'htmlize)

;; Use this to play with css styles
;; (setq org-export-htmlize-output-type 'css)

(org-agenda-list nil nil 1)
(org-agenda-write  "/tmp/agenda.html")

(provide 'generate-agenda)
