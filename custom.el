(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(elfeed-feeds
   '("https://www.reddit.com/r/orgmode.rss" "https://oneofus.la/have-emacs-will-hack/feed.xml"))
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 25))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Org mode config
(setq org-roam-node-display-template
      (concat "${title:*} "
              (propertize "${tags:10}" 'face 'org-tag)))

;; org-todo
(with-eval-after-load 'org
  (let ((new-states '((sequence "SPIKE(s)" "IN-PROGRESS(I)" "REVIEW(R)" "|" "DELEGATED(D)"))))
    (dolist (new-state new-states)
      (add-to-list 'org-todo-keywords new-state t)))
  (let ((new-faces '(("SPIKE" . (:foreground "orange" :weight bold))
                     ("IN-PROGRESS" . (:foreground "blue" :weight bold))
                     ("REVIEW" . (:foreground "purple" :weight bold))
                     ("DELEGATED" . (:foreground "brown" :weight bold)))))
    (dolist (face new-faces)
      (add-to-list 'org-todo-keyword-faces face t))))

;; Convert md file to org file
(defun markdown-convert-buffer-to-org ()
  "Convert the current buffer's content from markdown to orgmode format and save it with the current buffer's file name but with .org extension."
  (interactive)
  (shell-command-on-region (point-min) (point-max)
                           (format "pandoc -f markdown -t org -o %s"
                                   (concat (file-name-sans-extension (buffer-file-name)) ".org"))))
