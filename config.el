;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;

;; org mode config
(after! org
  (setq org-agenda-files '("~/opensource_projects/org-mode/files/agenda.org")))

;; org-roam config
(use-package! websocket
  :after org-roam
  (setq org-roam-directory "~/projects/fc-notes"))

(use-package! org-roam-ui
  :after org-roam ;; or :after org
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))


;; deft config
(setq deft-directory "~/projects/fc-notes"
      deft-extesions '("org")
      deft-recursive t)

;; elpy config
(use-package elpy
  :config
  (elpy-enable))

;; Copilot config
;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("C-`" . 'copilot-accept-completion)
              ("fn-TAB" . 'copilot-accept-completion)
              ;;("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))


;; custom config
(setq doom-theme 'tango)
(setq display-line-numbers-type 'relative)

;; Start and Stop timer in emacs with user input time
;; Add this to your ~/.doom.d/config.el

(defvar my-timer nil
  "Variable to store the current timer.")

(defvar my-timer-sound-file "~/.config/doom/sounds/alarm.wav"
  "Path to the sound file to play when the timer ends.")

(defun my-play-timer-sound ()
  "Play a sound to indicate the timer is up."
  (when (file-exists-p my-timer-sound-file)
    (play-sound-file my-timer-sound-file)
    (message "Time's up!")))

(defun start-timer (duration)
  "Start a timer for the specified DURATION that displays a message and plays a sound when time is up."
  (interactive "sEnter timer duration (e.g., 40 min, 1 hour, 30 sec): ")
  (when my-timer
    (cancel-timer my-timer)
    (setq my-timer nil))
  (setq my-timer (run-at-time duration nil (lambda ()
                                             (my-play-timer-sound)
                                             (setq my-timer nil))))
  (message "Timer started for %s. Timer ID: %s" duration my-timer))

(defun stop-timer ()
  "Stop the current timer if it is running."
  (interactive)
  (if my-timer
      (progn
        (cancel-timer my-timer)
        (setq my-timer nil)
        (message "Timer stopped."))
    (message "No active timer to stop.")))

(map! :leader
      :desc "Start timer"
      "t t" #'start-timer)

(map! :leader
      :desc "Stop timer"
      "t s" #'stop-timer)

;; elfeed config
(after! elfeed
  (setq elfeed-search-filter "@1-month-ago +unread"))
