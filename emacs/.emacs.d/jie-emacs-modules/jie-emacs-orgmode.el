;;; Enable auto-fill-mode when working within an .org file.
(add-hook 'org-mode-hook 'auto-fill-mode)

;;; Setup default org-directory to be ~/Notes
(setq org-directory "~/Notes")

;;; Configure Org mode capture.
(setq org-default-notes-files (concat org-directory "/inbox.org"))

;;; Configure Org mode capture templates.
;;; Configure the "Idea" template:
;;; Primarily used to quick add list items in a single file.
;;; Anything, anytime, that comes up to my mind and that I would need to revisit in the future...
;;; Configure the "Journal" template:
;;; Personal journal entries, ordered by day/month/year.
(setq org-capture-templates
      '(("i" "Quicky capture an idea." item (file+headline "inbox.org" "Inbox") "")
        ;;; I would like new entries to remember to leave one line gap... except just under the headline.
        ;;; Currently, the ":empty-line-after" is adding an empty line after captured content...
        ;;; but when a new entry is appended, the last remaining empty line is ignored...
        ("j" "Personal journal entry." plain (file+datetree "journal.org") "[%U]\n%?"
         :time-prompt t
         :tree-type day
         :empty-lines-after 1)))

(provide 'jie-emacs-orgmode)
