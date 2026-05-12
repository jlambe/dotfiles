;;; -*- lexical-binding: t -*-

;; Collect "diagnostic objects":
;; - problem position
;; - problem type
;; - problem description

;; Can create a diagnostic object by using the
;; "flymake-make-diagnostic" function.

;; Flow:
;; Flymake makes requests to backend throug the "backend function"
;; I think backends return a callback back to Flymake to process into the buffer.

;; Backend-function:
;; 1st argument is REPORT-FN, a callback function.
;; The function should accept a number of keyword-value pairs:
;; (:KEY VALUE :KEY2 VALUE2 ...)
;; The following keywords may be passed by Flymake to the backend function:
;; (:recent-changes ()
;;  :changes-start ()
;;  :changes-end ())

;; Backend function must be fast or signal an error, otherwise it is being disabled.

;; If function returns, Flymake considers the backend to be "running".
;; If not done, the backend is expected to call the REPORT-FN, and Flymkae considers the backend to be "reporting".
;; Backend call REPORT-FN by passing it a single argument REPORT-ACTION followed by an optional list of keyword-value pairs:
;; (:REPORT-KEY VALUE :REPORT-KEY2 VALUE2...)

;; Accepted values with REPORT-ACTION:
;; - A (possibly empty) list of diagnostic objects
;; - The symbol :panic, signaling backend got an exceptional situation and should be disabled

;; Accepted REPORT-KEY arguments:
;; - :explanation (string)
;; - :force (boolean)
;; - :region (BEG.END)

;; Create a diagnostic object using "flymake-make-diagnostic".

(require 'cl-lib)
(require 'flymake)

(defgroup eslint-flymake nil
  "Flymake backend for ESLint"
  :group 'programming
  :prefix "eslint-flymake-")

(defvar-local eslint-flymake-proc nil)

;; "resources/js/staff/app/products/fieldsOfStudy/views/Edit.tsx"
;; "--stdin-filename"

(defcustom eslint-flymake-command '("pnpm" "eslint" "--no-color" "--stdin")
  "The `eslint' command along with the arguments it should be called with."
  :type '(repeat  string)
  :group 'eslint-flymake)

(defvar eslint-flymake-regexp
  (rx-to-string
   '(seq (group (group (+ digit)) ":" (group (+ digit)))
         (+ " ") (group (or "error" "warning"))
         (group  (1+ any))
         blank
         (group (1+  any))
         eol)))

;; (source-path (file-truename (buffer-file-name)))
;; (dir (project-root (project-current)))
;; (relative-path (file-relative-name source-path dir)) -> calling this function breaks flymake backend
;; (buffer-file-name) -> fails also

;; (buffer-file-name (current-buffer))
;; (file-relative-name (buffer-file-name (current-buffer)))

(defun jl-flymake-eslint (report-fn &rest _args)
    "ESLint flymake backend function."
    (when (process-live-p eslint-flymake-proc)
    (kill-process eslint-flymake-proc))

  (let* ((source-buffer (current-buffer))
	(source-buffer-path (file-relative-name (buffer-file-name source-buffer))))
    (save-restriction
      (widen)
      (setq eslint-flymake-proc
            (make-process :name "eslint-flymake"
                          :noquery t
                          :connection-type 'pipe
                          :buffer (generate-new-buffer "*eslint-flymake*")
                          :command `("pnpm" "eslint" "--no-color" "--stdin" "--stdin-filename" ,(or source-buffer-path (buffer-name source-buffer)))
                          :sentinel (lambda (proc _event)
                                      (when (memq (process-status proc) '(signal exit))
                                        (unwind-protect
                                            (if (with-current-buffer source-buffer (eq proc eslint-flymake-proc))
                                                (with-current-buffer (process-buffer proc)
                                                  (goto-char (point-min))
                                                  (cl-loop
                                                   while (search-forward-regexp eslint-flymake-regexp nil t)
                                                   for (beg . end) = (flymake-diag-region source-buffer
                                                                                          (string-to-number (match-string 2))
                                                                                          (string-to-number (match-string 3)))
                                                   for type = (pcase (match-string 4)
                                                                ("warning" :warning)
                                                                ("error" :error)
                                                                (_ :note))
                                                   for msg = (match-string 5)
                                                   collect (flymake-make-diagnostic source-buffer
                                                                                    beg end
                                                                                    type msg)
                                                   into diags
                                                   finally (funcall report-fn diags)))
                                              (flymake-log :warning "Canceling obsolete check %s"
                                                           proc))
                                          (kill-buffer (process-buffer proc)))))))

      (process-send-region eslint-flymake-proc (point-min) (point-max))
      (process-send-eof eslint-flymake-proc))))

(defun jl-flymake-setup-eslint-backend ()
  "ESLint flymake backend setup."
  (add-hook 'flymake-diagnostic-functions #'jl-flymake-eslint nil t))

;; (add-hook 'tsx-ts-mode-hook #'jl-flymake-setup-eslint-backend)

;; Eglot hack
;; (add-to-list 'eglot-stay-out-of 'flymake)

;; (defun manually-activate-flymake ()
;;   "Manuall activate flymake support on eglot."
;;   (add-hook 'flymake-diagnostic-functions #'eglot-flymake-backend nil t)
;;   (flymake-mode 1))

;;  (add-hook 'eglot-managed-mode-hook #'manually-activate-flymake nil t)

(provide 'jl-flymake)
