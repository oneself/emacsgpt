;;; emacsgpt.el --- Interact with OpenAI API from Emacs

;; Copyright (C) 2023 Eyal Erez

;; Author: Eyal Erez <eyal@agilewanderer.com>
;; Maintainer: Eyal Erez <eyal@agilewanderer.com>
;; Created: 13 Nov 2023

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This Emacs mode allows interacting with ChatGPT directly from any buffer.

;; Configuration:
;; 1. Set your OpenAI API key as an environment variable in either .bashrc or .zshrc:
;;    Ex: 'export OPENAI_API_KEY=<API_KEY>'
;; 2. Add the following to your `~/.emacs.d/init.el`:
;;    ;; Ensure request.el is installed. This can be done via (use-package request) or manually.
;;    (load "<PATH>/emacsgpt.el")        ; Load this package
;;    (setq emacsgpt-api-model "gpt-4")  ; Set the API model to be used (default is "gpt-3.5-turbo")
;;    (global-emacsgpt-mode t)           ; Enable the minor mode globally


(require 'request)
(require 'subr-x)

;(setq debug-on-error t)

  ;;;;;;;;;;;;;;;
 ;; VARIABLES ;;
;;;;;;;;;;;;;;;

(defvar emacsgpt-api-key nil
  "API key for OpenAI. Set this variable or the environment variable OPENAI_API_KEY.")

(defvar emacsgpt-api-model "gpt-4"
  "Model name for OpenAI API.")

(defvar emacsgpt-api-chat-endpoint "https://api.openai.com/v1/chat/completions"
  "Endpoint for OpenAI API.")

(defvar emacsgpt-print-line-num 5
  "Number of lines to print in the *emacsgpt* buffer.")

(defvar emacsgpt-buffer "*emacsgpt*"
  "Name of the *emacsgpt* buffer.")

(defvar emacsgpt-buffer-log "*emacsgpt log*"
  "Name of the *emacsgpt log* buffer.")


  ;;;;;;;;;;;;;;;
 ;; UTILITIES ;;
;;;;;;;;;;;;;;;

(defun emacsgpt-api-key-get ()
  "Get the Emacsgpt API key."
  (or emacsgpt-api-key
      (getenv "OPENAI_API_KEY")))

(defun emacsgpt-get-create-switch-buffer (&optional sw)
  "Get the *emacsgpt* buffer, creating it if it doesn't exist, and switch to it."
  (interactive)
  (unless sw (setq sw t))
  (let* ((buffer (get-buffer-create emacsgpt-buffer)))
    (when sw
      (switch-to-buffer-other-window buffer))
    buffer))

(defun emacsgpt-log (content)
  "Log CONTENT to the *emacsgpt log* buffer."
  (with-current-buffer (get-buffer-create emacsgpt-buffer-log)
    (goto-char (point-max))  ; Move to the end of the buffer
    (insert content)         ; Insert the content
    (insert "\n")))          ; Add a newline for readability



  ;;;;;;;;;;
 ;; CHAT ;;
;;;;;;;;;;

(defun emacsgpt-print-input (context input)
  "Print the CONTEXT and INPUT to emacsgpt buffer."
  (with-current-buffer (emacsgpt-get-create-switch-buffer)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (when (not (string-empty-p context))  ; check if context is not empty
        (let* ((input-lines (split-string context "\n"))
               (first-lines (cl-subseq input-lines 0 (min emacsgpt-print-line-num (length input-lines))))
               (remaining-lines (- (length input-lines) emacsgpt-print-line-num)))
          (insert (format "\n\n------------------------------------ CONTEXT -----------------------------------\n\n%s\n"
                          (string-join first-lines "\n")))
          (when (> remaining-lines 0)
            (insert (format "\n[%d more lines]\n" remaining-lines)))))
      (insert (format "\n------------------------------------- INPUT ------------------------------------\n\n%s\n"
                      input)))
    (inferior-emacsgpt-mode)))


(defun emacsgpt-query (context input)
  "Send CONTEXT and INPUT to the OpenAI API and return the response."
  ;; Construct headers and request data
  (let* ((context-tr (string-trim context))
         (input-tr (string-trim input))
         (combined-input (concat context-tr "\n\n" input-tr))
         (headers `(("Content-Type" . "application/json")
                    ("Authorization" . ,(concat "Bearer " (emacsgpt-api-key-get)))))
         (request-data (json-encode `(("model" . ,emacsgpt-api-model)
                                      ("messages" . ((("role" . "user") ("content" . ,combined-input))))))))
    (if (and (string-empty-p context) (string-empty-p input))
        (message "Input is empty")
      (emacsgpt-log (format "Request: url: %s, headers: %s, data: %s" emacsgpt-api-chat-endpoint headers request-data))
      ;; Print the context
      (emacsgpt-print-input context-tr input-tr)
      (request emacsgpt-api-chat-endpoint
        :method "POST"
        :headers headers
        :data request-data
        :parser 'json-read
        :success (cl-function
                  (lambda (&key data &allow-other-keys)
                    (emacsgpt-log (format "Response: %S" data))
                    (emacsgpt-handle-response data)))
        :error (cl-function
                (lambda (&rest args &key error-thrown &allow-other-keys)
                  (emacsgpt-log (format ("Error: %S" error-thrown)))
                  (message "Error: %S" error-thrown)))))))

(defun extract-content-from-response (response)
  "Extract the 'content' value from the given RESPONSE structure."
  (let* ((choices (cdr (assoc 'choices response)))  ; Extract the 'choices' list
         (first-choice (aref choices 0))            ; Get the first element of the vector
         (message (cdr (assoc 'message first-choice))) ; Extract the 'message' alist
         (content (cdr (assoc 'content message))))  ; Extract the 'content' value
    content))

(defun emacsgpt-handle-response (data)
  "Handle the response from the Emacsgpt API, given DATA and INPUT."
  (let* ((output (extract-content-from-response data)))
    (with-current-buffer (emacsgpt-get-create-switch-buffer nil)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (format "\n\n------------------------------------- OUTPUT -----------------------------------\n\n%s\n" output))
        (goto-char (point-max))
        (inferior-emacsgpt-mode)))))


(defun emacsgpt-eval-region (start end)
  "Evaluate the region from START to END using the OpenAI API."
  (interactive "r")
  (let* ((context (buffer-substring-no-properties start end))
         (input (read-string "Message: ")))
    (emacsgpt-query context input)))

(defun emacsgpt-eval-paragraph ()
  "Evaluate the current paragraph using the OpenAI API."
  (interactive)
  (save-excursion  ; Preserve the point's original position
    (let* ((start (progn (backward-paragraph) (point)))  ; Find start of the paragraph
           (end (progn (forward-paragraph) (point))))    ; Find end of the paragraph
      (emacsgpt-eval-region start end))))

(defun emacsgpt-eval-buffer ()
  "Evaluate the current buffer using the OpenAI API by calling `emacsgpt-eval-region`."
  (interactive)
  ;; Use `point-min` and `point-max` to get the start and end of the buffer
  (emacsgpt-eval-region (point-min) (point-max)))

(defun emacsgpt-eval-org (&optional levels)
  "Evaluate the current Org mode heading and its content using the OpenAI API.
Optional argument LEVELS indicates the number of parent org levels to include."
  (interactive "p")
  (if (eq major-mode 'org-mode)
      (save-excursion
        (while (and (> levels 0) (org-up-heading-safe)) ; Go up by 'levels' number of headings
          (setq levels (1- levels)))
        (let* ((element (org-element-at-point)) ; Get the org element at the current point
               (start (org-element-property :begin element)) ; The start point is the beginning of the element
               (end (org-element-property :end element))) ; The end point is the end of the element
          (emacsgpt-eval-region start end)))
    (message "Not in org-mode")))

(defun emacsgpt-eval-org-0 ()
  "Evaluate the current Org mode heading and its content using the OpenAI API."
  (interactive)
  (emacsgpt-eval-org 0))

(defun emacsgpt-eval-org-1 ()
  "Evaluate the current Org mode heading and its content including one level of parent org levels using the OpenAI API."
  (interactive)
  (emacsgpt-eval-org 1))

(defun emacsgpt-eval-org-2 ()
  "Evaluate the current Org mode heading and its content including two levels of parent org levels using the OpenAI API."
  (interactive)
  (emacsgpt-eval-org 2))


(defun emacsgpt-eval-message ()
  "Send a message to OpenAI API."
  (interactive)
  (let* ((context (read-string "Message: ")))
      (emacsgpt-query "" context)))

  ;;;;;;;;;;
 ;; MODE ;;
;;;;;;;;;;

(define-minor-mode emacsgpt-mode
  "A minor mode to interact with OpenAI's Emacsgpt."
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c c s") 'emacsgpt-get-create-switch-buffer)
            (define-key map (kbd "C-c c r") 'emacsgpt-eval-region)
            (define-key map (kbd "C-c c b") 'emacsgpt-eval-buffer)
            (define-key map (kbd "C-c c p") 'emacsgpt-eval-paragraph)
            (define-key map (kbd "C-c c m") 'emacsgpt-eval-message)
            (define-key map (kbd "C-c c 0") 'emacsgpt-eval-org-0)
            (define-key map (kbd "C-c c 1") 'emacsgpt-eval-org-1)
            (define-key map (kbd "C-c c 2") 'emacsgpt-eval-org-2)
            map))

(define-derived-mode inferior-emacsgpt-mode markdown-mode "Inferior Emacsgpt"
  "Major mode for Emacsgpt interaction."
  ;; Combine markdown-mode's font-lock keywords with custom ones
  (setq-local font-lock-defaults `(,(append markdown-mode-font-lock-keywords
                                            '((".*CONTEXT.*" . font-lock-function-name-face)
                                              (".*INPUT.*"   . font-lock-function-name-face)
                                              (".*OUTPUT.*"  . font-lock-variable-name-face)))))
  ;; Make the buffer read-only
  (setq-local buffer-read-only t)
  ;; Set local key binding for 'q' to quit-window
  (local-set-key (kbd "q") 'quit-window))

(defun emacsgpt--turn-on ()
  "Turn on `emacsgpt-mode` in the current buffer if appropriate."
  (unless (or (minibufferp)  ; Don't enable in minibuffers
              (not (derived-mode-p 'text-mode 'prog-mode)))  ; Only enable in text and prog modes
    (emacsgpt-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-emacsgpt-mode
  emacsgpt-mode emacsgpt--turn-on)

(provide 'emacsgpt-mode)
