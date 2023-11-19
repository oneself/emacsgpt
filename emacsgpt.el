
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

  ;;;;;;;;;;;;;;;
 ;; UTILITIES ;;
;;;;;;;;;;;;;;;

(defun emacsgpt-api-key-get ()
  "Get the Emacsgpt API key."
  (or emacsgpt-api-key
      (getenv "OPENAI_API_KEY")))

(defun emacsgpt-log (content)
  "Log CONTENT to the *emacsgpt log* buffer."
  (with-current-buffer (get-buffer-create "*emacsgpt log*")
    (goto-char (point-max))  ; Move to the end of the buffer
    (insert content)         ; Insert the content
    (insert "\n")))          ; Add a newline for readability

  ;;;;;;;;;;
 ;; CHAT ;;
;;;;;;;;;;

(defun emacsgpt-print-input (context input)
  "Print the CONTEXT and INPUT to emacsgpt buffer."
  (with-current-buffer (switch-to-buffer-other-window (get-buffer-create "*emacsgpt*"))
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
      (emacsgpt-log (format "Request url: %s, headers: %s, data: %s" emacsgpt-api-chat-endpoint headers request-data))
      ;; Print the context
      (emacsgpt-print-input context-tr input-tr)
      (request emacsgpt-api-chat-endpoint
        :method "POST"
        :headers headers
        :data request-data
        :parser 'json-read
        :success (cl-function
                  (lambda (&key data &allow-other-keys)
                    (emacsgpt-handle-response data)))
        :error (cl-function
                (lambda (&rest args &key error-thrown &allow-other-keys)
                  (emacsgpt-log error-thrown)
                  (message "Got error: %S" error-thrown)))))))

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
    (with-current-buffer (get-buffer-create "*emacsgpt*")
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
        (let ((start (point)))
          (outline-end-of-subtree)
          (emacsgpt-eval-region start (point))))
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
