
(require 'request)
(require 'subr-x)

(defvar emacsgpt-api-key nil
  "API key for OpenAI. Set this variable or the environment variable OPENAI_API_KEY.")

(defvar emacsgpt-api-model "gpt-3.5-turbo"
  "Model name for OpenAI API.")

(defvar emacsgpt-api-endpoint (concat "https://api.openai.com/v1/chat/completions")
  "Endpoint for OpenAI API.")

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

(defun emacsgpt-query (input)
  "Send INPUT to the OpenAI API and return the response."
  ;; Construct headers
  (let ((headers `(("Content-Type" . "application/json")
                   ("Authorization" . ,(concat "Bearer " (emacsgpt-api-key-get)))))
        (request-data (json-encode `(("model" . ,emacsgpt-api-model)
                                     ("messages" . ((("role" . "user") ("content" . ,input))))))))
    (request emacsgpt-api-endpoint
      :method "POST"
      :headers headers
      :data request-data
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (emacsgpt-handle-response data input)))  ; Include 'input' in the lambda closure
      :error (cl-function
              (lambda (&rest args &key error-thrown &allow-other-keys)
                (message "Got error: %S" error-thrown))))))

(defun emacsgpt-query (input)
  "Send INPUT to the OpenAI API and return the response."
  ;; Construct headers
  (with-current-buffer (switch-to-buffer-other-window (get-buffer-create "*emacsgpt*"))
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (format "\n\n------------------------------------- INPUT ------------------------------------\n\n%s\n" input))  ; Insert input
      (inferior-emacsgpt-mode)))
  (let ((headers `(("Content-Type" . "application/json")
                   ("Authorization" . ,(concat "Bearer " (emacsgpt-api-key-get)))))
        (request-data (json-encode `(("model" . ,emacsgpt-api-model)
                                     ("messages" . ((("role" . "user") ("content" . ,input))))))))
    (emacsgpt-log (format "Request url: %s, headers: %s, data: %s" emacsgpt-api-endpoint headers request-data))
    (request emacsgpt-api-endpoint
      :method "POST"
      :headers headers
      :data request-data
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (emacsgpt-handle-response data)))
      :error (cl-function
              (lambda (&rest args &key error-thrown &allow-other-keys)
                (message "Got error: %S" error-thrown))))))

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
  (let* ((region-input (string-trim (buffer-substring-no-properties start end)))
         (user-input (read-string "Message: "))
         (combined-input (concat region-input "\n\n" user-input)))
    (if (string= "" (string-trim combined-input))
        (message "Input is empty.")
      (emacsgpt-query combined-input))))

(defun emacsgpt-eval-paragraph ()
  "Evaluate the current paragraph using the OpenAI API."
  (interactive)
  (save-excursion  ; Preserve the point's original position
    (let* ((start (progn (backward-paragraph) (point)))  ; Find start of the paragraph
           (end (progn (forward-paragraph) (point))))    ; Find end of the paragraph
      (emacsgpt-eval-region start end))))

(defun emacsgpt-eval-message ()
  "Send a message to OpenAI API."
  (interactive)
  (let* ((user-input (read-string "Message: ")))
    (if (string= "" (string-trim user-input))
        (message "Input is empty.")
      (emacsgpt-query user-input))))

(define-minor-mode emacsgpt-mode
  "A minor mode to interact with OpenAI's Emacsgpt."
  :lighter " Emacsgpt"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c c r") 'emacsgpt-eval-region)
            (define-key map (kbd "C-c c p") 'emacsgpt-eval-paragraph)
            (define-key map (kbd "C-c c m") 'emacsgpt-eval-message)
            map))

(define-derived-mode inferior-emacsgpt-mode fundamental-mode "Inferior Emacsgpt"
  "Major mode for Emacsgpt interaction."
  (setq-local font-lock-defaults '(inferior-emacsgpt-font-lock-keywords))
  (setq-local read-only t))

(defvar inferior-emacsgpt-font-lock-keywords
  '((".*INPUT.*" . font-lock-function-name-face)
    (".*OUTPUT.*" . font-lock-variable-name-face)))

(defun emacsgpt--turn-on ()
  "Turn on `emacsgpt-mode` in the current buffer if appropriate."
  (unless (or (minibufferp)  ; Don't enable in minibuffers
              (not (derived-mode-p 'text-mode 'prog-mode)))  ; Only enable in text and prog modes
    (emacsgpt-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-emacsgpt-mode
  emacsgpt-mode emacsgpt--turn-on)

(provide 'emacsgpt-mode)
