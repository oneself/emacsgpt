
(require 'request)


(setq debug-on-error t)
(setq url-debug t)

(defvar emacsgpt-api-key nil
  "API key for OpenAI. Set this variable or the environment variable OPENAI_API_KEY.")

;;(defvar emacsgpt-api-model "gpt-4"
(defvar emacsgpt-api-model "gpt-3.5-turbo"
  "Model name for OpenAI API.")

;;(defvar emacsgpt-api-endpoint (concat "https://api.openai.com/v1/engines/" emacsgpt-api-model "/completions")
(defvar emacsgpt-api-endpoint (concat "https://api.openai.com/v1/chat/completions")
  "Endpoint for OpenAI API.")

(defun emacsgpt-api-key-get ()
  "Get the Emacsgpt API key."
  (or emacsgpt-api-key
      (getenv "OPENAI_API_KEY")))

(defun emacsgpt-log (msg)
  (with-current-buffer (get-buffer-create "*emacsgpt log*")
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (msg)))))


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
      (insert (format "\n\n------------------------------------- INPUT ------------------------------------\n%s\n" input))  ; Insert input
      (inferior-emacsgpt-mode)))
  (let ((headers `(("Content-Type" . "application/json")
                   ("Authorization" . ,(concat "Bearer " (emacsgpt-api-key-get)))))
        (request-data (json-encode `(("model" . ,emacsgpt-api-model)
                                     ("messages" . ((("role" . "user") ("content" . ,input))))))))
    (log (format "Request url: %s, headers: %s, data: %s" emacsgpt-api-endpoint headers request-data))
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
        (insert (format "------------------------------------- OUTPUT -----------------------------------\n\n%s\n" output))   ; Insert output
        (inferior-emacsgpt-mode)))))


(defun emacsgpt-eval-region (start end)
  "Evaluate the region from START to END using the Emacsgpt API."
  (interactive "r")
  (let* ((input (buffer-substring-no-properties start end)))
    (emacsgpt-query input)))


;; (defun emacsgpt-eval-region (start end)
;;   "Evaluate the region from START to END using the Emacsgpt API."
;;   (interactive "r")
;;   (let* ((input (buffer-substring-no-properties start end))
;;          (messages `[ (("role" . "user") ("content" . ,input)) ]))
;;     (emacsgpt-query messages 'emacsgpt-handle-response)))

(defun emacsgpt-eval-paragraph ()
  "Evaluate the current paragraph using the Emacsgpt API."
  (interactive)
  (save-excursion  ; Preserve the point's original position
    (let* ((start (progn (backward-paragraph) (point)))  ; Find start of the paragraph
           (end (progn (forward-paragraph) (point))))    ; Find end of the paragraph
      (emacsgpt-eval-region start end))))

(define-minor-mode emacsgpt-mode
  "A minor mode to interact with OpenAI's Emacsgpt."
  :lighter " Emacsgpt"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c c r") 'emacsgpt-eval-region)
            (define-key map (kbd "C-c c p") 'emacsgpt-eval-paragraph)
            map))

(define-derived-mode inferior-emacsgpt-mode fundamental-mode "Inferior Emacsgpt"
  "Major mode for Emacsgpt interaction."
  (setq-local font-lock-defaults '(inferior-emacsgpt-font-lock-keywords))
  (setq-local read-only t))

(defvar inferior-emacsgpt-font-lock-keywords
  '(("\\(.*INPUT.*\\)" . font-lock-function-name-face)
    ("\\(.*OUTPUT.*\\)" . font-lock-variable-name-face)))

(defun emacsgpt--turn-on ()
  "Turn on `emacsgpt-mode` in the current buffer if appropriate."
  (unless (or (minibufferp)  ; Don't enable in minibuffers
              (not (derived-mode-p 'text-mode 'prog-mode)))  ; Only enable in text and prog modes
    (emacsgpt-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-emacsgpt-mode
  emacsgpt-mode emacsgpt--turn-on)

(provide 'emacsgpt-mode)
