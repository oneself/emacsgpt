
(require 'request)
(require 'subr-x)
(require 'spinner)

;(require 'f)
;(setq debug-on-error t)

  ;;;;;;;;;;;;;;;
 ;; VARIABLES ;;
;;;;;;;;;;;;;;;

(defvar emacsgpt-api-key nil
  "API key for OpenAI. Set this variable or the environment variable OPENAI_API_KEY.")

(defvar emacsgpt-api-model "gpt-4"
  "Model name for OpenAI API.")

(defvar emacsgpt-api-chat-endpoint (concat "https://api.openai.com/v1/chat/completions")
  "Endpoint for OpenAI API.")

(defvar-local emacsgpt--spinner nil)

(defconst emacsgpt--lighter
  '(" emacsgpt" (:eval (spinner-print emacsgpt--spinner))))

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

(defun emacsgpt--start-spinner ()
  "Create and start a spinner in the *chatgpt* buffer."
  ;(with-current-buffer (get-buffer-create "*chatgpt*")
    (unless emacsgpt--spinner
      (setq emacsgpt--spinner (spinner-create 'moon 10)))
    (spinner-start emacsgpt--spinner)
    (message "spinner started"))
;)

(defun emacsgpt--stop-spinner ()
  "Stop the spinner in the *chatgpt* buffer."
  ;(with-current-buffer (get-buffer-create "*chatgpt*")
    (when emacsgpt--spinner
      (spinner-stop emacsgpt--spinner)
      (setq emacsgpt--spinner nil)
      (message "spinner stopped")))
;)

;(emacsgpt--start-spinner)
;(emacsgpt--stop-spinner)


  ;;;;;;;;;;;;;;;
 ;; ASSISTANT ;;
;;;;;;;;;;;;;;;




  ;;;;;;;;;;
 ;; CHAT ;;
;;;;;;;;;;

(defun emacsgpt-query (input)
  "Send INPUT to the OpenAI API and return the response."
  ;; Start the spinner
  (emacsgpt--start-spinner)
  ;; Construct headers
  (with-current-buffer (switch-to-buffer-other-window (get-buffer-create "*emacsgpt*"))
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      ;; Split input into lines; insert first 5 and count the rest.
      (let* ((input-lines (split-string input "\n"))
             (first-lines (cl-subseq input-lines 0 (min 5 (length input-lines))))
             (remaining-lines (- (length input-lines) 5)))
        (insert (format "\n\n------------------------------------- INPUT ------------------------------------\n\n%s\n"
                        (string-join first-lines "\n")))
        (when (> remaining-lines 0)
          (insert (format "\n[%d more lines]\n" remaining-lines)))))  ; Insert number of remaining lines
    (inferior-emacsgpt-mode))
  (let ((headers `(("Content-Type" . "application/json")
                   ("Authorization" . ,(concat "Bearer " (emacsgpt-api-key-get)))))
        (request-data (json-encode `(("model" . ,emacsgpt-api-model)
                                     ("messages" . ((("role" . "user") ("content" . ,input))))))))
    (emacsgpt-log (format "Request url: %s, headers: %s, data: %s" emacsgpt-api-chat-endpoint headers request-data))
    (request emacsgpt-api-chat-endpoint
      :method "POST"
      :headers headers
      :data request-data
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (emacsgpt--stop-spinner)
                  (emacsgpt-handle-response data)))
      :error (cl-function
              (lambda (&rest args &key error-thrown &allow-other-keys)
                (emacsgpt--stop-spinner)
                (setq emacsgpt-spinner nil)
                (message "Got error: %S" error-thrown))))))

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

(defun emacsgpt-eval-buffer ()
  "Evaluate the current buffer using the OpenAI API by calling `emacsgpt-eval-region`."
  (interactive)
  ;; Use `point-min` and `point-max` to get the start and end of the buffer
  (emacsgpt-eval-region (point-min) (point-max)))

(defun emacsgpt-eval-message ()
  "Send a message to OpenAI API."
  (interactive)
  (let* ((user-input (read-string "Message: ")))
    (if (string= "" (string-trim user-input))
        (message "Input is empty.")
      (emacsgpt-query user-input))))

  ;;;;;;;;;;
 ;; MODE ;;
;;;;;;;;;;

(define-minor-mode emacsgpt-mode
  "A minor mode to interact with OpenAI's Emacsgpt."
  :lighter emacsgpt--lighter
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c c r") 'emacsgpt-eval-region)
            (define-key map (kbd "C-c c b") 'emacsgpt-eval-buffer)
            (define-key map (kbd "C-c c p") 'emacsgpt-eval-paragraph)
            (define-key map (kbd "C-c c m") 'emacsgpt-eval-message)
            map))

(define-derived-mode inferior-emacsgpt-mode markdown-mode "Inferior Emacsgpt"
  "Major mode for Emacsgpt interaction."
  ;; Combine markdown-mode's font-lock keywords with custom ones
  (setq-local font-lock-defaults `(,(append markdown-mode-font-lock-keywords
                                            '((".*INPUT.*" . font-lock-function-name-face)
                                              (".*OUTPUT.*" . font-lock-variable-name-face)))))
  ;; Make the buffer read-only
  (setq-local buffer-read-only t))

;; Set the lighter for the mode line
(add-to-list 'minor-mode-alist '(inferior-emacsgpt-mode emacsgpt--lighter))



(defun emacsgpt--turn-on ()
  "Turn on `emacsgpt-mode` in the current buffer if appropriate."
  (unless (or (minibufferp)  ; Don't enable in minibuffers
              (not (derived-mode-p 'text-mode 'prog-mode)))  ; Only enable in text and prog modes
    (emacsgpt-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-emacsgpt-mode
  emacsgpt-mode emacsgpt--turn-on)

(provide 'emacsgpt-mode)
