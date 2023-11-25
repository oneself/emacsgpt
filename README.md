<img src="https://raw.githubusercontent.com/oneself/emacsgpt/main/images/emacsgpt.png" width="200" align="right" title="macsGPT Logo" />

# EmacsGPT

This is an emacs mode that can be used to interact with ChatGPT from inside
emacs. 

To use it, follow these steps:

1. Set the [OpenAI API key](https://platform.openai.com/api-keys). In `.zshrc` or `.bashrc` (this can also be done in emacs):
```
export OPENAI_API_KEY=<API_KEY>
```
2. Add the following to your `~/.emacs.d/init.el`:
```
;; Ensure request.el is installed, this can be done via (use-package request) or manually.
(load "<PATH>/emacsgpt.el")        ; Load package
(setq emacsgpt-api-model "gpt-4")  ; Set API model to use (default is "gpt-3.5-turbo")
(setq emacsgpt-prefix-key "C-c c") ; Set the prefix key for emacsgpt-mode commands (default is "C-c -")
(global-emacsgpt-mode t)           ; Turn minor mode on globally
```
3. Once in any buffer use one of the following functions to interact with ChatGPT:
   - `emacsgpt-get-create-switch-buffer` (`C-c c s`): Show *emacsgpt* buffer.
   - `emacsgpt-eval-region` (`C-c c r`) : Send active region.
   - `emacsgpt-eval-paragraph` (`C-c c p`) : Send current paragraph.
   - `emacsgpt-eval-message` (`C-c c b`) : Send buffer.
   - `emacsgpt-eval-message` (`C-c c m`): Send a custom message.
   - `emacsgpt-eval-org-0` (`C-c c 0`): Send enclosing `org-mode` heading.
   - `emacsgpt-eval-org-1` (`C-c c 1`): Send enclosing `org-mode` heading one level up.
   - `emacsgpt-eval-org-2` (`C-c c 2`): Send enclosing `org-mode` heading two levels up.
