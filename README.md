# EmacsGPT

This is an emacs mode that can be used to interact with ChatGPT from inside
emacs. To use, follow these steps:

1. Set the [OpenAI API key](https://platform.openai.com/api-keys). In `.zshrc` or `.bashrc` (this can also be done in emacs):

    export OPENAI_API_KEY=<API_KEY>

2. Add the following to your `~/.emacs.d/init.el`:

    ;; Ensure request.el is installed, this can be done via (use-package
    request) or manually.
    (load "<PATH>/emacsgpt.el")        ; Load package
    (setq emacsgpt-api-model "gpt-4")  ; Set API model to use (default is "gpt-3.5-turbo")
    (global-emacsgpt-mode t)           ; Turn minor mode on globally

3. Once in any buffer use one of the following functions to interact with ChatGPT:
   - `emacsgpt-eval-region` (`C-c c r`) : Send active region.
   - `emacsgpt-eval-paragraph` (`C-c c r`) : Send current paragraph.
   - `emacsgpt-eval-message` (`C-c c r`) : Send a custom message.
