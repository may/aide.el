;;; aide.el --- An Emacs front end for GPT APIs like OpenAI  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2024 Junji Zhi and Nick May

;; Author: Junji Zhi
;; Keywords: gpt-4 openai chatgpt

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Simple wrapper to call GPT APIs
;;
;; For details, please see http://github.com/junjizhi/aide.el

;;; Code:

(require 'request) ;; M-x package-install RET request RET

(defgroup aide nil
  "aide.el custom settings"
  :group 'external
  :prefix "aide-")


;(defcustom aide-chat-model "gpt-4-1106-preview" ; best in class; EXPENSIVE. ~slow
;(defcustom aide-chat-model "gpt-4" ; good enough
(defcustom aide-chat-model "gpt-3.5-turbo" ; good enough; cheap! fast! (gpt-3.5-turbo-instruct not available at chat endpoint)
  "The model paramater that aide.el sends to OpenAI Chat API."
  :type 'string
  :group 'aide)

; GPT-4 and GPT 3.5 are 4k, with more expensive options available
(defcustom aide-max-input-tokens 4000 ; 16,385 coming in Dec 2023 ;; 128000
  "The maximum number of tokens that aide.el sends to OpenAI API.
Only affects the send COMPLETE buffer function."
  :type 'integer
  :group 'aide)

(defvar max-chars (* aide-max-input-tokens 4)) ; 1 token ~= 4 characters

;; above ~480-530 [~~400-425 words] you get that concluding block of text again that's useless.
;; Remember this is TOKENs not words.
(defcustom aide-max-output-tokens 430 ;; don't go above 480
  "The max-tokens parameter that aide.el sends to OpenAI API."
  :type 'integer
  :group 'aide)

(defcustom aide-temperature 1.1
  "The temperature paramater that aide.el sends to OpenAI API. 1 is default."
  :type 'float
  :group 'aide)

;; Not currently utilized.
;; (defcustom aide-top-p 0.1
;;   "The top-p parameter that aide.el sends to OpenAI API."
;;   :type 'float
;;   :group 'aide)

;; Not currently utilized.
;; (defcustom aide-frequency-penalty 0
;;   "The frequency_penalty parameter that aide.el sends to OpenAI API."
;;   :type 'float
;;   :group 'aide)

;; Not currently utilized.
;; (defcustom aide-presence-penalty 0
;;   "The presence_penalty parameter that aide.el sends to OpenAI API."
;;   :type 'float
;;   :group 'aide)

(defcustom aide-openai-api-key-getter (lambda () openai-api-key)
  "Function that retrieves the valid OpenAI API key"
  :type 'function
  :group 'aide)

(defcustom aide-memory-file "~/memory.txt"
  "The location in the file system where all of the prompt information, the memory,
that doesn't change between generations should be kept."
  :type 'string
  :group 'aide)

(defcustom use-memory 1
  "Should the system load the memory file, additional prompt information, or not?"
  :type 'boolean
  :group 'aide)

(defcustom aide-save-chat-file "~/aide-log.txt"
  "The location of the chat log; everything sent to and from OpenAI. Nil to disable."
  :type 'function
  :group 'aide)

;; Functions for users to call
(defun aide-openai-chat-region-insert (start end)
  "Send the region to OpenAI Chat API and insert the result to the end of buffer.

The function is smart to check if current buffer in org mode, and present result accordingly.

START and END are selected region boundaries.
"
  (interactive "r")
  (let* ((region (buffer-substring-no-properties start end))
         (is-in-org-mode (string-equal major-mode "org-mode"))
         (extra-conditions "\"\n\nIn your response, limit the characters to 80 characters
per line for text explanations and add line breaks if needed. Do not apply the character limit to code blocks.")
         (final-prompt (concat "Please help me with the following question:\n\n \"" region extra-conditions))
         original-point
         tmp-text-end-point)
    (goto-char (point-max))
    (setq original-point (point))
    (insert "\n\n>>> GPT: Generating response... (This is placeholder text. It will disppear. DO NOT edit.)")
    (setq tmp-text-end-point (point))

    (let ((x (make-overlay original-point tmp-text-end-point)))
      (overlay-put x 'face '(:foreground "lime green"))
      (deactivate-mark))

    (aide--openai-chat-string final-prompt (lambda (result)
                                             (delete-region original-point tmp-text-end-point)
                                       (if result
                                           (progn
                                             (if is-in-org-mode
                                                 (insert "\n\n>>> GPT:\n#+BEGIN_SRC markdown\n" result "\n#+END_SRC")
                                               (insert "\n\n>>> GPT: " result))
                                             (if is-in-org-mode
                                                 nil
                                                 (let ((x (make-overlay original-point (point-max))))
                                                   (overlay-put x 'face '(:foreground "orange red"))
                                               (deactivate-mark)))
                                             result)
                                         (message "Empty result"))))))

(defun aide-openai-chat-paragraph-insert ()
  "Send the current paragraph to OpenAI Chat API and append the result to the end of the buffer
"
  (interactive)
  (let (region-start
        region-end)
    (save-excursion
      (backward-paragraph)
      (setq region-start (point))
      (forward-paragraph)
      (setq region-end (point))
      )
    (aide-openai-chat-region-insert region-start region-end)))

(defun aide-openai-chat-buffer-insert (&optional result)
  "Send the ENTIRE buffer, before the point, up to max tokens, to OpenAI and
insert the result to the current point of the buffer.
The user may also optionally set 'aide-memory-file' and 'aide-use-memory' to provide
additional context to the AI. The user will be prompted for a one-line prompt to
provide to the AI to guide the current generation.
Memory, prompt and then buffer will be sent, without exceeding max tokens."
  (interactive)
  (let* ((original-point (point)))
    (if result
        (progn
          (insert "\n" result) ; insert at current point
          (fill-paragraph)
          (let ((x (make-overlay original-point
                                 (+ original-point (length result)))))
            (overlay-put x 'face '(:foreground "orange red"))))
      (aide--openai-chat-string
       (prepare-prompt-memory-input original-point)
       'aide-openai-chat-buffer-insert))))

;; private; should not be called by users

(defun aide-openai-chat (api-key prompt callback)
  "Return the prompt answer from OpenAI API.
API-KEY is the OpenAI API key.

PROMPT is the prompt string we send to the API."
  (let* ((result nil)
        (auth-value (format "Bearer %s" api-key)) 
        (payload (json-encode `(("model"  . ,aide-chat-model)
                                ("max_tokens" . ,aide-max-output-tokens)
                                ("temperature" . ,aide-temperature)
;                               ("frequency_penalty" . ,aide-frequency-penalty)
;                                ("presence_penalty" . ,aide-presence-penalty)
;                               ("top_p" . ,aide-top-p)))
                                ("messages" . [(("role" . "user") ("content" . ,prompt))])))))
    (message "Waiting for OpenAI...")
    (let ((request-timestamp (current-time)))
      (request
        "https://api.openai.com/v1/chat/completions"
        :type "POST"
        :data payload
        :headers `(("Authorization" . ,auth-value) ("Content-Type" . "application/json"))
        :sync nil
        :parser 'json-read
        :success (cl-function
                  (lambda (&key data &allow-other-keys)
                    (progn
                      (setq result (alist-get 'content (alist-get 'message (elt (alist-get 'choices data) 0))))
                      (log-call-response prompt result request-timestamp)
                      (funcall callback result)
                      (message "Done."))))
        :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                              (message "Got error: %S, payload: %S" error-thrown payload))))
      result)))

(defun aide--openai-chat-string (string callback)
  (aide-openai-chat (funcall aide-openai-api-key-getter) string callback))

(defun prepare-prompt-memory-input (current-point)
    (if use-memory
        (if (file-readable-p aide-memory-file)
            (setq memory (file-to-string aide-memory-file))
          (progn
            (setq memory "")
            (message "No memory found, but enabled!")
            (sleep-for 3)))) ; let user see the message
    (prompt-for-gpt-input) ;; sets gpt-prompt
    (let ((prompt (make-prompt memory gpt-prompt
                               (buffer-substring-no-properties (point-min) current-point))))
      (if (> (length prompt) max-chars)
          ;; need to take max-char minus memory and gpt-prompt
          ;; then take that last NUM chars off of the buffer
          ;; need to only trim off of the buffer, not the whole thing
          (make-prompt memory gpt-prompt
                       (get-last-n-chars
                        (buffer-substring-no-properties (point-min) current-point)
                        (- max-chars (+ (length memory) (length gpt-prompt)))))
        prompt))) ; else return prompt

(defun make-prompt (memory instructions buffer)
  (concat
   memory ;;; Memory first for basic context that doesn't change; who I am, what we're doing..
   "Instructions: "
   gpt-prompt
   "\n\The most recent scene(s) in the story so far: \n\n"
   buffer))

(defun get-min-point-org ()
  "OpenAI API limits requests of > ~4000 tokens (model-specific; davinci
maxes out at request of 4000 tokens; ~15200 char)"
  (if (> (buffer-size) (* 4 (or aide-max-input-tokens 3800))) ;; 1 tokens = ~4 char
      (- (point-max) (* 4 (or aide-max-input-tokens 3800)))
    (point-min)))

(defun log-call-response (prompt response request-timestamp)
  (if aide-save-chat-file
      (write-region
      ;; Starting with * allows users to view log w/ org mode for easy folding
      ;; Also log how long the request took, for which model
       (concat "\n\n* " (current-time-string)
               "(" (float-time (time-subtract (current-time) request-timestamp))
               ";" aide-chat-model
               ")"
               "\n" prompt "\n" response)
       nil aide-save-chat-file 'append)))

(defvar gpt-prompt "" "Prompt for OpenAI chat.")

(defun prompt-for-gpt-input ()
  "Prompt the user to enter a prompt, or accept the prompt they used last time."
  (interactive "s")
  (setq gpt-prompt (read-string "Enter your prompt here (comes after memory): " gpt-prompt)))

(defun file-to-string (file)
  "File to string function"
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun get-last-n-chars (str num)
  "Return the last NUM characters of STR."
  (if (> num (length str))
      str  ; Return the original string if NUM is larger than the length of the string
    (substring str (- (length str) num))))

(provide 'aide)
;;; aide.el ends here
