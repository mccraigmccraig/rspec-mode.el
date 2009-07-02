;;;
;;; rspec-mode.el
;;;
;;; Pat Maddox

(define-derived-mode rspec-mode ruby-mode "RSpec")

(add-to-list 'auto-mode-alist '("_spec.rb$" . rspec-mode))
(add-to-list 'auto-mode-alist '("_behavior.rb$" . rspec-mode))
(if (functionp 'yas/load-directory)
    (yas/load-directory (concat (file-name-directory load-file-name) "snippets")))

(setq rspec-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "\C-c s") 'run-specs)
    (define-key map (kbd "\C-c f") 'run-focused-spec)
    (define-key map (kbd "\C-c j") 'run-jspecs)
    (define-key map (kbd "\C-c k") 'run-focused-jspec)
    map))

(defun rspec-mode-ecb-compat ()
  (if (boundp 'ecb-compilation-buffer-names)
      (add-to-list 'ecb-compilation-buffer-names '("rspec-results"))))
  
(add-hook 'rspec-mode-hook
          (lambda () (progn (use-local-map rspec-mode-map)
			    (rspec-mode-ecb-compat))))


(defun rails-root (&optional dir)
  (or dir (setq dir default-directory))
  (if (file-exists-p (concat dir "config/environment.rb"))
      dir
    (unless (equal dir "/")
      (rails-root (expand-file-name (concat dir "../"))))))

(defun spec-command ( cmd-name )
  (if (rails-root)
      (let ((script-spec (concat (rails-root) "script/" cmd-name))
            (plugin-spec (concat (rails-root) "vendor/plugins/rspec/bin/" cmd-name)))
        (cond ((file-exists-p script-spec) script-spec)
              ((file-exists-p plugin-spec) plugin-spec)
              (t cmd-name)))
    cmd-name))

(defun run-specs ()
  "Run specs and display results in same buffer"
  (interactive)
  (do-run-spec (spec-command "spec")))

(defun run-focused-spec ()
  "Run the example defined on the current line"
  (interactive)
  (do-run-spec (spec-command "spec") (concat "--line=" (number-to-string (line-number-at-pos)))))

(defun run-jspecs ()
  "Run specs under jruby and display results in same buffer"
  (interactive)
  (do-run-spec (spec-command "jspec")))
 
(defun run-focused-jspec ()
  "Run the example defined on the current line"
  (interactive)
  (do-run-spec (spec-command "jspec") (concat "--line=" (number-to-string (line-number-at-pos)))))

(load (concat (file-name-directory load-file-name) "linkify"))
(defun do-run-spec (cmd &rest args)
  (setq rspec-results (get-buffer-create "rspec-results"))
  
  (lexical-let ((startwin (selected-window)))
    (labels ((scroll-to-end-of-results (proc state)
				       (save-excursion
					 (let ((curwin (selected-window)))
					   (select-window (display-buffer rspec-results) t)
					   (let ((resultswin (selected-window)))
					     (goto-char (point-max))
					     (insert "\n*finished*")
					     (if (eql curwin resultswin)
						 (select-window startwin)
					       (select-window curwin)))))))
      (save-excursion
	(set-buffer rspec-results)
	(erase-buffer)
	(setq linkify-regexps '("^\\(/.*\\):\\([0-9]*\\):$")))
      (setq proc (apply #'start-process "rspec" rspec-results cmd (buffer-file-name) args))
      (set-process-filter proc 'linkify-filter)
      (set-process-sentinel proc #'scroll-to-end-of-results)
      
      (select-window (display-buffer rspec-results))
      (goto-char (point-max)))))
  
(provide 'rspec-mode)
