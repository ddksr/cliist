;;; cliist.el --- Cliist calls in Emacs
     
;; Copyright (C) 2014 Žiga Stopinšek
     
;; Author: Žiga Stopinšek <sigi.kajzer@gmail.com>
;; Version: 0.1
;; Keywords: productiviry, todo
;; URL: http://github.com/ddksr/cliist
     
;;; Commentary:
     
;; This package provides some basic Todoist querying via commandline tool cliist.
;; You are required to install cliist ( http://github.com/ddksr/cliist )
     
;;;###autoload

(require 'dash)
(require 'org)

(setq cliist/list-exec "cliist %s --format org"
	  cliist/exec "cliist %s")

(defun cliist/open-buffer-and-run-command (title cliist-command)
  (switch-to-buffer (get-buffer-create (format "*Cliist: %s*" title)))
  (erase-buffer)
  (org-mode)
  (insert (shell-command-to-string (format cliist/list-exec qcliist-command)))
  (goto-line 0) (move-to-column 0)
  (org-cycle) (org-cycle))

(defun cliist/project-list ()
  (mapcar '(lambda (x)
			 (nth 1 (split-string x "#")))
		  (split-string
		   (shell-command-to-string (format cliist/list-exec "-P")) "\n")))

(defun cliist/today-and-overdue ()
  (interactive)
  (cliist/open-buffer-and-run-command "today and overdue" ""))

(defun cliist/query (query)
  (interactive "sQuery: \n")
  (cliist/open-buffer-and-run-command query (format "-q %s" query)))
  
(defun cliist/project (name)
  (interactive
   (list
	(completing-read "Project: " (cliist/project-list))))
  (cliist/open-buffer-and-run-command (format "#%s" name) (format "-p %s" name)))

(defun cliist/view-all ()
  (interactive)
  (cliist/open-buffer-and-run-command "All" "-A"))

(defun cliist/completed (number &optional project)
  (interactive
   (list
	(read-from-minibuffer "Number of items: " "30")
	(completing-read "Project (leave empty for all): "
					 (cliist/project-list))))
  (cliist/open-buffer-and-run-command "Completed"
									  (format "--archive --limit %s %s"
											  number
											  (if (= (length project) 0)
												  ""
												(concat "-p " project)))))

(defun cliist/run (command)
  (interactive "sCommand: ")
  (shell-command (format cliist/exec command)))


(defun cliist/empty (val)
  (if (stringp val)
	  (= (length val) 0)
	(not val)))

(defun cliist/not-empty (val)
  (not (cliist/empty val)))

(defun cliist/convert-date (&rest dates)
  (car (split-string (or (-first 'cliist/not-empty
								 dates) " ")
				" ")))

(defun cliist/org-get-info-at-point ()
  (list (nth 4 (org-heading-components))
		(cliist/convert-date (org-entry-get (point) "SCHEDULED")
							 (org-entry-get (point) "DEADLINE"))
		(car (-filter
					'cliist/not-empty
					(split-string (or (nth 5 (org-heading-components))
									  "")
								  ":")))))

(defun cliist/get-info-at-point ()
  (if (eq major-mode 'org-mode)
	  (cliist/org-get-info-at-point)
	(list (buffer-substring (line-beginning-position) (line-end-position))
		  nil
		  nil)))

(defun cliist/add-task-cont (content &optional date tag)
  (message "%s | %s | %s" date content tag)
  (let ((c-date (read-from-minibuffer "Date: " (or date "")))
		(c-content (read-from-minibuffer "Content: "
										 (if (cliist/not-empty tag)
											 (format "#%s %s" tag content)
										   (or content "")))))
	(message "%s -a %s"
			 (if (cliist/not-empty c-date)
				 (format "-d %s" c-date)
			   "")
			 c-content)))

(defun cliist/add-task (&optional content date tag)
  (interactive)
  (apply 'cliist/add-task-cont
		 (--zip-with (-first 'cliist/not-empty
							 (list it other))
					 (list content date tag)
					 (cliist/get-info-at-point))))

(define-minor-mode cliist-mode
  "Communicate with cliist via Emacs"
  :lighter " cliist"
  :global t
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c c r") 'cliist/run)
			(define-key map (kbd "C-c c t") 'cliist/today-and-overdue)
			(define-key map (kbd "C-c c c") 'cliist/completed)
			(define-key map (kbd "C-c c p") 'cliist/project)
			(define-key map (kbd "C-c c q") 'cliist/query)
			(define-key map (kbd "C-c c v") 'cliist/view-all)
			(define-key map (kbd "C-c c a") 'cliist/add-task)
            map))

(provide 'cliist)
