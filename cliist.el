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

(setq cliist/list-exec "cliist %s --format org"
	  cliist/exec "cliist %s")

(defun cliist/open-buffer-and-run-command (title cliist-command)
  (switch-to-buffer (get-buffer-create (format "*Cliist: %s*" title)))
  (erase-buffer)
  (org-mode)
  (insert (shell-command-to-string (format cliist/list-exec cliist-command)))
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

(provide 'cliist)
