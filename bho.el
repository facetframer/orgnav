(require 'helm)
(require 'helm-org)
(require 's)

(defun bho--make-source (default-action candidates-func)
  (list
   (cons 'name "HELM at the Emacs")
   (cons 'candidates candidate-func)
   (cons 'action (bho--make-actions default-action))))

(defun bho--make-actions (default-action)
  (list
    (cons "Default action" default-action)
    (cons "Decrease depth `M-h`" 'bho-decrease-depth)
    (cons "Increase depth `M-l`" 'bho-increase-depth)
    (cons "Explore node `M-m`" 'bho-explore)
    (cons "Explore node `M-n`" 'bho-explore-parent)
    (cons "Rename node `M-r`" 'bho--rename-action)
    (cons "Refile node `M-f`" 'bho-refile-action)
    (cons "Go to node `M-g`" 'bho--goto-char)
    (cons "Go to node `M-c`" 'bho-clock-action)
    (cons "Go to node `M-a`" 'bho-search-ancestors)
    ))

(defun bho--make-candidate (point)
  "Construct a helm candidate from an org node at point"
  (cons
   (bho--get-entry-str point)
   point))

(defun bho--get-desc-candidates ()
  "Helm candidate function for descendants"
  (save-excursion
    (set-buffer bho-var-buffer)

    (mapcar
     'bho--make-candidate
     (let ((current-level
            (if (null bho-var-point) 0
              (save-excursion
                (goto-char bho-var-point)
                (org-outline-level)))))
       (bho--filter-by-depth
        (bho--get-descendants bho-var-point)
        current-level (+ current-level bho-var-depth))))))


(defun bho--get-anc-candidates ()
  (save-excursion
    (set-buffer bho-var-buffer)
    (if bho-var-point
        (goto-char bho-var-point))

    (mapcar 'bho--make-candidate
            (bho--ancestors))))

(defun bho--get-entry-str (point)
  "How bho should represent a heading"
  (save-excursion
    (goto-char point)
    (concat (s-repeat (org-outline-level) "*") " " (org-get-heading))))

(defun bho--filter-by-depth (headings min-level max-level)
  (-filter (lambda (x)
             (save-excursion
               (goto-char x)
               (and
                (or (null min-level)
                    (>= (org-outline-level) min-level))
                (or (null max-level)
                    (<= (org-outline-level) max-level)))))
           headings))

(defun bho--get-descendants (tree)
  "Get the positions of all the headings under this node"
  (interactive)
  (let ((result))
    (save-excursion
      (if (not (null tree)) (goto-char tree))
      (if
          (null tree)
          (org-map-region (lambda () (add-to-list 'result (point) 't)) (point-min) (point-max))
        (org-map-tree (lambda () (add-to-list 'result (point) 't)))))
    result))

(defun bho--goto-char (point)
  (interactive)
  (goto-char point)
  (org-reveal))

(defun bho-explore (point)
  (message (format "Exploring %S" point))
  (bho-search-subtree point 1 bho-var-default-action bho-var-buffer-name))

(defun bho-explore-parent (_)
  (bho-search-subtree (bho-get-parent bho-var-point) 1 bho-var-default-action bho-var-buffer-name))

(defun bho-search-ancestors (&optional node default-action buffer-name)
  "Search through the ancestors of a node (by the default the current node)"
  (interactive)
  (setq node (or node (save-excursion (org-back-to-heading) (point))))
  (setq default-action (or default-action 'bho--goto-char))
  (bho--search 'bho--get-anc-candidates node nil default-action buffer-name))

(defun bho-search-clocking ()
  "Search the relative to the clocking activity"
  (interactive)
  (save-excursion
    (org-clock-goto)
    (bho-search-ancestors)))

(setq bho-mapping
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    ;; We can't call these things directly because we need
    ;; to quit helm

    ;; We should probably put a layer of naming
    ;; on top of this, so we don't refer to things
    ;; by index
    (define-key map (kbd "M-h") (lambda () (interactive) (helm-select-nth-action 1)))
    (define-key map (kbd "M-l") (lambda () (interactive) (helm-select-nth-action 2)))
    (define-key map (kbd "M-m") (lambda () (interactive) (helm-select-nth-action 3)))
    (define-key map (kbd "M-n") (lambda () (interactive) (helm-select-nth-action 4)))
    (define-key map (kbd "M-r") (lambda () (interactive) (helm-select-nth-action 5)))
    (define-key map (kbd "M-f") (lambda () (interactive) (helm-select-nth-action 6)))
    (define-key map (kbd "M-g") (lambda () (interactive) (helm-select-nth-action 7)))
    (define-key map (kbd "M-c") (lambda () (interactive) (helm-select-nth-action 8)))
    (define-key map (kbd "M-a") (lambda () (interactive) (helm-select-nth-action 9)))
    (define-key map (kbd "M-j") 'helm-next-line)
    (define-key map (kbd "M-k") 'helm-previous-line)
    map))

(defun bho-increase-depth (_)
  (interactive)
  (bho-search-subtree bho-var-point (+ bho-var-depth 1) bho-var-default-action) bho-var-buffer-name)

(defun bho-decrease-depth (_)
  (interactive)
  (bho-search-subtree bho-var-point (max (- bho-var-depth 1) 1) bho-var-default-action) bho-var-buffer-name)

(defun bho-get-parent (point)
  (save-excursion
    (goto-char point)
    (condition-case nil
        (progn
          (outline-up-heading 1 t)
          (point))
      (error nil))))

(defun bho-search-subtree (point depth default-action &optional buffer-name)
  "Explore the org subtree at `point`. If `point` is nil explore the buffer. This function returns immediately.
Use buffer-name to give the helm search buffer a name. This is useful for helm-resume
"
  (interactive (list (point) 1 'bho--goto-char))
  (bho--search 'bho--get-desc-candidates point depth default-action buffer-name))

(defun bho--search (candidate-func point depth default-action buffer-name)
  "Search using the candidates generated by candidate-func"

  ;; Ugg -- too many arguments

  ;; Candidate functions appear to
  ;;   not be run in the current buffer, we need to keep track of the buffer
  (setq bho-var-buffer (current-buffer))
  (setq bho-var-point point)
  (setq bho-var-depth depth)
  (setq bho-var-buffer-name buffer-name)
  (setq bho-var-default-action default-action)
  (setq buffer-name (or buffer-name "*bho*"))
  (interactive)
  (helm :sources (list (bho--make-source default-action candidate-func)) :keymap bho-mapping :buffer buffer-name))


(defun bho-search-subtree-sync (point depth &optional buffer-name)
  "Explore the subtree. Wait for the exploration to finish and return the point selected"
  ;; Work around for helm's asychronicity
  (setq bho-var-result nil)
  (bho-search-subtree point depth 'bho--return-result buffer-name)
  ;; RACE CONDITION
  (while (null bho-var-result)
    (sit-for 0.05))
  (prog1
      bho-var-result
  (setq bho-var-result nil)))

(defun bho--return-result (point)
  (setq bho-var-result point))

(defun bho-search-root (depth default-action)
  "Explore the current org document"
  (interactive (list 1 'bho--goto-char))
  (bho-search-subtree nil depth default-action "*bho-search*"))

(defun bho--rename (point name)
  (let (new-heading)
    (save-excursion
      (goto-char point)
      (setq new-heading
            (concat (s-repeat (org-outline-level) "*") " " name))

      (beginning-of-line)
      (kill-line)
      (insert new-heading))))

(defvar bho-refile-depth 2 "depth of tree to show when refiling")
(defvar bho-clock-depth 2 "depth of tree to show when clocking in")
(defvar bho-clock-buffer nil "Switch to this buffer when clocking in")

(defun bho--rename-action (point)
  (interactive)
  (let (heading)
    (setq heading
          (read-string "New name" (save-excursion
                              (goto-char point)
                              (org-get-heading))))
    (bho--rename point heading)
    (bho-search-subtree bho-var-point bho-var-depth bho-var-default-action bho-var-buffer-name)
    ))

(defun bho-refile-action (point)
  (interactive)
  (goto-char point)
  (call-interactively 'org-refile)
  (bho-refile point))

(defun bho-clock-action (point)
  (save-excursion
    (goto-char point)
    (org-clock-in)))

(defun bho-refile-to-point (refile-point)
  (message (format "Refiling to %S" refile-point))
  (setq bho-last-refile-mark (make-marker))
  (set-marker bho-last-refile-mark refile-point)
  (org-refile nil nil (list nil buffer-file-name nil refile-point)))

(defun bho-refile (source-point target-point)
  "Refile the node at source-point to an ancestor of the node at target-point interactively."
  (interactive (list nil nil))
  (save-excursion
    (if (not (null source-point))
        (goto-char source-point))
    (bho-search-subtree target-point bho-refile-depth 'bho-refile-to-point "*bho refile*")))

(defun bho-refile-again ()
  (interactive)
  (assert bho-last-refile-mark)
  (bho-refile-to-point (marker-position bho-last-refile-mark))
  (message
   (save-excursion
     (goto-char bho-last-refile-mark)
     (org-no-properties (org-get-heading)) ;; who needs encapsulation
     ))

  )

(defun bho-clock-in (buffer point)
  "Clock in to an activity"
  (interactive (list bho-clock-buffer nil))
  (save-excursion
    (if (not (null buffer))
        (set-buffer buffer))
    (bho-search-subtree nil bho-clock-depth 'bho-clock-action "*bho-clock-in*")))

(defun bho-capture-function-global ()
  "A function that can be used with org-capture-template as a *file+function* or *function* capture point
Here is an example entry

        (\"*\" \"Create a new entry\" entry (file+function \"test.org\" bho-capture-function-global) \"** Title\")

"
  (goto-char (bho-search-subtree-sync nil bho-refile-depth "*bho-capture*")))

(defun bho-capture-function-relative ()
  "A function that can be used with org-capture-template as a *function* capture point.
Start search below the current node
        (\"*\" \"Create a new entry\" entry (function bho-capture-function-relative) \"** Title\")

"
  (goto-char (bho-search-subtree-sync
              (save-excursion
                (outline-back-to-heading 't)
                (point))
              bho-refile-depth "*bho-capture*")))

(defun bho-refile-nearby (&optional up-levels-arg)
  "Refile nearby"
  (interactive)
  (let* ((up-levels (or up-levels-arg 3)))
    (bho-refile (point) (save-excursion (org-back-to-heading) (outline-up-heading up-levels) (point)))))

(defun bho-jump-interactive (base-filename base-heading)
  (if (not (null base-filename)) (find-file base-filename))
  (bho--goto-char
   (bho-search-subtree-sync
    (and base-heading (org-find-exact-headline-in-buffer base-heading))
    2)))

(defun bho--ancestors ()
  "Find the ancestors of the current node"
  (save-excursion
    (outline-back-to-heading 't)
    (cons (point) (evorg-ancestors-rec))))

(defun bho--ancestors-rec ()
  (if
      (condition-case nil
          (progn
            (outline-up-heading 1 't)
            't)
        (error nil))
      (cons (point) (evorg-ancestors-rec))
    nil))



(provide 'bho)
