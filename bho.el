;;; bho.el --- Org tree navigation using helm

;; Copyright (C) 2016 Facet Framer

;; Author: Facet Framer (facet@facetframer.com)
;; URL: github.com/facetframer/bho

;; Version: 0.1.0
;; Package-Version: 20161028.1
;; Package-Requires: ((helm "1.5.5") (dash) (s))
;; Created October 2016

;; Keywords: org tree navigation

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; See README.md

;;; Code:

(require 'helm)
(require 'helm-org)
(require 's)
(require 'dash)

(defvar bho-log nil "Whether bho should log")
(defvar bho-refile-depth 2 "The number of levels to show when refiling.")
(defvar bho-clock-depth 2 "The number of levels to show when clocking in.")
(defvar bho-clock-buffer nil "The buffer to search when clocking in.")
(defvar bho-mapping
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    ;; We can't call these things directly because we need
    ;; to quit helm

    ;; We should probably put a layer of naming
    ;; on top of this, so we don't refer to things
    ;; by index
    (define-key map (kbd "M-h") (lambda () (interactive) (helm-exit-and-execute-action 'bho--decrease-depth-action)))
    (define-key map (kbd "M-l") (lambda () (interactive) (helm-exit-and-execute-action 'bho--increase-depth-action)))
    (define-key map (kbd "M-.") (lambda () (interactive) (helm-exit-and-execute-action 'bho--explore-action)))
    (define-key map (kbd "M-,") (lambda () (interactive) (helm-exit-and-execute-action 'bho--explore-parent-action)))
    (define-key map (kbd "M-r") (lambda () (interactive) (helm-exit-and-execute-action 'bho--rename-action)))
    (define-key map (kbd "M-g") (lambda () (interactive) (helm-exit-and-execute-action 'bho--goto-action)))
    (define-key map (kbd "M-c") (lambda () (interactive) (helm-exit-and-execute-action 'bho--clock-action)))
    (define-key map (kbd "M-a") (lambda () (interactive) (helm-exit-and-execute-action 'bho-search-ancestors)))
    (define-key map (kbd "M-n") (lambda () (interactive) (helm-exit-and-execute-action 'bho--new-action)))
    (define-key map (kbd "M-j") 'helm-next-line)
    (define-key map (kbd "M-k") 'helm-previous-line)
    map)
  "Keyboard mapping within helm.")

;; Private state variables
(defvar bho--var-buffer nil "Private state.")
(defvar bho--var-default-action nil "Private state.")
(defvar bho--var-depth nil "Private state.")
(defvar bho--var-helm-buffer nil "Private state.")
(defvar bho--var-point nil "Private state.")
(defvar bho--var-result nil "Private state.")
(defvar bho--var-last-refile-mark nil "Private state")

(defun bho--log (fmt-string &rest args)
  (when bho-log
    (message (apply 'format fmt-string args))))


(defun bho-search-subtree (point &optional depth default-action helm-buffer-name)
  "Explore the org subtree at POINT.  If POINT is nil explore the buffer.
This function returns immediately.
Show DEPTH levels.  By default run DEFAULT-ACTION on enter.
If HELM-BUFFER-NAME create a helm buffer with this name (of use with `helm-resume')."
  (interactive (list (point) nil nil))

  (setq depth (or depth 1))
  (setq default-action (or default-action 'bho--goto-action))

  (bho--search 'bho--get-desc-candidates point depth default-action helm-buffer-name))

(defun bho-search-ancestors (&optional node default-action helm-buffer-name)
  "Search through the ancestors of NODE (by the default the current node).
Run DEFAULT-ACTION on enter.  Name the helm buffer HELM-BUFFER-NAME.
By default jump to a node."
  (interactive)
  (setq node (or node (save-excursion (org-back-to-heading) (point))))
  (setq default-action (or default-action 'bho--goto-action))
  (bho--search 'bho--get-ancestor-candidates node nil default-action helm-buffer-name))

(defun bho-search-subtree-sync (point depth &optional buffer-name)
  "Search the tree at POINT by default displaying DEPTH levels.
Return the `(point)' at the selected node.
Start searching in the buffer called BUFFER-NAME."
  ;; Work around for helm's asychronicity
  (setq bho--var-result nil)
  (bho-search-subtree point depth 'bho--return-result-action buffer-name)
  ;; RACE CONDITION
  (while (null bho--var-result)
    (sit-for 0.05))
  (prog1
      bho--var-result
  (setq bho--var-result nil)))

(defun bho-search-ancestors-sync (point &optional buffer-name)
  "Search the ancesters of the node at POINT.
Return the `(point)' at the selected node.
Start searching in the buffer called BUFFER-NAME."
  ;; Work around for helm's asychronicity
  (setq bho--var-result nil)
  (bho-search-ancestors point 'bho--return-result-action buffer-name)
  ;; RACE CONDITION
  (while (null bho--var-result)
    (sit-for 0.05))
  (prog1
      bho--var-result
  (setq bho--var-result nil)))


(defun bho-search-root (depth default-action)
  "Explore all nodes in the current org document.
Display DEPTH levels.  Run DEFAULT-ACTION on enter."
  (interactive (list 1 'bho--goto-action))
  (bho-search-subtree nil depth default-action "*bho-search*"))

(defun bho-jump-interactive (base-filename base-heading)
  "Jump to an ancestor for a heading in BASE-FILENAME called BASE-HEADING."
  (if (not (null base-filename)) (find-file base-filename))
  (bho--goto-action
   (bho-search-subtree-sync
    (and base-heading (org-find-exact-headline-in-buffer base-heading))
    2)))

(defun bho-refile (source-point target-point)
  "Refile the node at SOURCE-POINT to a descendant of the node at TARGET-POINT interactively."
  (interactive (list nil nil))
  (save-excursion
    (if (not (null source-point))
        (goto-char source-point))
    (bho-search-subtree target-point bho-refile-depth 'bho--refile-to-action "*bho refile*")))

(defun bho-refile-ancestors (source-point target-point)
  "Refile the node at SOURCE-POINT to an ancestor of the node at TARGET-POINT interactively."
  (interactive (list nil nil))
  (save-excursion
    (if (not (null source-point))
        (goto-char source-point))
    (bho-search-ancestors target-point 'bho--refile-to-action "*bho refile*")))

(defun bho-refile-nearby (&optional levels-up)
  "Refile nearby the current point.  Go up LEVELS-UP."
  (interactive)
  (let* ((up-levels (or levels-up 3)))
    (bho-refile (point) (save-excursion (org-back-to-heading) (outline-up-heading up-levels t) (point)))))

(defun bho-refile-again ()
  "Refile to the location last selected by `bho-refile'."
  (interactive)
  (if (null bho--var-last-refile-mark)
      (error 'no-last-run))
  (bho--refile-to-action (marker-position bho--var-last-refile-mark))
  (message
   (save-excursion
     (goto-char bho--var-last-refile-mark)
     (org-no-properties (org-get-heading)))))

(defun bho-clock-in (buffer node-point)
  "Clock in to a node in an org buffer BUFFER, starting searching in descendents of NODE-POINT."
  (interactive (list bho-clock-buffer nil))
  (save-excursion
    (if (not (null buffer))
        (set-buffer buffer))
    (bho-search-subtree node-point bho-clock-depth 'bho--clock-action "*bho-clock-in*")))

(defun bho-search-clocking ()
  "Start a search relative to the currently clocking activity."
  (interactive)
  (save-excursion
    (org-clock-goto)
    (bho-search-ancestors)))

(defun bho-capture-function-global ()
  "A function that can be used with `org-capture-template'.
A *file+function* or *function* capture point to capture to
a location selected using bho under the root node.
Here is an example entry:
        `(\"*\" \"Create a new entry\" entry
              (file+function \"test.org\" bho-capture-function-global) \"** Title\")'"
  (goto-char (bho-search-subtree-sync nil bho-refile-depth "*bho-capture*")))

(defun bho-capture-function-relative ()
  "A function that can be used with `org-capture-template'.
A *function* capture point to capture to a location under
the current node selected using bho.
Here is an example entry:
        `(\"*\" \"Create a new entry\" entry
               (function bho-capture-function-relative) \"** Title\")'"
  (goto-char (bho-search-subtree-sync
              (save-excursion
                (outline-back-to-heading 't)
                (point))
              bho-refile-depth "*bho-capture*")))

(defun bho-capture-function-ancestors ()
  "A function that can be used with `org-capture-template'.
A *function* capture point'to capture to a ancestor
of the current node.
Here is an example entry:
        (\"*\" \"Create a new entry\" entry (function bho-capture-function-ancestor) \"** Title\")"
  (goto-char (bho-search-ancestors-sync
              (save-excursion
                (outline-back-to-heading 't)
                (point))
              "*bho-capture*")))

;; Private functions

(defun bho--ancestors ()
  "Find the ancestors of the current org node."
  (save-excursion
    (outline-back-to-heading 't)
    (cons (point) (bho--ancestors-rec))))

(defun bho--ancestors-rec ()
  "Convenience function used by `bho--ancestors'."
  (if
      (condition-case nil
          (progn
            (outline-up-heading 1 't)
            't)
        (error nil))
      (cons (point) (bho--ancestors-rec))
    nil))

(defun bho--make-source (candidate-func default-action)
  "Make helm source which gets candidates by calling CANDIDATES-FUNC.
by default run DEFAULT-ACTION when return pressed."
  (list
   (cons 'name "HELM at the Emacs")
   (cons 'candidates candidate-func)
   (cons 'action (bho--make-actions default-action))))

(defun bho--make-actions (default-action)
  "Actions for used by helm.  On return run DEFAULT-ACTION."
  (list
    (cons "Default action" default-action)
    (cons "Decrease depth `M-h`" 'bho--decrease-depth-action)
    (cons "Increase depth `M-l`" 'bho--increase-depth-action)
    (cons "Explore node `M-.`" 'bho--explore-action)
    (cons "Explore parent `M-,`" 'bho--explore-parent-action)
    (cons "Create a new node `M-n`" 'bho--new-action)
    (cons "Rename node `M-r`" 'bho--rename-action)
    (cons "Go to node `M-g`" 'bho--goto-action)
    (cons "Clock into the node `M-c`" 'bho--clock-action)
    (cons "Explore ancestors of a node `M-a`" 'bho-search-ancestors)
    ))

(defun bho--make-candidate (point)
  "Construct a helm candidate from a node at POINT."
  (cons
   (bho--get-entry-str point)
   point))

(defun bho--get-desc-candidates ()
  "Helm candidate function for descendants."
  (with-current-buffer bho--var-buffer
    (save-excursion
      (mapcar
       'bho--make-candidate
       (let ((current-level
              (if (null bho--var-point) 0
                (save-excursion
                  (goto-char bho--var-point)
                  (org-outline-level)))))
         (bho--filter-by-depth
          (bho--get-descendants bho--var-point)
          current-level (+ current-level bho--var-depth)))))))

(defun bho--get-ancestor-candidates ()
  "Find helm candidates for the ancestors of the location set by a search function."
  (with-current-buffer bho--var-buffer
    (save-excursion
      (if bho--var-point
          (goto-char bho--var-point))
      (mapcar 'bho--make-candidate
              (bho--ancestors)))))

(defun bho--get-entry-str (point)
  "How bho should represent a the node at POINT."
  (save-excursion
    (goto-char point)
    (concat (s-repeat (org-outline-level) "*") " " (org-get-heading))))

(defun bho--filter-by-depth (headings min-level max-level)
  "Filter the nodes at points in HEADINGS.
Only returning those between with a level better MIN-LEVEL and MAX-LEVEL."
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
  "Get the positions of all the headings under the tree at TREE."
  (interactive)
  (let ((result))
    (save-excursion
      (if (not (null tree)) (goto-char tree))
      (if
          (null tree)
          (org-map-region (lambda () (add-to-list 'result (point) 't)) (point-min) (point-max))
        (org-map-tree (lambda () (add-to-list 'result (point) 't)))))
    result))

(defun bho--goto-action (helm-entry)
  "Go to the node represented by HELM-ENTRY."
  (interactive)
  (goto-char helm-entry)
  (org-reveal))

(defun bho--explore-action (helm-entry)
  "Start search again from HELM-ENTRY."
  (message (format "Exploring %S" helm-entry))
  (bho-search-subtree helm-entry 1 bho--var-default-action bho--var-helm-buffer))

(defun bho--explore-parent-action (ignored)
  "Start search again from one level higher.  Ignore IGNORED."
  (bho-search-subtree (bho--get-parent bho--var-point) 1 bho--var-default-action bho--var-helm-buffer))

(defun bho--increase-depth-action (ignored)
  "Search again showing nodes at a greater depth.  IGNORED is ignored."
  (interactive)
  (bho-search-subtree bho--var-point (+ bho--var-depth 1) bho--var-default-action) bho--var-helm-buffer)

(defun bho--new-action (helm-entry)
  "Create child under the select HELM-ENTRY.  IGNORED is ignored."
  (let* (
         (point-function (lambda ()  (set-buffer bho--var-buffer) (goto-char helm-entry)))
         (org-capture-templates (list (list "." "Default action" 'entry (list 'function point-function) "* %(read-string \"Name\")"))))
    (org-capture nil ".")))


(defun bho--decrease-depth-action (ignored)
  "Search again hiding more ancestors.  IGNORED is ignored."
  (interactive)
  (bho-search-subtree bho--var-point (max (- bho--var-depth 1) 1) bho--var-default-action) bho--var-helm-buffer)

(defun bho--get-parent (point)
  "Get the parent of the node at POINT."
  (save-excursion
    (goto-char point)
    (condition-case nil
        (progn
          (outline-up-heading 1 t)
          (point))
      (error nil))))

(defun bho--search (candidate-func point depth default-action helm-buffer-name)
  "Search using the helm candidate function CANDIDATE-FUNC.
Start at POINT, displaying DEPTH levels.
On enter run DEFAULT-ACTION.
Search in a helm buffer with the name HELM-BUFFER-NAME."
  ;; Ugg -- too many arguments

  ;; Candidate functions appear to
  ;;   not be run in the current buffer, we need to keep track of the buffer
  (setq bho--var-buffer (current-buffer))
  (setq bho--var-point point)
  (setq bho--var-depth depth)
  (setq helm-buffer-name (or helm-buffer-name "*bho"))
  (setq bho--var-default-action default-action)
  (helm :sources (list (bho--make-source candidate-func default-action)) :keymap bho-mapping :buffer helm-buffer-name))

(defun bho--return-result-action (helm-entry)
  "A convenience action for synchronouse functions.
Store the location of HELM-ENTRY so that the synchronous functions can return them."
  (setq bho--var-result helm-entry))

(defun bho--rename (point name)
  "Rename the node at POINT to NAME."
  (let (new-heading)
    (save-excursion
      (goto-char point)
      (setq new-heading
            (concat (s-repeat (org-outline-level) "*") " " name))

      (beginning-of-line)
      (kill-line)
      (insert new-heading))))

(defun bho--rename-action (helm-entry)
  "Action to rename HELM-ENTRY."
  (interactive)
  (let (heading)
    (setq heading
          (read-string "New name" (save-excursion
                              (goto-char helm-entry)
                              (org-get-heading))))
    (bho--rename helm-entry heading)
    (bho-search-subtree bho--var-point bho--var-depth bho--var-default-action bho--var-helm-buffer)))

(defun bho--clock-action (helm-entry)
  "Clock into the selected HELM-ENTRY."
  (save-excursion
    (goto-char helm-entry)
    (org-clock-in)))

(defun bho--refile-to-action (helm-entry)
  "Action used by `bho-refile` to refile to the selected entry HELM-ENTRY."
  (message (format "Refiling to %S" helm-entry))
  (setq bho--var-last-refile-mark (make-marker))
  (set-marker bho--var-last-refile-mark helm-entry)
  (org-refile nil nil (list nil buffer-file-name nil helm-entry)))


(provide 'bho)
;;; bho.el ends here
