;;; orgnav-tree.el --- Orgnav functions to interact with trees

;; Copyright (C) 2016 Facet Framer

;; Author: Facet Framer (facet@facetframer.com)
;; URL: github.com/facetframer/orgnav

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
;;

;;; Code:

(require 'org)

(defun orgnav-tree-get-parent (point)
  "Get the parent of the node at POINT."
  (orgnav-tree-get-ancestor point 1))

(defun orgnav-tree-get-ancestor (point levels)
  "Get the ancestor of the node at POINT, LEVELS levels up."
  (save-excursion
    (goto-char point)
    (condition-case nil
        (progn
          (outline-up-heading levels t)
          (point))
      (error nil))))

(defun orgnav-tree-get-heading (buffer point)
  "Get the heading of an org element in BUFFER at POINT."
  (with-current-buffer buffer
    (save-excursion
      (setq point (or point (point)))
      (progn
        (goto-char point)
        (substring-no-properties (org-get-heading))))))

(defun orgnav-tree-get-descendants (tree)
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

(defun orgnav-tree-rename (point name)
  "Rename the node at POINT to NAME."
  (let (new-heading)
    (save-excursion
      (goto-char point)
      (setq new-heading
            (concat (s-repeat (org-outline-level) "*") " " name))

      (beginning-of-line)
      (kill-line)
      (insert new-heading))))

(defun orgnav-tree-ancestors (&optional point)
  "Find the ancestors of the current org node."
  (save-excursion
    (when point (goto-char point))
    (outline-back-to-heading 't)
    (cons (point) (orgnav-tree--ancestors-rec))))

(defun orgnav-tree--ancestors-rec ()
  "Convenience function used by `orgnav-tree-ancestors'."
  (if
      (condition-case nil
          (progn
            (outline-up-heading 1 't)
            't)
        (error nil))
      (cons (point) (orgnav-tree--ancestors-rec))
    nil))

(provide 'orgnav-tree)
;;; orgnav-tree.el ends here
