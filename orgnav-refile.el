;;; orgnav-refile.el --- Orgnav refile convenience functions

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
(require 'orgnav)

(defvar orgnav-refile-depth 2 "The number of levels to show when refiling.")
(defvar orgnav-refile--last-mark nil "Private state.")

;;; Interactive entry points for refiling
(defun orgnav-refile (source-point target-point &rest options)
  "Refile the node at SOURCE-POINT to a descendant of the node at TARGET-POINT interactively.  Start with DEPTH levels displayed."
  (interactive (list nil nil))
  (setq options (orgnav--plist-update options :depth (or (plist-get options :depth) orgnav-refile-depth)))
  (setq options (orgnav--plist-update options :default-action 'orgnav-refile--action :helm-buffer-name "*orgnav refile*"))

  (save-excursion
    (if (not (null source-point))
        (goto-char source-point))
    (apply 'orgnav-search-subtree target-point options)))

(defun orgnav-refile-keep (source-point target-point)
  "Refile the node at SOURCE-POINT to a descendant of the node at TARGET-POINT interactively."
  (interactive (list nil nil))
  (save-excursion
    (if (not (null source-point))
        (goto-char source-point))
    (orgnav-search-subtree target-point
                        :depth orgnav-refile-depth
                        :default-action 'orgnav-refile--action-keep
                        :helm-buffer-name "*orgnav refile*")))

(defun orgnav-refile-ancestors (source-point target-point)
  "Refile the node at SOURCE-POINT to an ancestor of the node at TARGET-POINT interactively."
  (interactive (list nil nil))
  (save-excursion
    (if (not (null source-point))
        (goto-char source-point))
    (orgnav-search-ancestors target-point
                          :default-action 'orgnav-refile--action
                          :helm-buffer-name "*orgnav refile*")))

(defun orgnav-refile-nearby (&optional levels-up keep &rest settings)
  "Refile nearby the current point.  Go up LEVELS-UP.  If KEEP keep the original entry."
  (interactive)
  (let* (
         (up-levels (or levels-up 3))
         (refile-function (if keep 'orgnav-refile-keep 'orgnav-refile))
         (node))
    (setq node (save-excursion
                 (org-back-to-heading)
                 (outline-up-heading (min up-levels (- (org-outline-level) 1))
                                     t) (point)))
    (apply refile-function (point) node settings)))

(defun orgnav-refile-again ()
  "Refile to the location last selected by `orgnav-refile'."
  (interactive)
  (if (null orgnav-refile--last-mark)
      (error 'no-last-run))
  (orgnav-refile--action (marker-position orgnav-refile--last-mark))
  (save-excursion
     (goto-char orgnav-refile--last-mark)
     (org-no-properties (org-get-heading))))


(defun orgnav-refile--action (helm-entry)
  "Action used by `orgnav-refile` to refile to the selected entry HELM-ENTRY."
  (orgnav--log "Action: refiling %S to %S" (point) helm-entry)
  (setq orgnav-refile--last-mark (make-marker))
  (set-marker orgnav-refile--last-mark helm-entry)
  (org-refile nil nil (list nil buffer-file-name nil helm-entry)))

(defun orgnav-refile--action-keep (helm-entry)
  "Action used by `orgnav-refile-keep` to refile to a selected HELM-ENTRY.
The original entry is kept unlike `orgnav-refile--action'."
  (orgnav--log "Action: refiling %S to %S" (point) helm-entry)
  (setq orgnav-refile--last-mark (make-marker))
  (set-marker orgnav-refile--last-mark helm-entry)
  (org-refile 3 nil (list nil buffer-file-name nil helm-entry)))





(provide 'orgnav-refile)
;;; orgnav-refile.el ends here
