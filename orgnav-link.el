;;; orgnav-link.el --- Use orgnav to create links

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

(defvar orgnav-link-depth 3 "How many levels to display when creating links")

(defun orgnav-link-new (point &optional depth)
  "Insert link to a child of POINT.  If POINT is nil search the entire buffer."
  (interactive (list nil))
  (orgnav-link--insert (orgnav-link--get-custom-id (orgnav-search-subtree-sync point :depth (or depth orgnav-link-depth)))))

(defun orgnav-link--insert (custom-id)
  (insert (format "[[%s]]" custom-id)))

(defun orgnav-link--get-custom-id (point)
  (message (format "Getting custom id for %S" point))
  (let (custom-id)
    (save-excursion
      (goto-char point)
      (setq custom-id (org-entry-get point "CUSTOM_ID" nil))
      (when (null custom-id)
        (setq custom-id (orgnav-link--new-custom-id))
        (org-set-property "CUSTOM_ID" custom-id))
      custom-id)))

(defun orgnav-link--new-custom-id ()
  (save-excursion
    (org-back-to-heading)
    (message (format "Build customing link at %S" (point)))

    (substring-no-properties (s-concat (s-replace " " "-" (org-get-heading 't)) "-" (format-time-string "%s-") (format "%s" (random 10000))))))


(provide 'orgnav-link)



