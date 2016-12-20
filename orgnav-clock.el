;;; orgnav-clock.el --- Orgnav capture convenience functions

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

(defvar orgnav-clock-buffer nil "The buffer to search when clocking in.")
(defvar orgnav-clock-depth 2 "How many levels to show for clock operations.")

;;; Interactive entry points for clocking
(defun orgnav-clock-in (buffer node-point)
  "Clock in to a node in an org buffer BUFFER, starting searching in descendents of NODE-POINT."
  (interactive (list orgnav-clock-buffer nil))
  (save-excursion
    (if (not (null buffer))
        (set-buffer buffer))
    (orgnav-search-subtree node-point
                        :depth orgnav-clock-depth
                        :default-action 'orgnav--clock-action
                        :helm-buffer-name "*orgnav-clock-in*")))

(defun orgnav-search-clocking ()
  "Start a search relative to the currently clocking activity."
  (interactive)
  (save-excursion
    (org-clock-goto)
    (orgnav-search-ancestors)))

(provide 'orgnav-clock)
;;; orgnav-clock.el ends here
