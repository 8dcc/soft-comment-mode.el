;;; soft-comment.el --- Minor mode for softening the color of comments -*-

;; Author: 8dcc <8dcc.git@gmail.com>
;; Version: 1.0.0
;; URL: https://github.com/8dcc/soft-comment-mode.el

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Minor mode for softening the color of comments.
;;
;; Useful for temporarily ignoring comments, effectively focusing on actual
;; code.

;;; Code:

(defgroup soft-comment ()
  "Minor mode for using bigger fonts globally."
  :group 'faces
  :prefix "soft-comment-")

(defcustom soft-comment-ratio 0.5
  "Specifies how much the comment color should be blended with the background.

The value should be a floating-point number in the range [0..1], where a lower
value will translate to a softer comment (i.e. to a bigger difference)."
  :group 'soft-comment
  :type 'float
  :validate #'soft-comment--validate-scale
  :risky t)

(defcustom soft-comment-face 'font-lock-comment-face
  "Symbol representing the name of the font used for comments."
  :group 'soft-comment
  :type 'face
  :risky t)

(defvar soft-comment--old-foregound nil
  "The last foreground color of `soft-comment-face' before enabling the mode.")

(defun soft-comment--validate-scale (widget)
  "Validate that the value in the specified WIDGET is a valid float.

The scale is expected to be in the [0..1] range."
  (let ((value (widget-value widget)))
    (if (and (floatp value)
             (>= value 0.0)
             (<= value 1.0))
        nil
      (widget-put widget :error "Invalid range, expected [0..1]")
      widget)))

;; NOTE: The `cl-mapcar' function could be used similarly to
;; `soft-comment--color-mult', but it would add a package/version dependency.
(defun soft-comment--color-add (a b)
  "Add each RGB component in color A to color B."
  (list (+ (car   a) (car   b))
        (+ (cadr  a) (cadr  b))
        (+ (caddr a) (caddr b))))

(defun soft-comment--color-mult (color scalar)
  "Multiply each RGB component of COLOR by SCALAR."
  (mapcar (lambda (component)
            (* component scalar))
          color))

(defun soft-comment--color-blend (foreground background &optional ratio)
  "Blend the FOREGROUND and BACKGROUND colors.

The optional RATIO argument specifies how much \"opacity\" the foreground should
have. If ommited, the value of `soft-comment-ratio' is used.

Each color is supposed to be a 3-element list containing its RGB values."
  (unless ratio (setq ratio soft-comment-ratio))
  (soft-comment--color-add
   (soft-comment--color-mult foreground ratio)
   (soft-comment--color-mult foreground (- 1.0 ratio))))

(defun soft-comment--enable ()
  "Enable `soft-comment-mode'."
  (let ((current-foreground (face-attribute soft-comment-face :foreground))
        (current-background (face-attribute 'default :background)))
    (setq soft-comment--old-foregound current-foreground)
    (set-face-attribute soft-comment-face nil :foreground
                        (soft-comment--color-blend current-foreground
                                                   current-background
                                                   soft-comment-ratio))))

(defun soft-comment--disable ()
  "Disable `soft-comment-mode'."
  (when soft-comment--old-foregound
    (set-face-attribute soft-comment-face nil :foreground
                        soft-comment--old-foregound)
    (setq soft-comment--old-foregound nil)))

;;;###autoload
(define-minor-mode soft-comment-mode
  "Minor mode for softening the color of comments.

Useful for temporarily ignoring comments, effectively focusing on actual code."
  :init-value nil
  :global t
  (if soft-comment-mode
      (soft-comment--enable)
    (soft-comment--disable)))

(provide 'soft-comment)
;;; soft-comment.el ends here
