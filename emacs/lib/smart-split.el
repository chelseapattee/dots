;;; smart-split -- Summary
;;; Commentary:

;;; Code:
(defvar smart-split--columns 100)

(defun smart-split--ordered-window-list ()
  "Get the list of windows in the selected frame, starting from the one at the top left."
  (window-list (selected-frame) 'no-minibuf (frame-first-window)))

(defun smart-split--resize-windows-destructively (windows)
  (when windows
    (condition-case nil
        (progn
          (adjust-window-trailing-edge
           (first windows)
           (- smart-split--columns (window-body-width (first windows))) t)
          (smart-split--resize-windows-destructively (cdr windows)))
      (error
       (if (cdr windows)
           (progn
             (delete-window (cadr windows))
             (smart-split--resize-windows-destructively (cons (car windows)
                                                    (cddr windows))))
         (ignore-errors (delete-window (car windows))))))))

(defun smart-split--subsplit (w)
  (when (> (window-body-width w) (* 2 (+ 1 smart-split--columns)))
    (let ((w2 (split-window w (+ 2 smart-split--columns) 'right)))
      (save-excursion
        (smart-split--subsplit w2)))))

(defun smart-split ()
  "Split the frame into exactly as many 80-column sub-windows as possible."
  (interactive)
  (smart-split--resize-windows-destructively (smart-split--ordered-window-list))
  (walk-windows 'smart-split--subsplit)
  (balance-windows))

(provide 'smart-split)
;;; smart-split.el ends here
