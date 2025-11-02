(defun move-line-up (arg)
  "Drag current line to previous line, keeping point on current line.
With argument ARG, takes current line and moves it past ARG lines."
  (interactive "*p")
  (let ((original-column (current-column)))
    (dotimes (i arg)
      (transpose-lines 1)
      (previous-line 2)
      (move-to-column original-column))))

(defun move-line-down (arg)
  "Drag current line to next line, keeping point on current line.
With argument ARG, takes current line and moves it past ARG lines."
  (interactive "*p")
  (let ((original-column (current-column)))
    (dotimes (i arg)
      (next-line)
      (transpose-lines 1)
      (previous-line 1)
      (move-to-column original-column))))

(provide 'init-functions)
