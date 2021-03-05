
(setq package-native-compile t)

(setq lsp-ui-doc-enable nil)


(setq company-idle-delay 0.3)


;; Vertical Scroll
;; (setq scroll-step 1)
;; (setq scroll-margin 1)
;; (setq scroll-conservatively 101)
;; (setq scroll-up-aggressively 0.01)
;; (setq scroll-down-aggressively 0.01)
;; (setq auto-window-vscroll nil)
;; (setq fast-but-imprecise-scrolling nil)
;; (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
;; (setq mouse-wheel-progressive-speed nil)
;; ;; Horizontal Scroll
;; (setq hscroll-step 1)
;; (setq hscroll-margin 1)















(global-set-key [s-left] 'windmove-left)
(global-set-key [s-right] 'windmove-right)
(global-set-key [s-up] 'windmove-up)
(global-set-key [s-down] 'windmove-down)



(global-set-key [(s ?\.)] 'ska-point-to-register)
(global-set-key [(s ?\,)] 'ska-jump-to-register)
(defun ska-point-to-register()
  "Store cursorposition _fast_ in a register.
Use ska-jump-to-register to jump back to the stored
position."
  (interactive)
  (setq zmacs-region-stays t)
  (point-to-register 8))

(defun ska-jump-to-register()
  "Switches between current cursorposition and position
that was stored with ska-point-to-register."
  (interactive)
  (setq zmacs-region-stays t)
  (let ((tmp (point-marker)))
    (jump-to-register 8)
    (set-register 8 tmp)))
