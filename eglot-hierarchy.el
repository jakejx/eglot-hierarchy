;;; eglot-hierarchy.el --- Add support for displaying incoming and outgoing call hierarchies -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Jake Ng <jakejx.ng@gmail.com>

;; Author: Jake Ng
;; Created: 2023
;; License: GPL-3.0-or-later
;; Version: 0.1
;; Homepage: https://github.com/jakejx/eglot-hierarchy
;; Package-Requires: ((emacs "29.1") (eglot "1.15"))

;;; Commentary:
;; eglot-hierarchy add supports for displaying the incoming and outgoing call hierarchies using Eglot.
;;
;; LSP requires 2 steps to retrieve the call hierarchy for a symbol.
;; 1. A textDocument/prepareCallHierarchy call to retrieve information about the symbol at point.
;; 2. Using the response, a subsequent textDocument/{incoming,outgoing}Calls to retrieve the call hierarchy.
;; 
;; Main entrypoints:
;; * eglot-hierarchy-incoming-calls
;; * eglot-hierarchy-outgoing-calls

(require 'hierarchy)
(require 'eglot)
(require 'button)
(require 'tree-widget)

;;; Code:
(defun eglot-hierarchy--prepare-root-item (server method)
  "Prepare root item for METHOD using SERVER."
  (if-let* ((prepare-res (eglot--request server :textDocument/prepareCallHierarchy (eglot--TextDocumentPositionParams)))
            (root-item (aref prepare-res 0))
            (root-range (plist-get root-item :selectionRange))
            (field (eglot-hierarchy--method-to-field method))
            (root `(,field ,root-item :fromRanges [,root-range])))
      root
    (user-error "No calls for symbol at point")))

(defun eglot-hierarchy--result-jump-range (result method)
  "Get the jump range for RESULT based on the METHOD.
For incoming calls should jump to :fromRange, but outgoing calls should jump to
the :to. Returns the range to jump to."
  (pcase method
    (:callHierarchy/incomingCalls (aref (plist-get result :fromRanges) 0)) ;; TODO: handle more than one fromRanges
    (:callHierarchy/outgoingCalls (plist-get (plist-get result :to) :range))))

(defun eglot-hierarchy--method-to-field (method)
  "Return the field that points to the CallHierarchyItem bsaed on METHOD.
incomingCalls places the item in :from while outgoingCalls places it in :to."
  (pcase method
    (:callHierarchy/incomingCalls :from)
    (:callHierarchy/outgoingCalls :to)
    (_ (error "Invalid call hierarchy method"))))

(defun eglot-hierarchy--make-call-tree (root server method)
  "Make a call tree for ROOT with eglot SERVER using METHOD.
METHOD should be either :callHierarchy/incomingCalls or
:callHierarchy/outgoingCalls."
  (eglot-server-capable-or-lose :callHierarchyProvider)
  (when-let* ((hierarchy (hierarchy-new))
              (location (eglot-hierarchy--method-to-field method))
              (labelfn (hierarchy-labelfn-button (eglot-hierarchy--labelfn method) (eglot-hierarchy--goto-result method)))
              (childfn (lambda (item)
                         ;; item is HierarchyItem
                         ;; TODO: remove unnecessary let
                         (let ((res (eglot--request server method `(:item ,(plist-get item location)))))
                           res))))
    (hierarchy-add-tree hierarchy root nil childfn nil t)
    (hierarchy-convert-to-tree-widget hierarchy labelfn)))

(defun eglot-hierarchy--labelfn (method)
  "Return the function to create the label.
METHOD is used to select the right field in the response."
  (lambda (result _)
    (let* ((from (plist-get result (eglot-hierarchy--method-to-field method)))
           (name (plist-get from :name))
           (detail (plist-get from :detail)))
      (insert name)
      (insert " ")
      (insert (propertize detail 'face 'eglot-inlay-hint-face)))))

(defun eglot-hierarchy--goto-result (method)
  "Return the function to go to the result based on the METHOD."
  (lambda (result _)
    (let* ((item (plist-get result (eglot-hierarchy--method-to-field method)))
           (path (eglot-uri-to-path (plist-get item :uri)))
           (range (eglot-hierarchy--result-jump-range result method)))
      (find-file-other-window path)
      (goto-char (car (eglot-range-region range))))))

(defun eglot-hierarchy--display-call-tree (method &optional buffer)
  "Display the call tree in BUFFER for METHOD.

If BUFFER is provided, it will be used instead of creating a new buffer."
  (let* ((server (eglot--current-server-or-lose))
         (root (eglot-hierarchy--prepare-root-item server method))
         (tree-widget (eglot-hierarchy--make-call-tree root server method))
         (buffer (or buffer (generate-new-buffer (concat "Incoming calls: " (plist-get root :name))))))
    (with-current-buffer buffer
      (setq-local buffer-read-only t)
      (let ((inhibit-read-only t))
        (eglot-hierarchy-mode)
        (widget-create tree-widget)
        (goto-char (point-min))))
    (select-window (display-buffer buffer '((display-buffer-in-side-window)
                                            (side . bottom)
                                            (window-height . 0.3))))))

(defun eglot-hierarchy-incoming-calls (&optional buffer)
  "Display the incoming call tree in BUFFER.

If BUFFER is provided, it will be used instead of creating a new buffer."
  (interactive)
  (eglot-hierarchy--display-call-tree :callHierarchy/incomingCalls buffer))

(defun eglot-hierarchy-outgoing-calls (&optional buffer)
  "Display the outgoing call tree in BUFFER.

If BUFFER is provided, it will be used instead of creating a new buffer."
  (interactive)
  (eglot-hierarchy--display-call-tree :callHierarchy/outgoingCalls buffer))

;; inspired/stolen from dape - this seems like something tree-widget should have built in
(defun eglot-hierarchy--press-widget-at-line (predicate)
  "Press first widget on current line matching PREDICATE."
  (save-excursion
    (if (funcall predicate (widget-at))
        (widget-button-press (point))
      (pcase-let ((`(,start . ,end) (bounds-of-thing-at-point 'line))
                  (found))
        (goto-char start)
        (while (and (not found)
                    (< (point) end))
          (cond
           ((funcall predicate (widget-at))
            (widget-button-press (point))
            (setq found t))
           ((eobp) (setq found t))
           (t (goto-char (next-overlay-change (point))))))))))

;; inspired/stolen from dape.
;; TODO: maybe have a specific button type?
(defun eglot-hierarchy-select-line ()
  "Press the action button on the current line."
  (interactive)
  (save-excursion
    (if (button-at (point))
        (push-button (point))
      (pcase-let ((`(,start . ,end) (bounds-of-thing-at-point 'line))
                  (found))
        (goto-char start)
        (while (and (not found)
                    (< (point) end))
          (cond
           ((button-at (point))
            (push-button (point))
            (setq found t))
           ((eobp) (setq found t))
           (t (forward-button 1 t nil t))))))))

(defun eglot-hierarchy-tree-dwim ()
  "Toggle tree expansion in *eglot hierarchy* buffer."
  (interactive)
  (eglot-hierarchy--press-widget-at-line (lambda (widget)
                                           (memq (widget-type widget)
                                                 '(tree-widget-open-icon tree-widget-close-icon)))))

(defvar eglot-hierarchy-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<tab>") #'eglot-hierarchy-tree-dwim)
    (define-key map (kbd "<return>") #'eglot-hierarchy-select-line)
    map)
  "Keymap active in *eglot-hierarchy* buffers.")

(define-derived-mode eglot-hierarchy-mode special-mode "Eglot Hierarchy"
  :group 'eglot-hierarchy
  :interactive nil
  (let ((inhibit-read-only t))
    (erase-buffer))
  (setq-local buffer-read-only t
              truncate-lines t
              indent-tabs-mode nil
              tree-widget-image-enable nil))

(provide 'eglot-hierarchy)
;;; eglot-hierarchy.el ends here
