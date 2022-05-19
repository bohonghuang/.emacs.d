;;;  -*- lexical-binding: t; -*-
(require 'dbus)

(defcustom gnome-color-scheme-theme-light 'modus-operandi
  "Emacs theme corresponding to GNOME light color scheme.")

(defcustom gnome-color-scheme-theme-dark 'modus-vivendi
  "Emacs theme corresponding to GNOME dark color scheme.")

(use-package dbus
  :ensure nil
  :demand t
  :init
  (defun gnome-color-scheme-switcher (value)
    (pcase value
      (0 (consult-theme gnome-color-scheme-theme-light))
      (1 (consult-theme gnome-color-scheme-theme-dark))
      (2 (consult-theme gnome-color-scheme-theme-light))
      (_ (message "Invalid key value"))))
  (defun gnome-color-scheme-callback (value)
    (gnome-color-scheme-switcher (car (car value))))
  (defun gnome-color-scheme-signal-handler (namespace key value)
    (if (and
         (string-equal namespace "org.freedesktop.appearance")
         (string-equal key "color-scheme"))
        (gnome-color-scheme-switcher (car value))))
  :config
  (dbus-call-method-asynchronously
   :session
   "org.freedesktop.portal.Desktop"
   "/org/freedesktop/portal/desktop"
   "org.freedesktop.portal.Settings"
   "Read"
   #'gnome-color-scheme-callback
   "org.freedesktop.appearance"
   "color-scheme")
  (dbus-register-signal
   :session
   "org.freedesktop.portal.Desktop"
   "/org/freedesktop/portal/desktop"
   "org.freedesktop.portal.Settings"
   "SettingChanged"
   #'gnome-color-scheme-signal-handler))

(provide 'gnome-color-scheme)
;;; gnome-color-scheme.el ends here
