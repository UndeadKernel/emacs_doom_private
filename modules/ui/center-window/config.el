;;; ui/center-window/config.el -*- lexical-binding: t; -*-

(defvar +center-window-size-percent 0.4
  "The resulting size of the main window in percentage (between 0 and 1).")

(defvar +center-window--empty-buffer "*empty-window*"
  "Name of an empty buffer used to emulate margins.")

(defvar +center-window--window-conf nil
  "Window configuration before centering a window.")
