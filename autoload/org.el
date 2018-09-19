;;; ~/.doom.d/autoload/org.el -*- lexical-binding: t; -*-

;;;###autoload (autoload '+boy/org-babel-hydra/body "~/.doom.d/autoload/org.el" nil t)
(defhydra +boy/org-babel-hydra (:hint nil)
    "
Org-Babel:
_n_:next      _c_:clear results  _i_:show all
_p_:previous  _h_:show/hide      _I_:hide all
_e_:edit      _RET_:execute      _l_:center screen
_g_:goto      _s_:split          _q_:cancel
"
    ("c" org-babel-remove-result)
    ("RET" org-babel-execute-src-block)
    ("e" org-edit-src-code)
    ("h" org-hide-block-toggle-maybe)
    ("s" org-babel-demarcate-block)
    ("g" org-babel-goto-named-src-block)
    ("i" org-show-block-all)
    ("I" org-hide-block-all)
    ("n" org-babel-next-src-block)
    ("p" org-babel-previous-src-block)
    ("l" recenter-top-bottom)
    ("q" nil :color blue))
