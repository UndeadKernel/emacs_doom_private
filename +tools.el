;;; +tools.el --- description -*- lexical-binding: t; -*-

;; LLM tools configuration
(after! gptel
  (setopt gptel-default-mode #'org-mode
          gptel-prompt-prefix-alist '((markdown-mode . "# ") (org-mode . "* ") (text-mode . "# "))
          gptel-response-prefix-alist '((markdown-mode . "## /n") (org-mode . "** \n") (text-mode . "## /n"))
          gptel-model 'gpt-4o-mini)

  ;; Integrate with tools defined with mcp.el
  (require 'gptel-integrations)

  ;; Presets
  (gptel-make-preset 'word-live
    :description "Word editing expert" :backend "ChatGPT" :model 'gpt-5.2
    :system "You are a Microsoft Word writing assistant. You have access to many tools to modify Word documents. When possible, you prefer using live tools to edit Word files."
    :tools
    '("verify_document" "add_digital_signature" "add_restricted_editing"
      "merge_documents" "add_table_of_contents" "add_watermark" "add_bookmark"
      "set_paragraph_spacing" "add_section_break" "add_page_numbers"
      "add_header_footer" "set_page_layout" "word_live_add_watermark"
      "word_live_add_bookmark" "word_live_set_paragraph_spacing"
      "word_live_add_section_break" "word_live_add_page_numbers"
      "word_live_add_header_footer" "word_live_set_page_layout"
      "word_live_diagnose_layout" "word_live_undo" "word_live_get_undo_history"
      "word_live_get_page_text" "word_live_reject_revisions"
      "word_live_accept_revisions" "word_live_list_revisions"
      "word_live_add_comment" "word_live_get_comments" "word_live_find_text"
      "word_live_get_info" "word_live_get_paragraph_format" "word_live_get_text"
      "word_live_setup_heading_numbering" "word_live_apply_list"
      "word_live_delete_text" "word_live_modify_table" "word_live_format_table"
      "word_live_add_table" "word_live_replace_text" "word_live_format_text"
      "word_live_insert_text" "word_screen_capture" "reject_tracked_changes"
      "accept_tracked_changes" "list_tracked_changes" "track_delete"
      "track_insert" "track_replace" "set_table_cell_padding"
      "format_table_cell_text" "auto_fit_table_columns" "set_table_width"
      "set_table_column_widths" "set_table_column_width" "manage_hyperlinks"
      "add_comment" "get_comments_for_paragraph" "get_comments_by_author"
      "get_all_comments" "replace_block_between_manual_anchors"
      "replace_paragraph_block_below_header" "convert_to_pdf"
      "get_highlighted_text" "find_text_in_document"
      "get_paragraph_text_from_document" "delete_footnote_robust"
      "validate_document_footnotes" "add_footnote_robust"
      "delete_footnote_from_document" "customize_footnote_style"
      "add_endnote_to_document" "add_footnote_enhanced" "add_footnote_before_text"
      "add_footnote_after_text" "add_footnote_to_document" "unprotect_document"
      "protect_document" "set_table_alignment_all" "set_table_cell_alignment"
      "merge_table_cells_vertical" "merge_table_cells_horizontal"
      "merge_table_cells" "highlight_table_header" "apply_table_alternating_rows"
      "set_table_cell_shading" "format_table" "format_text" "create_custom_style"
      "search_and_replace" "delete_paragraph" "add_page_break" "add_table"
      "add_picture" "add_heading" "add_paragraph" "insert_numbered_list_near_text"
      "insert_line_or_paragraph_near_text" "insert_header_near_text"
      "get_document_xml" "list_available_documents" "get_document_outline"
      "get_document_text" "get_document_info" "copy_document" "create_document"
      "list_allowed_directories" "get_file_info" "search_files" "move_file"
      "directory_tree" "list_directory_with_sizes" "list_directory"
      "create_directory" "edit_file" "write_file" "read_multiple_files"
      "read_media_file" "read_text_file" "read_file")
    :stream t :temperature 1.0 :max-tokens nil :use-context 'system :track-media
    nil :include-reasoning t)

  (gptel-make-preset 'markitdown
    :description "Transform documents to markdown" :backend "ChatGPT" :model
    'gpt-4.1 :system
    "You have access to the markitdown tool. This tool will help you to fulfill any task that requires you to transform arbitrary documents (e.g., PDF files) into markdown files. Use the markitdown tool whenever you are asked to transform something to markdown."
    :tools
    '("list_allowed_directories" "get_file_info" "search_files" "move_file"
      "directory_tree" "list_directory_with_sizes" "list_directory"
      "create_directory" "edit_file" "write_file" "read_multiple_files"
      "read_media_file" "read_text_file" "read_file" "convert_to_markdown")
    :stream t :temperature 1.0 :max-tokens nil :use-context 'system :track-media
    nil :include-reasoning t)

)


;; ssh-deploy config
(after! ssh-deploy
  (setq ssh-deploy-on-explicit-save nil))

;; pdf-tools config
;; enable visual-line-mode in the buffer where annotations
;; ... are shown (*Contents*)
(after! pdf-tools
  (defun boy/annot-visual-line (_id _buffer)
    (let ((contents-buf (get-buffer "*Contents*")))
      (when (and contents-buf (not visual-line-mode))
        (with-current-buffer contents-buf
          (visual-line-mode)))))
  ;; advice the function responsible for creating the *Contents* buffer
  (advice-add 'pdf-annot-list-context-function :after #'boy/annot-visual-line))
;; more fine-grained zooming
(setq pdf-view-resize-factor 1.1)
;; set some default properties for all annotations
(setq pdf-annot-default-annotation-properties
      '((t
         (label . "carlos")
         (color . "#483d8b"))
        (text
         (icon . "Note"))
        (highlight
         (color . "#eee8aa"))
        (squiggly
         (color . "orange"))
        (strike-out
         (color . "red"))))

;; Magit config
(add-hook! magit-mode (visual-line-mode +1))
;; search for Magit passwords in `auth-sources'
(add-hook 'magit-process-find-password-functions 'magit-process-password-auth-source)

;; EIN config
(setq +ein-notebook-dir "~")
(add-hook! ein:notebook-multilang-mode
  (map! :map ein:notebook-mode-map
        "M-p" #'+boy/up-scroll
        "M-n" #'+boy/down-scroll))

