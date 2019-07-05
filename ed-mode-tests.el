(ert-deftest ed-mode-starts ()
  (with-temp-buffer
    (ed)
    (should (equal major-mode 'ed-mode))))
