# Disable vc-mode

When you edit files on a remote server with emacs, and the files are in a working directory of a version control system such as subversion or git, emacs may block long time after you save a file. It seems that's because vc-mode runs svn or git, which takes long time.

To disable vc-mode, set vc-handled-backends to nil. For example, you can set it in init.el:

    (setq vc-handled-backends nil)
