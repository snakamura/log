# Apply svn:ignore to git using git svn

When you checkout a subversion repository with git svn, you may find that files listed in svn:ignore appear when you use `git status`.

There are two ways to apply svn:ignore to git repository. First, you can utilize .git/info/exclude. You may generate it using `git svn show-ignore`.

    git svn show-ignore >> .git/info/exclude

Second, you can convert every `svn:ignore` to `.gitignore` file. You may convert them by `git svn create-ignore`, and commit them to the original repository.

    git svn create-ignore
    git commit
    git svn dcommit
