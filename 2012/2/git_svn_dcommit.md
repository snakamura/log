# Recovering from a failure of git svn dcommit

Assume you're using git-svn and have committed some changes to git. When you `dcommit`ted these changes to the upstream subversion repository, something went wrong and you found that the local working directory was left weird state.

To recover from these situations, first see reflog to find the hash of the last commit.

    $ git reflog
    0012345 HEAD@{0}: reset: moving to refs/remotes/trunk
    5678902 HEAD@{1}: commit: This is the last commit.
    3556516 HEAD@{2}: commit: Made some changes.

In this case it's 5678902. Now you can reset the working directory.

    $ git reset --hard 5678902

Then rebase to the upstream subversion repository.

    $ git svn rebase
