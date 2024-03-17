# Retrieve branches on a subversion repository using git-svn

When you clone a svn repository using `git svn clone` it fetches all branches in the svn repository and makes them remote branches of git. However, when you create a new branch on the svn repository and try to retrieve the branch using `git svn rebase`, you'll find there is no remote branch corresponding to the svn branch.

To fetch a svn branch and make it a remote branch of git, you need to fetch it explicitly using:

    git svn fetch
