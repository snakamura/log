# Show diff of the specified commit with its parent

It's annoying to type something like

    git diff 8b5d1d1^..8b5d1d1

every time you want git to show the diff of the specified commit. You can use `8b5d1d1^!` as a shorthand for `8b5d1d1^..8b5d1d1`, which is described in [man git-rev-parse](http://www.kernel.org/pub/software/scm/git/docs/git-rev-parse.html).

    git diff 8b5d1d1^!

You may want to use `show` command to show diff with additional information.

    git show 8b5d1d1
