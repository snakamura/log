# Installing Python using asdf on macos

```
# Install asdf and load it in .bash_profile
brew install asdf
echo -e "\n. $(brew --prefix asdf)/asdf.sh" >> ~/.bash_profile

# Load it in this shell
. ($brew --prefix asdf)/asdf.sh

# Add python plugin and install python 3.7.12
asdf plugin add python
asdf install python 3.7.12

# Set a python version in this directory
cd foo
asdf local python 3.7.12
```
