# Haskell Group 1 (best group)

idk if either of you have used GitHub before, but if you haven't it'll be good practice (plus i haven't really used it much for collaboration so good practice for me as well).

i'd recommend using the command line. if you're using windows i think you need to download "git bash", or you can probably use the WSL (windows subsystem for linux) instead. 

it'll also make your lives easier if you set up SSH keys for your accounts: https://docs.github.com/en/authentication/connecting-to-github-with-ssh

anyway yeah once git is installed open up your bash terminal, `cd` to the folder you want to store this repo in (it'll create a new folder in the current folder), and use the following command

```git clone git@github.com:jclmnop/cs205_cw1.git```

then change into this directory with `cd cs205_cw1`

by default, you'll be on the "master" branch, this'll be the branch for our final submission basically.

```git branch``` this command will show you a list of all the branches. we each have our own branch that we can do all our work on. then later on we can figure out what we'll include in our final submission and start working on the master branch. 

to change to your branch, just use the command `git checkout <branch-name>`

so i'd use `git checkout jonny` to switch to my branch, for example. 
