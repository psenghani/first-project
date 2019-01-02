# Data Engineer at Stepsize – take home exercise
The goal of this task is to implement an algorithm to retrieve the relevant Git history of any snippet of code, and to think about how this algorithm could be put into production and perform well.

## Requirements (Packages/Dependencies)
- python 3.x
- numpy

## How to run

From within your git repository, run the command from command prompt:
`python /<path to the script>/git-commit-history.py`

This will ask you to input 3 parameters:
- Enter the branch name: ( you need to specify the branch name for which you wish to find the commits For eg. development)
- Enter the file name with path: ( you need to specify the path of the file you wish to find the commits for For eg. /etc/bin/abc.py)
- Enter the start line number and end line number (separated by , eg. 10,15) of snippet of code: ( you need to specify line number range For eg. 10,15)

```
Enter the branch name: development
Ether the file name with path: /etc/bin/abc.py
Enter the start line number and end line number ( separated by , eg. 10,15 ) of snippet of code: 10,15i# first-project
```
