import os , sys, subprocess
import numpy as np

# Taking inputs from the user
branchName = input("Enter the branch name: ")
pathToFile = input("Ether the file name with path: ")
lineNo = np.array([input(
    "Enter the start line number and end line number ( separated by , eg. 10,15 ) of snippet of code: ")])

# Function to get the StartSHA( latest commit) of the branch
def git_branch_sha():
    command = 'git rev-parse ' + branchName
    process = subprocess.Popen(command,
                               stdout=subprocess.PIPE,
                               universal_newlines=True,
                               shell=True,
                               cwd=os.getcwd())
    stdout = process.communicate()[0]
    return stdout

# Function to retrieve relevant commit history
def git_output(start, line, path):
    command = 'git log {} --pretty=short -u -L {}:{}|grep -is commit|sed -n /^commit/p|cut -d " " -f2'.format(
        start, line, path)
    process = subprocess.Popen(command,
                               stdout=subprocess.PIPE,
                               universal_newlines=True,
                               shell=True,
                               cwd=os.getcwd())

 # Retrieving Array of commit hash
    commit_list = []
    for commits in process.stdout:
        commit_list.append(commits)
    commit_final = [i[:-1] for i in commit_list]
    commit_array = np.asarray(commit_final)
    print(commit_array)

# Calling function to get the StartSHA
startSha = git_branch_sha()

# Calling function to get the relevant commit history
# Input parameters startSha, lineNo,pathToFile
# startSha - Retrieved from the function git_branch_sha()
git_output(startSha[:-1], lineNo[0], pathToFile)
