To run a process in the background without stopping on hangup:
`nohup <command> &` (writes command output to nohup.out in pwd)
`nohup <command> &>/dev/null &` (writes command output to /dev/null)
