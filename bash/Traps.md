Another goodie from Mastering Unix Shell Scripting. We can control the signal returned by a process upon exit. This is often called setting a trap. The following are some trap signals and their meanings (values are taken from mac osX):

|signal|meaning|
|------|-------|
|0     | Normal exit. No issues. |
|1 SIGHUP| There was a hangup in the program.|
|2 SIGINT| Terminal interrupt (typically ^c)|
|3 SIGQUIT| Quit key.|
|4 SIGILL| Invalid instruction or insufficeint priveleges.|
|5 SIGTRAP| `trap` occured.|
|6 SIGABRT| Abort. Raised when the `abort` function is called.|
|7 SIGEMT| Raised when an emulator trap occurs. |
|8 SIGFPE| Raised when erroneous mathematical operation is executed. |
|9 SIGKILL| Kill command `kill -9`. Cannot trap.|
|10 SIGBUS| Raised on a bus error (address/meemory errors).|
|11 SIGSEGV| Raised on an invalid memory reference.|
|12 SIGSYS| Raised on a bad system call.
|13 SIGPIPE| Raised when a process attempts to pipe.
|14 SIGALRM| Raised when a time period set for an alarm function elapses. (Clock time)|
|15 SIGTERM| Kill command run with no option (default).|
|16 SIGURG| Raised when a socket has urgent data. |
|17 SIGSTOP| Stop. (Typically ^-z). Can't be ignored.|
|18 SIGTSTP| Raised to tell the controlling terminal to stop the process, raised by ^-z can be ignored.|
|19 SIGCONT| Instructs OS to continue a process stopped by SIGSTOP or SIGTSTP.|
|20 SIGCHLD| Raised when a child process terminates.|
|21 SIGTTIN| Sent when a process attempts to read in from tty.|
|22 SIGTTOU| Sent when a process attempts to read out to tty.|
|23 SIGIO| Raised when file descriptor is ready to perform IO. |
|24 SIGXCPU| Raised when CPU time limit is exceeded.|
|25 SIGXFSZ| Raised when file size limit is exceeded. |
|26 SIGVTALRM| Raised when a time period set for an alarm function elapses. (Cpiu time used by process)|
|27 SIGPROF| Raised when a time period set for an alarm function elapses. (Cpu time used by process and system)|
|28 SIGWINCH| Raised when a controlling terminal changes in size. |
|29 SIGINFO| Raised when an info request is recieved from controlling terminal. |
|30 SIGUSR1| Signal for a user defined condition.|
|31 SIGUSR2| Signal for a user defined condition. |

It's good practice to catch and send these signals so that we can do some clean up based on the error the program encountered (e.g. there may be som folders created mid process that we ought to delete.)

To catch a signal we use the `trap` command as follows:

```sh
trap `echo "Exiting on trapped signal"; exit`
```



