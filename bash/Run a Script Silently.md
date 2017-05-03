We can redirect shell output to run a script ’siletly’ or in ’silent-mode’. Scripts run in silent mode do not disaply output to the user. A silent mode function is useful for particular scripts, especially those which are:

* Frequently run.
* Average long run times.
* Is run on a schedule.

To execute a command silently, redirect its output to `/dev/null`:

```sh
ls 2>&1 /dev/null
```

The prior command redirects the standard and error output of the `ls` command to the /dev/null folder, ensuring it is not displayed.