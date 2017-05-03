Cron tables enable us to specify reccuring run times for shell scripts and other jobs. A cron table file specifies when the system should run a given command and requires the following syntax: 

**NOTE:** Wildcards `*` and `?` are supported for each cron table time value. 

\<Minute\> \<Hour\> \<Day\> \<Month\> \<WeekDay\> \<command\>

e.g.

`12 3 4 5 \* ls`

will run the `ls` command on May 4th, no matter what day of the week it is, at precisely 3:12 a.m.

To view a list of current cron tables, run `crontab -l`. To create a new cron table, run `crontab -e`