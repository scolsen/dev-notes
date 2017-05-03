Set log file permissions to monitoruser activity—we give the user access then set to root only at the close of the session (don’t be devious!):

```sh
TS=$(date +%m%d%y%H%M%S) #LOGFIL Timestamp
HOST=$(hostname | cut -f1-2 -d.) # Host machine name.
LOGDIR=/usr/local/logs # log storage
LOGFILE=$($HOST).$(LOGNAME).$TS #LOGFILE name
touch $LOGDIR/$LOGFILE

#set prompt
export PS1=*[$LOGNAME.$HOST]@"$PWD>"
# Run
chown $LOGNAME $(LOGDIR)/$(LOGFILE) # set user to owner, so his session can edit the file.
chmod 600 $(LOGDIR)/$(LOGFILE) # Give user rw permission on logfile to start logging.
script $(LOGDIR)/$(LOGFILE) # Start monitoring.

chown root $(LOGDIR)/$(LOGFILE) # change owner to root
chmod 400 $(LOGDIR)/$(LOGFILE) # set to read only by root.
```

Reference: Mastering Unix Shell Scripts