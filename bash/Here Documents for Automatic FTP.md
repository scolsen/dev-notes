We can set up an automatic FTP transfer using a here doc as follows:

```sh
ftp -i -v -n myftp <<END_FTP

user scott password
binary
lcd /scripts/download
cd /scripts
get auto_ftp.sh
bye

END_FTP
```

Reference: Mastering Unix Shell Scripting