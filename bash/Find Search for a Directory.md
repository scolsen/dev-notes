A simple find command to search for a directory by name. the / indicates we search from root, so the entire system is subject to a search. If you do not run with sudo find will lack permission to scan some directories. This may take some time, as the entirety of the machine is scanned.

```sh
find / -type d -name "directory"
```