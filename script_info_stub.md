# Stub for providing script information at the beginging of scripts, simply copy and paste into all new scripts.
(better yet, write a vim script that auto populates new sh files with this stub.)

Cribbed from Mastering Unix Shell Scripting.

```sh
#!/bin/bash
#
# SCRIPT: Name
# AUTHOR: Author
# DATE: Creation Date
# VERSION: 1.0 (use a coherent and consistent versioning scheme)
# TYPE: Type (type, user, system, admin, background, etc.)
# 
# PLATFORM: (Linux, OSX, BSD, Solaris, etc. comma separate)
#
# PURPOSE: Breif description of the intended purpose of the script. 
#
# REV LIST: 
# 	DATE: Last Modified Date
# 	BY: Author
# 	MODIFICATION: Description of Change
# 	REPOSITORY URL: URL to Git repo 
#
# set -n # uncomment to check script syntax without
# 	 # executing the script.
# 	 # Be sure to recomment or the script will
# 	 # not exectue
# set -x # uncomment to debug this script
#
####################################
# VARIABLES
####################################

####################################
# FUNCTIONS
####################################

####################################
# MAIN
####################################
```
