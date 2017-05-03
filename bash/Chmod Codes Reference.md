Chmod provides distinct values for setting permissions on executables. Values are listed in the following table.

|Value |Permission Set             |
|------|---------------------------|
|4000  |Sets user ID on execution. |
|2000  |Sets group ID on execution.|
|1000  |Sets the link permission to directories or sets the save text attribute for files.
|0400  |Permits read by owner.
|0200  |Permits write by owner.
|0100  |Permits execute or search by owner.
|0040  |Permits read by group.
|0020  |Permits write by group.
|0010  |Permits execute or search by group.
|0004  |Permits read by others.
|0002  |Permits write by others.
|0001  |Permits execute or search by others.

The following handy chmod will make a file read, write, and executable by owner, read and exectuable by group, and read by the world:

```sh
chmod 754 my_script.sh
```

A variant of the prior command:

```sh
chmod u+rwx,g+rx,o+r my_script.sh
```

The alternative letter argument is fairly straightforward:

u = user/owner

g = group

o = others

+ = add permissions

r = read

w = write

x = execute