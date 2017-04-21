Move all files contained in a child directory into the current working directory.

```sh
find . -maxdepth 1 -exec mv {} .. \;
```

Alternatively:

```sh
mv * ../
```