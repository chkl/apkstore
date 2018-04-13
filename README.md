# apkstore
simple and fast HTTP file server that stores apk files that can be accessed by their hash. 

(this was used during the development of the Android App Analysis Framework)

## Build

To build this application you will need the `stack` tool. In fact, this is the
only Haskell-related tool or library that you have to install at all. An
installation instruction can be found
[here](https://docs.haskellstack.org/en/stable/README/).

Make sure that the folder `~/.local/bin` is in your `PATH` variable.

To compile the project just type `stack build`. If this is the first-time you
are compiling this project, you might need to run `stack setup` which
automatically downloads and installes the right haskell compiler for you.

With `stack install` stack will copy the binary file `apkstore-server` into the
folder "~/.local/bin".  Since that folder should be in your `PATH` variable you
should know be able to start the server by typing `apkstore-server` into any
terminal.

If there are any questions ask Christian.


## Usage
The parameters of `apkstore-server` can be seen with `apkstore-server --help`.

The RESTful API currently contains three endpoints:

 * `GET /apk/<hash>` to download an apk with the given hash

 * `GET /apk/<hash>` like above but only returns a status code (no file)

 * `PUSH /apk` to upload an apk file

 * `GET /version` to see the version of the apkstore-server

 * `GET /status` some status information including the number of stored apks

APKs will be automatically uploaded and retrieved based on the `--basedir`
parameter. If that parameter is not set the default directory "./srv" (inside
your current working directory) is used.
