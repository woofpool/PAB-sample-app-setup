# Install Core Executables

This guide covers installing `cardano-node`, `cardano-cli` into `$HOME/.local/bin`

If necessary, edit your `$HOME/.bashrc` to modify the PATH variable so that the executables can be found on your system path
  ```shell
  export PATH="$HOME/.local/bin:$PATH"  
  ```
## Download the pre-built binaries from IOHK
- Go to the [README page](https://github.com/input-output-hk/cardano-node#linux-executable) of the `cardano-node` project
  and you will see links to follow, where you can download the latest release binaries.
- Copy the binaries to local user path
  ```shell
  # extract cardano-cli and cardano-node from the archive
  # copy them to local path location
  cp cardano-cli $HOME/.local/bin/
  cp cardano-node $HOME/.local/bin/
  ```
- Verify the versions
  ```shell
  cardano-node version
  cardano-cli version
  
  # when this document was written, the current version for each is 1.31.0 on linux-x86_64
  ```  