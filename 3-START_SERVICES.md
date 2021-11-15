# Start services

## Prepare configuration data
- create directories for node config
  ```shell
  mkdir $HOME/pab-config
  cd $HOME/pab-config
  mkdir testnet
  ```
- fetch the testnet node config files
  ```shell
  cd testnet
  NODE_BUILD_NUM=$(curl https://hydra.iohk.io/job/Cardano/iohk-nix/cardano-deployment/latest-finished/download/1/index.html | grep -e "build" | sed 's/.*build\/\([0-9]*\)\/download.*/\1/g')
  wget https://hydra.iohk.io/build/${NODE_BUILD_NUM}/download/1/testnet-byron-genesis.json
  wget https://hydra.iohk.io/build/${NODE_BUILD_NUM}/download/1/testnet-topology.json
  wget https://hydra.iohk.io/build/${NODE_BUILD_NUM}/download/1/testnet-shelley-genesis.json
  wget https://hydra.iohk.io/build/${NODE_BUILD_NUM}/download/1/testnet-alonzo-genesis.json
  wget https://hydra.iohk.io/build/${NODE_BUILD_NUM}/download/1/testnet-config.json
  ```
- fetch the PAB config and chainindex config files for `v2021-11-05` tag of `plutus-apps`
  ```shell
  # ensure current working directory is $HOME/pab-config/testnet
  # fetch appropriate config files
  wget https://github.com/input-output-hk/plutus-apps/blob/v2021-11-05/plutus-pab/test-node/testnet/chain-index-config.json
  wget https://github.com/input-output-hk/plutus-apps/blob/v2021-11-05/plutus-pab/test-node/testnet/pab-config.yml
  ```
## Run Cardano node relay with testnet config
- **In term 1**, run `cardano-node` on port 3003
  ```shell
  cd $HOME/pab-config
  cardano-node -- run \
  --config testnet/testnet-config.json \
  --topology testnet/testnet-topology.json \
  --database-path testnet/db \
  --socket-path testnet/node.sock \
  --port 3003
  ```

- **In another terminal**, ensure the node has fully synced against the testnet
  - We can use cardano-cli to check if the node has fully synchronized
      ```shell
      cd $HOME/pab-config
      export CARDANO_NODE_SOCKET_PATH=testnet/node.sock
      cardano-cli query tip --testnet-magic 1097911063    
      ```
  - Compare tip info to `cardanoscan` data
    - Visit [cardanoscan website](https://testnet.cardanoscan.io/)
    - Compare the block/slot information with the results of the `cardano-cli query tip` to make sure they agree

## Run the Plutus chain index
- **In a new terminal**, run `plutus-chain-index`.
  ```shell
  cd $HOME/pab-config
  cabal run plutus-chain-index -- start-index --config testnet/chain-index-config.json
  # this will store the chain data in /tmp folder based on the `chain-index-config.json`
  ```
- wait for it to fully sync - query chain-index tip to monitor the progress
  ```shell
  curl http://localhost:9083/tip
  # compare the tipBlockNo to the cardano node tip block 
  ```

## Run the wallet API server
TODO: WHY 8090
This step starts a wallet API server listening on port 8090. We can use this wallet server API for: blah, blah
- **In a new terminal**, run `cardano-wallet`
  ```shell
  cd $HOME/pab-config
  cardano-wallet serve \
  --testnet testnet/testnet-byron-genesis.json \
  --node-socket testnet/node.sock
  ```
- ensure fully synced (it only takes a few minutes)
  In the STDOUT logging, look for a message like the following 
  ```log
  [cardano-wallet.pools-engine:Notice:45] [2021-11-15 18:16:56.41 UTC] In sync! 
  ```

## Set up a test wallet and load some tAda into it
- **In a new terminal**, run the `cardano-wallet` executable in command line mode to create a recovery phrase.
  We than create a `restore-wallet.json` config file, which includes the recovery phrase words.
  ```shell
  cd $HOME/pab-config
  
  # generate recovery phrase words and turn into a comma separated list of quoted words
  RECOVERY_WORDS=$(cardano-wallet recovery-phrase generate |  sed -e 's| |", "|g' -e 's|^|"|g' -e 's|$|"|g')
  
  # create the restore-wallet.json file in the testnet folder
  cat > testnet/restore-wallet.json << EOF 
  { "name": "PAB testing wallet"
    , "mnemonic_sentence": [${RECOVERY_WORDS}]
    , "passphrase": "pab123456789"
  }
  EOF
  ```
- **In same terminal**, post to the v2/wallets endpoint to create a new wallet
  ```shell
  curl -H "content-type: application/json" -XPOST \
  -d @testnet/restore-wallet.json \
  localhost:8090/v2/wallets
  
  # take note of the wallet ID created
  ```
- **In same terminal**, load some tAda into some address of the wallet we just created
  ```shell
  # create env. variable
  export WALLET_ID=<paste wallet ID you just created above>
  
  # get wallet info for the wallet including addresses
  curl -H "content-type: application/json" \
      -XGET localhost:8090/v2/wallets/$WALLET_ID/addresses | jq '.'
     
  ```
- Get some tAda from the [testnet faucet](https://testnets.cardano.org/en/testnets/cardano/tools/faucet/)

## Set up a PAB database and start your application PAB web server
The PAB web server process allows client callers to interact with the application contract schema endpoints
- **In new terminal**, run `pab-pay-to-wallet` in command line mode to migrate schema to set up the contract database.
  ```shell
  cd $HOME/pab-config
  cabal run pab-pay-to-wallet --config testnet/pab-config.yml migrate  
  ```
- **In same terminal**, start a PAB webserver up
  ```shell
  cabal run pab-pay-to-wallet \
  --config testnet/pab-config.yml webserver \
  --passphrase pab123456789
  ```
  