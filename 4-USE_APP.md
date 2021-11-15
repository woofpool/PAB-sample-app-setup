# Interact with the contract

## Create test wallet #1 and give it some test ADA
- **In a new terminal**, create a `restore-wallet1.json` config file, which includes the recovery phrase words.
  ```shell
  cd $HOME/pab-config
  
  # generate recovery phrase words and turn into a comma separated list of quoted words
  RECOVERY_WORDS=$(cardano-wallet recovery-phrase generate |  sed -e 's| |", "|g' -e 's|^|"|g' -e 's|$|"|g')
  
  # create the restore-wallet1.json file in the testnet folder
  cat > testnet/restore-wallet1.json << EOF 
  { "name": "PAB testing wallet"
    , "mnemonic_sentence": [${RECOVERY_WORDS}]
    , "passphrase": "pab123456789"
  }
  EOF
  ```
- **In same terminal**, send post request to the v2/wallets endpoint to create a new wallet
  ```shell
  curl -H "content-type: application/json" -XPOST \
  -d @testnet/restore-wallet1.json \
  localhost:8090/v2/wallets
  
  # copy the wallet ID created, so we can paste it in the next step
  ```
- **In same terminal**, get the addresses of the wallet we just created
  ```shell
  # create env. variable
  export WALLET_ID=<paste wallet ID you just created above>
  
  # get wallet info for the wallet including addresses
  curl -H "content-type: application/json" \
      -XGET localhost:8090/v2/wallets/$WALLET_ID/addresses | jq '.'
     
  ```
- Get some test Ada from the [testnet faucet](https://testnets.cardano.org/en/testnets/cardano/tools/faucet/) and send it to
  one of the unused addresses of wallet1
    - On the page, you can paste an unused wallet address that you found above into the address field and submit the form
    - Wait to get success confirmation message. It will include the transaction hash code.
    - If you wait a minute or so, you can click on the link of the transaction hash to view the transaction in the explorer.
      The transaction should show the `To addresses` section including your test wallet address and the 1000 test ADA amount

## Create test wallet #2.  This wallet will receive some test ADA from test wallet #1
- **In a new terminal**, create a `restore-wallet2.json` config file, which includes the recovery phrase words.
  ```shell
  cd $HOME/pab-config
  
  # generate recovery phrase words and turn into a comma separated list of quoted words
  RECOVERY_WORDS=$(cardano-wallet recovery-phrase generate |  sed -e 's| |", "|g' -e 's|^|"|g' -e 's|$|"|g')
  
  # create the restore-wallet2.json file in the testnet folder
  cat > testnet/restore-wallet2.json << EOF 
  { "name": "PAB testing wallet"
    , "mnemonic_sentence": [${RECOVERY_WORDS}]
    , "passphrase": "pab123456789"
  }
  EOF
  ```
- **In same terminal**, send post request to the v2/wallets endpoint to create a new wallet
  ```shell
  curl -H "content-type: application/json" -XPOST \
  -d @testnet/restore-wallet2.json \
  localhost:8090/v2/wallets
  
  # copy the wallet ID created, so we can paste it in the next step
  ```
- **In same terminal**, get the addresses of the wallet we just created
  ```shell
  # create env. variable
  export WALLET_ID=<paste wallet ID you just created above>
  
  # get wallet info for the wallet including addresses
  curl -H "content-type: application/json" \
      -XGET localhost:8090/v2/wallets/$WALLET_ID/addresses | jq '.'

  
- interact with contract
    - In terminal 1
        - activate the contract for user1 wallet
        - invoke lock endpoint to lock value with a secret word
    - In terminal 2
        - activate the contract for user2 wallet
        - invoke guess endpoint
- verify the wallet transfer is made on successful guess