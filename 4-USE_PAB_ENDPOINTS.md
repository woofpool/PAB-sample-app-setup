# Use PAB endpoints

## Create test wallet #1 and give it some test ADA
- **In a new terminal**, create a `restore-wallet1.json` config file, which includes the recovery phrase words.
  ```shell
  cd $HOME/pab-config
  
  # generate recovery phrase words and turn into a comma separated list of quoted words
  RECOVERY_WORDS=$(cardano-wallet recovery-phrase generate |  sed -e 's| |", "|g' -e 's|^|"|g' -e 's|$|"|g')
  
  # create the restore-wallet1.json file in the testnet folder
  cat > testnet/restore-wallet1.json << EOF 
  { "name": "PAB test wallet #1"
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
  
  # get unused wallet addresses
  curl -H "content-type: application/json" \
      -XGET localhost:8090/v2/wallets/$WALLET_ID/addresses | jq '.'
     
  ```
- Get some test Ada from the [testnet faucet](https://testnets.cardano.org/en/testnets/cardano/tools/faucet/) and send it to
  one of the unused addresses of wallet1
    - On the page, you can paste an unused wallet address that you found above into the address field and submit the form
    - Wait to get success confirmation message. It will include the transaction hash code.
    - If you wait a minute or so, you can click on the link of the transaction hash to view the transaction in the explorer.
      The transaction should show the `To addresses` section including your test wallet address and the 1000 test ADA amount
- Send GET request to get wallet details and verify the test ADA balance
  ```shell
  curl -H "content-type: application/json" \
      -XGET localhost:8090/v2/wallets/$WALLET_ID
  ```
  Sample output
  ```log
  # confirm you see that the wallet has a balance of 1000 test ADA
  {"passphrase":{"last_updated_at":"2021-11-15T19:34:58.812854918Z"},"address_pool_gap":20,"state":{"status":"ready"},"balance":{"reward":{"quantity":0,"unit":"lovelace"},"total":{"quantity":1000000000,"unit":"lovelace"},"available":{"quantity":1000000000,"unit":"lovelace"}},"name":"PAB testing wallet","delegation":{"next":[],"active":{"status":"not_delegating"}},"id":"9e076253925172656de562da94bb79f303492299","tip":{"height":{"quantity":3076110,"unit":"block"},"time":"2021-11-15T21:43:17Z","epoch_number":169,"absolute_slot_number":42643381,"slot_number":4981},"assets":{"total":[],"available":[]}}
  ```
## Create test wallet #2 in 2nd terminal.  This wallet will receive some test ADA from test wallet #1
- **In a new terminal**, create a `restore-wallet2.json` config file, which includes the recovery phrase words.
  ```shell
  cd $HOME/pab-config
  
  # generate recovery phrase words and turn into a comma separated list of quoted words
  RECOVERY_WORDS=$(cardano-wallet recovery-phrase generate |  sed -e 's| |", "|g' -e 's|^|"|g' -e 's|$|"|g')
  
  # create the restore-wallet2.json file in the testnet folder
  cat > testnet/restore-wallet2.json << EOF 
  { "name": "PAB test wallet #2"
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
- Send GET request to get wallet details and verify the test ADA balance
  ```shell
  curl -H "content-type: application/json" \
      -XGET localhost:8090/v2/wallets/$WALLET_ID
  ```
  Sample output
  ```log
  # confirm you see that the wallet has a balance of 0 test ADA
  {"passphrase":{"last_updated_at":"2021-11-15T21:51:38.461168757Z"},"address_pool_gap":20,"state":{"status":"syncing","progress":{"quantity":32.57,"unit":"percent"}},"balance":{"reward":{"quantity":0,"unit":"lovelace"},"total":{"quantity":0,"unit":"lovelace"},"available":{"quantity":0,"unit":"lovelace"}},"name":"PAB test wallet #2","delegation":{"next":[],"active":{"status":"not_delegating"}},"id":"6b3deba51763744d4ad85451d8ee9c784d416d07","tip":{"height":{"quantity":1187971,"unit":"block"},"time":"2020-04-25T02:33:36Z","epoch_number":55,"absolute_slot_number":1189120,"slot_number":1120},"assets":{"total":[],"available":[]}}
  ```
## Activate the contract for each wallet

    - In terminal 1
        - activate the contract for user1 wallet
        - invoke lock endpoint to lock value with a secret word
    - In terminal 2
        - activate the contract for user2 wallet
        - invoke guess endpoint
- verify the wallet transfer is made on successful guess