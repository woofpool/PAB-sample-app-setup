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
  curl -s -H "content-type: application/json" \
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
  curl -s -H "content-type: application/json" -XGET localhost:8090/v2/wallets/$WALLET_ID | jq .
  
  # In the response, if you notice the syncing progress is not 100%
  # wait until it reaches 100
  ```
  Sample output
  ```log
  # confirm you see that the wallet has a balance of 1000 test ADA (or 1000000000 LOVELACE)
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
  curl -s -H "content-type: application/json" -s \
      -XGET localhost:8090/v2/wallets/$WALLET_ID
  
  # In the response, if you notice the syncing progress is not 100%
  # wait until it reaches 100
  ```
  Sample output
  ```log
  # confirm you see that the wallet has a balance of 0 test ADA
  {"passphrase":{"last_updated_at":"2021-11-15T21:51:38.461168757Z"},"address_pool_gap":20,"state":{"status":"syncing","progress":{"quantity":32.57,"unit":"percent"}},"balance":{"reward":{"quantity":0,"unit":"lovelace"},"total":{"quantity":0,"unit":"lovelace"},"available":{"quantity":0,"unit":"lovelace"}},"name":"PAB test wallet #2","delegation":{"next":[],"active":{"status":"not_delegating"}},"id":"6b3deba51763744d4ad85451d8ee9c784d416d07","tip":{"height":{"quantity":1187971,"unit":"block"},"time":"2020-04-25T02:33:36Z","epoch_number":55,"absolute_slot_number":1189120,"slot_number":1120},"assets":{"total":[],"available":[]}}
  ```
## Activate the contract for each wallet
- **In terminal 1**, activate the contract for user1 wallet
  ```shell
  curl --location --request POST 'http://localhost:9080/api/contract/activate' \
  --header 'Content-Type: application/json' \
  --data-raw '{ \
  "caID": [], \
  "caWallet": { \
  "getWalletId": "${WALLET_ID}" \
  } \
  }'
  
  # capture the contract ID response
  {
    "unContractInstanceId": "68a928a3-2647-4473-93a1-5c7678577d19"
  }
  ```
- **In Terminal 2**, activate the contract for user2 wallet
  ```shell
  curl --location --request POST 'http://localhost:9080/api/contract/activate' \
  --header 'Content-Type: application/json' \
  --data-raw '{
  "caID": [],
  "caWallet": {
  "getWalletId": "6b3deba51763744d4ad85451d8ee9c784d416d07"
  }
  }'
  ```

## Prepare the request body to invoke the `lock` endpoint
We want to use the `payWallet` endpoint to make a payment from wallet 1 user to wallet 2 user 
- **In Terminal 2**, find an unused wallet address for wallet 2 user. This will be the receiving address of the money
  we send from wallet 1 user
  ```shell
  curl -H "content-type: application/json" \
      -XGET localhost:8090/v2/wallets/$WALLET_ID/addresses | jq '.'
  
  # copy an unused test wallet address (BECH32) to a text file for use later
  # e.g., addr_test1qzeqq43xfrv5np53gfvvpfmv2rkxyjxcp4x87zr6985cq67cnsm6pvc6l4pv0gukjmwpyq7yc226hjatttqg003a7kuqj5aw3v
  ```
- Using `repl`, build a request body to invoke the `payAddress` endpoint
  ```shell
  # change directory to where you've cloned this repo
  cd $HOME/src/PAB-starter-app-setup
  
  # start the repl
  cabal repl
  
  # At the repl prompt, import modules
  > import Plutus.Contracts.Game
  > import Ledger.Ada
  > import Data.Aeson
  > import Data.ByteString.Lazy.Char8 as BSL
  
  # create the args data object 
  > args = LockParams { secretWord = "eagle", amount = lovelaceValueOf 2000000 } 

  # serialize the args object into JSON string
  > BSL.putStrLn $ encode args
  
  # sample response
  > {"amount":{"getValue":[[{"unCurrencySymbol":""},[[{"unTokenName":""},2000000]]]]},"secretWord":"eagle"}
  ```

## Invoke the endpoint as wallet 1 user  
- **In Terminal 1**, look at the status of the contract for wallet 1 user
  ```shell
  curl --location --request GET 'http://localhost:9080/api/contract/instance/68a928a3-2647-4473-93a1-5c7678577d19/status'
  
  # verify that lock is available in the hooks section  
  ```
- POST request to `lock` endpoint
  The amount of ADA needs to be more than the minimum amount defined by network protocol parameters. In the example code below, 
  we use 2 ADA or 2000000 lovelaces
  ```shell
  curl --location --request POST 'http://localhost:9080/api/contract/instance/68a928a3-2647-4473-93a1-5c7678577d19/endpoint/lock' \
  --header 'Content-Type: application/json' \
  --data-raw '{"amount":{"getValue":[[{"unCurrencySymbol":""},[[{"unTokenName":""},2000000]]]]},"secretWord":"eagle"}'
  
  # check the logs in the terminal where the PAB server is running to ensure no errors
  # here is a sample output for a successful invocation
  [pab:Info:7560] [2021-12-01 13:58:54.98 UTC] ae437806-2b0d-4995-8346-506f31906952: "Pay Value (Map [(,Map [(\"\",2000000)])]) to the script"
  [pab:Info:7560] [2021-12-01 13:58:55.12 UTC] ae437806-2b0d-4995-8346-506f31906952: "Waiting for guess or lock endpoint..."
  ```
- Query the wallet information to make sure the balance has decreased by 2 ADA
  ```shell
  # ensure WALLET_ID env variable is set
  curl -H "content-type: application/json" -XGET localhost:8090/v2/wallets/$WALLET_ID
  
  # verify balance info
  ```

## Make a successful guess as wallet 2 user
- POST good-guess request to `guess` endpoint
  ```shell
  # ensure the contract_id is correct for wallet 2 user
  curl --location --request POST 'http://localhost:9080/api/contract/instance/58f06e66-06f2-4770-b2c4-a93b5cb3844b/endpoint/guess' \
  --header 'Content-Type: application/json' \
  --data-raw '{"guessWord": "eagle"}'
  
  # check the logs in the terminal where the PAB server is running to ensure no errors
  # here is a sample output for a successful invocation
  [pab:Info:7560] [2021-12-01 13:58:54.98 UTC] ae437806-2b0d-4995-8346-506f31906952: "Pay Value (Map [(,Map [(\"\",2000000)])]) to the script"
  [pab:Info:7560] [2021-12-01 13:58:55.12 UTC] ae437806-2b0d-4995-8346-506f31906952: "Waiting for guess or lock endpoint..."
  ```


## Verify the funds have been transferred to wallet 2 user