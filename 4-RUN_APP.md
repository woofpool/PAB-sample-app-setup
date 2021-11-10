# Run the app

- verify contract definitions can be fetched
- stage the wallets
    - In term 1, create user1 wallet
        - run post commands against cardano-wallet server
        - load some address with tADA
        - export WALLET_ID
        -

    - In term 2, create user2 wallet
        - run post commands against cardano-wallet server
        - load some address with tADA
        - export WALLET_ID
- interact with contract
    - In terminal 1
        - activate the contract for user1 wallet
        - invoke lock endpoint to lock value with a secret word
    - In terminal 2
        - activate the contract for user2 wallet
        - invoke guess endpoint
- verify the wallet transfer is made on successful guess