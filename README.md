# Setup Guide to run Plutus application using PAB services

This setup guide shows how to deploy a simple, "smart contract" application bundled with Plutus Application Backend (PAB) services.

Hopefully, this documentation provides a lot of value for others. The contributors of this project welcome your feedback-- both good and bad!

### Why is this useful?
- reasons go here

### Key Details
- The smart-contract application code is taken from the IOHK Plutus Starter repository. It is a simple guessing game
  with `lock` and `guess` endpoints.
- Client wallets interact with the application by invoking HTTP endpoints via the PAB Web Server.
- The PAB services connect to a Cardano Testnet for on-chain transaction activity

### Medium article


## Usage Instructions

1. **Install executables**

    * Install the following executables: `cardano-node`, `cardano-wallet`, `plutus-chain-index`
    * Please refer to the [Install executables guide](1-INSTALL_EXECUTABLES.md) for instructions.
    
2. **Build sample app and PAB package dependencies** 
    
    * Build the PAB services bundled with the sample contract into one executable. 
    * Please refer to the [Install posgreSQL](2-INSTALL_POSTGRESQL.md) for instructions to set up.

3. **Run scripts to set up & run private Cardano network and connect DB Sync process**

    * Run scripts to bootstrap the Cardano private network and attach the `cardano-db-sync` process to it to sync blockchain data to SQL database.
    * Please refer to the [Run network scripts guide](3-RUN_NETWORK_SCRIPTS.md) for instructions. 

4. **Attach DB Sync process the network**

    * Attach the `cardano-db-sync` process to the network, which syncs blockchain data to a `PostgreSQL` database.
    * Please refer to the [Attach db-sync guide](4-ATTACH_DB_SYNC.md) for instructions.

5. **Run simple transaction and query the db-sync database to see results**

    * Set up a new walllet for user2 and make a payment from user1 to user2. Query the database to confirm the transaction.
    * Please refer to the [Run transaction guide](5-RUN_TRANSACTION.md) for instructions.

## Contributors

This project is provided free of charge to the Cardano community. The author of this project is a fan of Cardano, as well as a Cardano stake pool operator.
I am not affiliated with IOHK in any official capacity.  

If you want to support the continued development of this project, you can delegate or recommend my staking pool:

- [**WOOF Cardano Staking Pool**](https://woofpool.github.io/)

## Contributing

If you'd like to help maintain this project, please feel free to submit a pull request. Please ensure that any script changes have been tested and verified.

## License

This project is licensed under the terms of the [MIT License](LICENSE).