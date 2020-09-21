import storageT1 from "assets/nomos/storage-contract-t1.nom"
import storageT2 from "assets/nomos/get-storage-t2.nom"
import storageT3 from "assets/nomos/set-storage-t3.nom"
import storageT4 from "assets/nomos/get-storage-t4.nom"

import walletT1 from "assets/nomos/create-wallet-t1.nom"
import walletT2 from "assets/nomos/create-wallet-t2.nom"
import walletT3 from "assets/nomos/transfer-t3.nom"
import walletT4 from "assets/nomos/print-balance-t4.nom"

import auctionT1 from "assets/nomos/auction-contract-t1.nom"
import auctionT2 from "assets/nomos/auction-bid-t2.nom"
import auctionT3 from "assets/nomos/auction-bid-t3.nom"
import auctionT4 from "assets/nomos/auction-bid-t4.nom"
import auctionT5 from "assets/nomos/auction-bid-t5.nom"

const nomosFiles = [
  { name: "Storage: Txn 1", file: storageT1},
  { name: "Storage: Txn 2", file: storageT2},
  { name: "Storage: Txn 3", file: storageT3},
  { name: "Storage: Txn 3", file: storageT4},
  { name: "Wallet: Txn 1", file: walletT1},
  { name: "Wallet: Txn 2", file: walletT2},
  { name: "Wallet: Txn 3", file: walletT3},
  { name: "Wallet: Txn 4", file: walletT4},
  { name: "Auction: Txn 1", file: auctionT1},
  { name: "Auction: Txn 2", file: auctionT2},
  { name: "Auction: Txn 3", file: auctionT3},
  { name: "Auction: Txn 4", file: auctionT4},
  { name: "Auction: Txn 5", file: auctionT5}
]


export default nomosFiles
