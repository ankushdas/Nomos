
import React from "react";

// reactstrap components
import {
  Row,
  Col,
} from "reactstrap";

// react plugin for creating notifications over the dashboard
import NotificationAlert from "react-notification-alert";

import Messages from "./Messages.js"
import Server from "./Server.js"

import Transaction from "./Transaction.js"
import AddAccount from "./AddAccount.js"
import LoadFile from "./LoadFile.js"

import ListTransactions from "./ListTransactions.js"
import ListAccounts from "./ListAccounts.js"
import ListContracts from "./ListContracts.js"


class Interface extends React.Component {

  constructor(props) {
    super(props);

     this.state = {
	loading: false,
	transactionCounter: 1,	
	transactionCode: "(* Write or select a transaction. *)",
        savedTransactionCode: "",
	/* Ocaml sexp representation of the blockchain state*/
	ocamlState : "(0 0 () () ((conf ()) (conts ()) (shared ()) (types ())))",
	contractList : [] ,             /* Array of contracts */
	accountList :  [] ,             /* Array of accounts */
	transactionList :  [] ,         /* Array of past transactions */	
     };
    
     this.handleCheckTransaction = this.handleCheckTransaction.bind(this);
     this.handleSubmitTransaction = this.handleSubmitTransaction.bind(this);
     this.handleCancelSubmit = this.handleCancelSubmit.bind(this);     
     this.handleAddAccount = this.handleAddAccount.bind(this);     
     this.setTransactionText = this.setTransactionText.bind(this);
  }
    
  setTransactionText(text) {
     this.setState({transactionCode: text});
  }

  notify(arg) {
    const options = {
      place: arg.place,
      message: (<div>{arg.message}</div>),
      type: arg.type,
      icon: "tim-icons icon-bell-55",
      autoDismiss: arg.autoDismiss
    };
    this.refs.notificationAlert.notificationAlert(options);
  }
    
  async handleCheckTransaction() {
     const transCode = this.state.transactionCode;
     const ocamlState = this.state.ocamlState;
     
     this.setState({loading:true});
     this.notify(Messages.serverContacted("Type checking transaction"));
     
     const response = await Server.requestTypeCheck(ocamlState,transCode);

     if (response.status === "success") {
	const newState = {
	   loading:false,
	   transactionCode: response.body.transcode,
           savedTransactionCode: transCode
	}
	const gasBound = response.body.gasbound;
	const ocamlTxn = response.body.transaction;	
	this.setState(newState);
	this.notify(Messages.success(
	   "Type checking and elaboration successful.\n \nReady to submit with gas bound " + String(gasBound) + ".")
	);
	return {bound: gasBound, transaction: ocamlTxn};
     }
     else {
	const errorHeading = "Elaboration unsuccessful.";
	const errorMsg = response.error.msg;
	this.setState({loading:false});
	this.notify(Messages.error(errorHeading,errorMsg));
	return {bound: -1, transaction: null};
     }
  }

  handleCancelSubmit() {
     const savedTransCode = this.state.savedTransactionCode;
     this.setState ({transactionCode: savedTransCode});
  }

  async handleSubmitTransaction(account,ocamlTxn) {
     const transCounter = this.state.transactionCounter;
     const transList = this.state.transactionList;
     const transCode = this.state.transactionCode;
     const ocamlState = this.state.ocamlState;     
     const savedTransCode = this.state.savedTransactionCode;

     this.setState({loading:true});
     this.notify(Messages.serverContacted("Submitting transaction"));

     const response = await Server.requestSubmit(ocamlState,ocamlTxn,account);

     if (response.status === "success") {
	const usedGas = response.body.gascost;
	const newState = {
	   transactionCounter: transCounter+1,
	   transactionList: [{number:transCounter, messages:response.body.execmsgs, code:transCode, account:account, gasCost:usedGas},...transList],
	   loading: false,
	   ocamlState: response.body.state,
	   contractList: response.body.contlist,
	   accountList: response.body.acclist,
           transactionCode: savedTransCode
	};

	this.setState(newState);
	this.notify(Messages.success("Transaction #" + transCounter + " posted."));
     }
     else {
	const errorHeading = "Submissin failure.";
	const errorMsg = response.error.msg;
	this.setState({loading:false, transactionCode:savedTransCode});
	this.notify(Messages.error(errorHeading,errorMsg));
     }
  }

   async handleAddAccount(account,balance) {
      const ocamlState = this.state.ocamlState;

      this.setState({loading:true});
      this.notify(Messages.serverContacted("Creating gas account ..."));

      const response = await Server.createAccount(ocamlState,account,balance);

      if (response.status === "success") {
	 const newState = {
	    loading: false,
	    ocamlState: response.body.state,
	    accountList: response.body.acclist
	 };

	 this.setState(newState);
	 this.notify(Messages.success("Account " + account + " created."));
      }
      else {
	 const errorHeading = "Failed to create account.";
	 const errorMsg =  response.error.msg;
	 this.setState({loading:false});
	 this.notify(Messages.error(errorHeading,errorMsg));
      }
   }
   

   render() {
      return (
         <div className="content">
            <div className="react-notification-alert-container">
             <NotificationAlert ref="notificationAlert" />
            </div>

            <Row>
              <Col md="8">
 		<Transaction
 		   transactionCode = {this.state.transactionCode}
 		   handleTextChange = {this.setTransactionText}
 		   handleCheckTransaction = {this.handleCheckTransaction}
 		   handleSubmitTransaction = {this.handleSubmitTransaction}
                   handleCancel = {this.handleCancelSubmit}
  		   loading = {this.state.loading}
 	       />
             </Col>
 	    <Col md="4">
 	       <LoadFile
		   updateText = {this.setTransactionText}
	       />
    	       <AddAccount
 		   handleAddAccount = {this.handleAddAccount}		    
  		   loading = {this.state.loading}
	       />
 	       <ListAccounts
 		   accList = {this.state.accountList}
 	       />	    
 	    </Col>
 	  </Row>
 	  
 	  <Row>
             <Col md="8">
 	       <ListContracts
 		   contList = {this.state.contractList}
 	       />
             </Col>
 	    <Col md="4">
 	       <ListTransactions
 		   transList = {this.state.transactionList}
 	       />
 	    </Col>
           </Row>
         </div>
     );
   }
}

export default Interface;
