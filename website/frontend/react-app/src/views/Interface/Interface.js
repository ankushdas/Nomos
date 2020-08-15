
import React from "react";

// reactstrap components
import {
  Button,
  Card,
  CardBody,
  FormGroup,
  Form,
  Input,
  Row,
  Col,
  UncontrolledDropdown,
  DropdownToggle,
  DropdownMenu,
  DropdownItem,
} from "reactstrap";

// react plugin for creating notifications over the dashboard
import NotificationAlert from "react-notification-alert";

import Messages from "./Messages.js"
import Server from "./Server.js"
import Transaction from "./Transaction.js"
import ListTransactions from "./ListTransactions.js"
import ListAccounts from "./ListAccounts.js"
import ListContracts from "./ListContracts.js"


const addAccount =
  <Card className="text-center">
    <CardBody>
      <Form>
	  <Row>
           <Col className="pr-md-1" md="7">
            <FormGroup>
              <label>Account Name</label>
              <Input
                defaultValue="Frank"
                placeholder="Username"
                type="text"
                />
            </FormGroup>
           </Col>
n           <Col className="pl-md-1" md="5">
           <FormGroup>
             <label htmlFor="exampleInputEmail1">
               Balance
             </label>
             <Input
	       placeholder="amount" type="text" 
               defaultValue="10000"
	       />
           </FormGroup>
           </Col>
           <Col md="12">
           <Button className="btn-fill" color="primary" type="submit">
             Create Gas Account
           </Button>
           </Col>	   
         </Row>
      </Form>
    </CardBody>
  </Card>

const loadTransaction =
  <Card className="text-center">
    <CardBody>
      <Form>
	  <Row>
           <Col className="pr-md-1" md="12">
	   <UncontrolledDropdown group>
	     <DropdownToggle caret color="primary" data-toggle="dropdown">
               Load Transaction
	     </DropdownToggle>
	     <DropdownMenu>
               <DropdownItem>File 1</DropdownItem>
               <DropdownItem>File 2</DropdownItem>
               <DropdownItem>File 3</DropdownItem>
	     </DropdownMenu>
	   </UncontrolledDropdown>
           </Col>	   
         </Row>
      </Form>
    </CardBody>
  </Card>  


const testAccList = [{account : "Jan", balance : 1000000}, {account : "Ankush", balance : 10}]
const testContList = [{channel : "C1", type : "T", code: "sss", gas : 1000000},
		      {channel : "C2", type : "TT", code: "ddd", gas : 22}]


class Interface extends React.Component {

  constructor(props) {
    super(props);

     this.state = {
	loading: false,
	transactionCounter: 1,	
	
	transactionCode: "(*Write or select a transaction.*)",
	ocamlState : "",                /* Ocaml representation of the blockchain state*/
	contractList : testContList ,             /* Array of contracts */
	accountList :  testAccList ,             /* Array of accounts */
	transactionList :  [] ,         /* Array of past transactions */	
     };
    
     this.handleCheckTransaction = this.handleCheckTransaction.bind(this);
     this.handleSubmitTransaction = this.handleSubmitTransaction.bind(this);     
     this.handleTransactionTextChange = this.handleTransactionTextChange.bind(this);
  }
    
  handleTransactionTextChange(text) {
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
    this.setState({loading:true});
    this.notify(Messages.serverContacted("Type checking transaction"));
    const response = await Server.requestTypeCheck(this.state.transactionCode);
    this.setState({loading:false, transactionTypeChecked: true});
    this.notify(Messages.typeCheckResponse(response));
    return true 
  }

   async handleSubmitTransaction(account,gasBound) {
      const transCounter = this.state.transactionCounter;
      const transCode = this.state.transactionCode;
      const transList = this.state.transactionList;

      this.setState({loading:true});
      this.notify(Messages.serverContacted("Submitting transaction"));

      const response = await Server.requestSubmit(transCode);

      this.setState(
	 {transactionCounter : transCounter+1,
	  transactionList : [{number:transCounter, code:transCode},...transList]
	 }
      );

      this.setState({loading:false});
      this.notify(Messages.typeCheckResponse(response));

      return true
   }

  render() {
    return (
        <div className="content">
          <div className="react-notification-alert-container">
            <NotificationAlert ref="notificationAlert" />
          </div>
	  
          <Row>
             <Col md="8">
		{/* the handle checkTransaction function should return a boolean that indicates if the transaction was succesful
		typeChecked can be updated based on the result  */}
		<Transaction
		   transactionCode = {this.state.transactionCode}
		   handleTextChange = {this.handleTransactionTextChange}
		   handleCheckTransaction = {this.handleCheckTransaction}
		   handleSubmitTransaction = {this.handleSubmitTransaction}		    
 		   loading = {this.state.loading}
	       />
            </Col>
	    <Col md="4">
	      {loadTransaction}	    
   	      {addAccount}
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
