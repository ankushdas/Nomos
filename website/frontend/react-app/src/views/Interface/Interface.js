
import React from "react";

// reactstrap components
import {
  Button,
  Card,
  CardHeader,
  CardTitle,  
  CardBody,
  CardFooter,
  FormGroup,
  Form,
  Input,
  Table,
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
import ModalButton from "./ModalButton.js"
import Transaction from "./Transaction.js"
import ListTransactions from "./ListTransactions.js"


const listAccounts =
  <Card className="card-plain">
    <CardHeader>
      <CardTitle tag="h4">Gas Accounts</CardTitle>
      <p className="category">Existing Accounts in the Current State</p>
    </CardHeader>
    <CardBody>
     <div  style={{height:"35rem",overflowY:"auto"}}>
      <Table className="tablesorter" responsive>
        <thead className="text-primary">
          <tr>
            <th>Account Name</th>
            <th className="text-center">Balance</th>
          </tr>
        </thead>
        <tbody>
          <tr>
            <td>Dakota Rice</td>
            <td className="text-center">$36,738</td>
          </tr>
          <tr>
            <td>Minerva Hooper</td>
            <td className="text-center">$23,789</td>
          </tr>
          <tr>
            <td>Dakota Rice</td>
            <td className="text-center">$36,738</td>
          </tr>
          <tr>
            <td>Minerva Hooper</td>
            <td className="text-center">$23,789</td>
          </tr>
          <tr>
            <td>Dakota Rice</td>
            <td className="text-center">$36,738</td>
          </tr>
          <tr>
            <td>Minerva Hooper</td>
            <td className="text-center">$23,789</td>
          </tr>
          <tr>
            <td>Dakota Rice</td>
            <td className="text-center">$36,738</td>
          </tr>
          <tr>
            <td>Minerva Hooper</td>
            <td className="text-center">$23,789</td>
          </tr>
          <tr>
            <td>Dakota Rice</td>
            <td className="text-center">$36,738</td>
          </tr>
          <tr>
            <td>Minerva Hooper</td>
            <td className="text-center">$23,789</td>
          </tr>
          <tr>
            <td>Dakota Rice</td>
            <td className="text-center">$36,738</td>
          </tr>
          <tr>
            <td>Minerva Hooper</td>
            <td className="text-center">$23,789</td>
          </tr>
          <tr>
            <td>Dakota Rice</td>
            <td className="text-center">$36,738</td>
          </tr>
          <tr>
            <td>Minerva Hooper</td>
            <td className="text-center">$23,789</td>
          </tr>
        </tbody>
      </Table>
    </div>		  
    </CardBody>
  </Card>


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
           <Col className="pl-md-1" md="5">
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





class Interface extends React.Component {

  constructor(props) {
    super(props);

     this.state = {
	loading: false,
	transactionCounter: 1,	
	
	transactionCode: "(*Write or select a transaction.*)",
	ocamlState : "",                /* Ocaml representation of the blockchain state*/
	contractList : [] ,             /* Array of contracts */
	accountList :  [] ,             /* Array of accounts */
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

   
modalButton = () =>
   <ModalButton
   buttonLabel = "Show"
   modalTitle = "title 1 "
   modalBody = <p>ddd</p>
   />





listState = () =>
  <Card className="card-plain">
    <CardHeader>
      <CardTitle tag="h4">Blockchain State</CardTitle>
      <p className="category">The Current Blockchain State</p>
    </CardHeader>
    <CardBody>
     <div  style={{height:"35rem",overflowY:"auto"}}>
      <Table className="tablesorter" responsive>
        <thead className="text-primary">
          <tr>
            <th>Channel Name</th>
            <th className="text-center">Session Type</th>
            <th className="text-center">Process Code</th>
            <th className="text-center">Gas</th>		
          </tr>
        </thead>
        <tbody>
          <tr>
            <td>1</td>
            <td className="text-center">{this.modalButton ()}</td>
            <td className="text-center">{this.modalButton ()}</td>
            <td className="text-center">2000</td>		
          </tr>
          <tr>
            <td>1</td>
            <td className="text-center">{this.modalButton ()}</td>
            <td className="text-center">{this.modalButton ()}</td>
            <td className="text-center">2000</td>		
          </tr>
          <tr>
            <td>1</td>
            <td className="text-center">{this.modalButton ()}</td>
            <td className="text-center">{this.modalButton ()}</td>
            <td className="text-center">2000</td>		
          </tr>
          <tr>
            <td>1</td>
            <td className="text-center">{this.modalButton ()}</td>
            <td className="text-center">{this.modalButton ()}</td>
            <td className="text-center">2000</td>		
          </tr>
          <tr>
            <td>1</td>
            <td className="text-center">{this.modalButton ()}</td>
            <td className="text-center">{this.modalButton ()}</td>
            <td className="text-center">2000</td>		
          </tr>
          <tr>
            <td>1</td>
            <td className="text-center">{this.modalButton ()}</td>
            <td className="text-center">{this.modalButton ()}</td>
            <td className="text-center">2000</td>		
          </tr>
          <tr>
            <td>1</td>
            <td className="text-center">{this.modalButton ()}</td>
            <td className="text-center">{this.modalButton ()}</td>
            <td className="text-center">2000</td>		
          </tr>
          <tr>
            <td>1</td>
            <td className="text-center">{this.modalButton ()}</td>
            <td className="text-center">{this.modalButton ()}</td>
            <td className="text-center">2000</td>		
          </tr>
          <tr>
            <td>1</td>
            <td className="text-center">{this.modalButton ()}</td>
            <td className="text-center">{this.modalButton ()}</td>
            <td className="text-center">2000</td>		
          </tr>
          <tr>
            <td>1</td>
            <td className="text-center">{this.modalButton ()}</td>
            <td className="text-center">{this.modalButton ()}</td>
            <td className="text-center">2000</td>		
          </tr>
        </tbody>
      </Table>
    </div>		  
    </CardBody>
  </Card>
    

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
	      {listAccounts}	    
	    </Col>
	  </Row>
	  
	  <Row>
            <Col md="8">
	    {this.listState ()}
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
