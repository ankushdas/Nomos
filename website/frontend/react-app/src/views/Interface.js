
import React from "react";

// reactstrap components
import {
  Button,
  Modal,
  ModalBody,
  ModalFooter,
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

const msgTypeCheckStarted = {
    place: "br",
    message: "Server contacted. Type Checking ...",
    type: "info",
    icon: "tim-icons icon-bell-55",
    autoDismiss: 10
}


const msgTypeCheckResponse = msg => ({
    place: "br",
    message: msg,
    type: "success",
    icon: "tim-icons icon-bell-55"
})


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
        modalVisible: false,
	loading: false,
        transactionCode: "(*Write or select a transaction.*)"
    };
    
    this.toggleModal = this.toggleModal.bind(this);
    this.handleCheckTransaction = this.handleCheckTransaction.bind(this);
    this.handleTransactionTextChange = this.handleTransactionTextChange.bind(this);    
}

toggleModal(){
    this.setState({
        modalVisible: !this.state.modalVisible
    });
}

async handleCheckTransaction() {
  this.setState({loading:true});
  this.notify(msgTypeCheckStarted);
  const url = "https://5v1khg1b7b.execute-api.us-east-2.amazonaws.com/version1/execute";
  const msg = {
    method:"post",
    headers: {"Content-Type": "application/json" },
    body:JSON.stringify({code:this.state.transactionCode})
    };
  const response = await fetch(url,msg);
  console.log(response);
  // see https://developer.mozilla.org/en-US/docs/Web/API/Response
  // I hope a json string from ocaml will allow us to use .json() instead of .text() below
  const data = await response.text();
  this.setState({loading:false});
  this.notify(msgTypeCheckResponse(data));
}

handleTransactionTextChange(event) {
   this.setState({transactionCode: event.target.value});
}

transactionForm = () =>
  <Card>
    <CardHeader>
      <h6 className="title">Transaction</h6>
    </CardHeader>
    <CardBody>
        <Row>
          <Col md="12">
          <FormGroup>
             <Input
	      style={{fontFamily:"Monaco,Consolas,Lucida Console,Liberation Mono,DejaVu Sans Mono,Bitstream Vera Sans Mono,Courier New, monospace"}}
              type="textarea"
	      resize="both"
              onChange={this.handleTransactionTextChange}
	      value={this.state.transactionCode}
              rows="30"
              />
          </FormGroup>			  
	 </Col>
        </Row>
    </CardBody>
                <CardFooter>
	<Row>
          <Col md="5">
	  </Col>
          <Col md="4">
                        <FormGroup>
                          <label>Account</label>
                          <Input
                            defaultValue="Frank"
                            placeholder=""
                            type="text"
			    disabled
                          />
                        </FormGroup>
	  </Col>
          <Col md="3">
                        <FormGroup>
                          <label>Gas Bound</label>
                          <Input
                            defaultValue=""
                            placeholder=""
                            type="text"
			    disabled
                          />
                        </FormGroup>
	  </Col>
        </Row>
	<Row>
          <Col md="5">
           <Button
	     className="btn-fill"
	     color="primary"
             onClick={this.handleCheckTransaction}
	   >
             Type Check / Elaborate
           </Button>
	  </Col>
          <Col md="4">
           <Button className="btn-fill" color="primary" type="submit" disabled={this.state.loading}>
             Cancel
           </Button>
	  </Col>
          <Col md="3">
           <Button className="btn-fill" color="primary" type="submit">
             Submit
           </Button>
	  </Col>
        </Row>	
     </CardFooter>
  </Card>


modalButton = () =>
<div>
<Button size="sm" color="secondary" onClick={this.toggleModal}>
    Show
</Button>
<Modal isOpen={this.state.modalVisible} toggle={this.toggleModal} size="lg">
    <div className="modal-header">
      <h5 className="modal-title" id="exampleModalLabel">
        Modal title
      </h5>
      <button
        type="button"
        className="close"
        data-dismiss="modal"
        aria-hidden="true"
        onClick={this.toggleModal}
      >
        <i className="tim-icons icon-simple-remove" />
      </button>
    </div>
	<ModalBody style={{maxHeight: "70rem", overflowY: "auto"}}>
    <p>transaction code</p>
    </ModalBody>
    <ModalFooter>
        <Button color="secondary" onClick={this.toggleModal}>
            Close
        </Button>
        <Button
	  color="primary"
          disabled={this.state.loading}
	 >
            Save changes
        </Button>
    </ModalFooter>
</Modal>
</div>

notify = arg => {
  const options = {
    place: arg.place,
    message: (<div>{arg.message}</div>),
    type: arg.type,
    icon: "tim-icons icon-bell-55",
    autoDismiss: arg.autoDismiss
  };
  this.refs.notificationAlert.notificationAlert(options);
};



listTransactions = () =>
  <Card className="card-plain">
    <CardHeader>
      <CardTitle tag="h4">Past Transactions</CardTitle>
      <p className="category">The "Blockchain"</p>
    </CardHeader>
    <CardBody>
     <div  style={{height:"35rem",overflowY:"auto"}}>
      <Table className="tablesorter" responsive>
        <thead className="text-primary">
          <tr>
            <th>Transaction #</th>
            <th className="text-center">Source Code</th>
          </tr>
        </thead>
        <tbody>
          <tr>
            <td>1</td>
            <td className="text-center">{this.modalButton ()}</td>
          </tr>
        </tbody>
      </Table>
    </div>		  
    </CardBody>
  </Card>


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
               {this.transactionForm ()}
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
	      {this.listTransactions ()}	    
	    </Col>
          </Row>
        </div>
    );
  }
}

export default Interface;
