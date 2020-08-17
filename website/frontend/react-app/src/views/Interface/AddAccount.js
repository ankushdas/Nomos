


import React from "react";

// reactstrap components
import {
   Button,
   Card,
   CardBody,
   FormGroup,
   Input,
   Row,
   Col,
   Form
} from "reactstrap";


class AddAccount extends React.Component {

   constructor(props) {
      super(props);
      this.state = {
	 account : "Frank",
	 balance : 10000
      };
      
      this.handleSubmit = this.handleSubmit.bind(this);
      this.handleAccountChange = this.handleAccountChange.bind(this);
      this.handleBalanceChange = this.handleBalanceChange.bind(this);      
      
   }

   handleAccountChange(event){
      this.setState({account: event.target.value});
   }
   
   handleBalanceChange(event){
      this.setState({balance: parseInt(event.target.value,10)});
   }

   handleSubmit(event){
      const account = this.state.account;
      const balance = this.state.balance;      
      this.props.handleAddAccount(account,balance);
   }

   render() {
      const loading = this.props.loading;
      const account = this.state.account;
      const balance = this.state.balance;      
      return (
	 <Card className="text-center">
	    <CardBody>
	       <Form>
		  <Row>
		     <Col className="pr-md-1" md="7">
			<FormGroup>
			   <label>Account Name</label>
			   <Input
			       value={account}			       
			       type="text"
			       onChange={this.handleAccountChange}			       
                               disabled={loading}	 			       
			   />
			</FormGroup>
		     </Col>
		     <Col className="pl-md-1" md="5">
			<FormGroup>
			   <label>Balance</label>
			   <Input
			       value={balance}
			       onChange={this.handleBalanceChange}			       			       
			       type="text"
                               disabled={loading}	 			     
			   />
			</FormGroup>
		     </Col>
		     <Col md="12">
			<Button
			    block
                            color="primary"
			    onClick={this.handleSubmit}
                            disabled={loading}	 			     			    
			>
			   Create Gas Account
			</Button>
		     </Col>	   
		  </Row>
	       </Form>
	    </CardBody>
	 </Card>
   )}
}


export default AddAccount
