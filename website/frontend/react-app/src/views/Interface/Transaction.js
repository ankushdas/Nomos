


import React from "react";

// reactstrap components
import {
   Button,
   Card,
   CardHeader,
   CardBody,
   CardFooter,
   FormGroup,
   Input,
   Row,
   Col
} from "reactstrap";

import {Controlled as CodeMirror} from 'react-codemirror2'
import 'codemirror/theme/night.css';


/* const monospaceFonts = "Monaco,Consolas,Lucida Console,Liberation Mono,Bitstream Vera Sans Mono,Courier New, monospace"*/


class Transaction extends React.Component {

   constructor(props) {
      super(props);
      this.state = {
	 typedTransaction : null,
	 account : "Frank",
	 gasBound : ""
      };
      
      this.handleTypeCheck = this.handleTypeCheck.bind(this);
      this.handleSubmit = this.handleSubmit.bind(this);
      this.handleCancel = this.handleCancel.bind(this);            
   }

   async handleTypeCheck(event){
      const res = await this.props.handleCheckTransaction();
      if (res.transaction != null) {
	 this.setState({typedTransaction:res.transaction, gasBound : String(res.gasBound)});
      };
   }

   handleSubmit(event){
      const account = this.state.account;
      const txn = this.state.typedTransaction;
      this.props.handleSubmitTransaction(account,txn);
      this.setState({
	 typedTransaction: null,
	 gasBound: ""
      });
   }

   handleCancel(event){
      this.setState({typedTransaction:null, gasBound : ""});
   }
   
   render() {
      const transactionCode = this.props.transactionCode;
      const loading = this.props.loading;
      const typeChecked = this.state.typedTransaction == null;
      return (
      <Card>
	 <CardHeader>
	    <h6 className="title">Transaction</h6>
	 </CardHeader>
	 <CardBody>
            <Row>
               <Col md="12">
		  <FormGroup>
                   <CodeMirror
                       value={transactionCode}
                       options={{
			  lineNumbers: true,
			  spellcheck: false,
			  autocorrect: false,
			  readOnly: ((loading || typeChecked) ? "nocursor" : false ) 
		       }}
	               onBeforeChange={(editor, data, value) => {
			     this.props.handleTextChange(value);			     
			  }}
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
			 disabled={loading || !typeChecked}
                     />
                  </FormGroup>
	       </Col>
               <Col md="3">
                  <FormGroup>
                     <label>Gas Bound</label>
                     <Input
                         value={this.state.gasBound}
                         type="text"
			 disabled
                     />
                  </FormGroup>
	       </Col>
            </Row>
	    <Row>
               <Col md="5">
		  <Button
		      color="primary"
                      block		      
		      onClick={this.handleTypeCheck}
		      disabled={loading || typeChecked}		      
		  >
		     Check / Elaborate
		  </Button>
	       </Col>
               <Col md="4">
		  <Button
		      color="primary"
                      block		      
		      onClick={this.handleSubmit}		      
		      disabled={loading || !typeChecked}		      
		  >
		     Submit
		  </Button>
	       </Col>
               <Col md="3">
		  <Button
		      color="primary"
                      block		      
		      onClick={this.handleCancel}		      
		      disabled={loading || !typeChecked}
		  >
		     Cancel
		  </Button>
	       </Col>
            </Row>	
	 </CardFooter>
      </Card>
   )}
}


export default Transaction
