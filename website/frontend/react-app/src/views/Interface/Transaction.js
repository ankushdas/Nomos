


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


const monospaceFonts = "Monaco,Consolas,Lucida Console,Liberation Mono,Bitstream Vera Sans Mono,Courier New, monospace"



class Transaction extends React.Component {

   constructor(props) {
      super(props);
      this.state = {
	 typeChecked : false,
	 account : "Frank",
	 gasBound : ""
      };
      
      this.handleTypeCheck = this.handleTypeCheck.bind(this);
      this.handleSubmit = this.handleSubmit.bind(this);
      this.handleCancel = this.handleCancel.bind(this);            
   }

   handleTypeCheck(event){
      const gasBound = this.props.handleCheckTransaction();
      if (gasBound >= 0) {
	 this.setState({typeChecked:true, gasBound : String(gasBound)});
      };
   }

   handleSubmit(event){
      const account = this.state.account;
      this.props.handleSubmitTransaction(account);
      this.setState({
	 typeChecked:false,
	 gasBound:""
      });
   }

   handleCancel(event){
      this.setState({typeChecked:false, gasBound : ""});
   }
   
   render() {
      const transactionCode = this.props.transactionCode;
      const loading = this.props.loading;
      const typeChecked = this.state.typeChecked;
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
