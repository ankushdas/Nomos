


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

const monospaceFonts = "Monaco,Consolas,Lucida Console,Liberation Mono,Bitstream Vera Sans Mono,Courier New, monospace"


class Transaction extends React.Component {

   constructor(props) {
      super(props);
      this.state = {
	 typeChecked : false,
	 account : "Frank",
	 gasBound : -1
      };
      
      this.handleTextChange = this.handleTextChange.bind(this);
      this.handleTypeCheck = this.handleTypeCheck.bind(this);
      this.handleSubmit = this.handleSubmit.bind(this);
      this.handleCancel = this.handleCancel.bind(this);            
   }

   handleTextChange(event){
      this.props.handleTextChange(event.target.value);
   }

   handleTypeCheck(event){
      const checked = this.props.handleCheckTransaction();
      if (checked) {
	 this.setState({typeChecked:true});
      };
   }

   handleSubmit(event){
      const account = this.state.account;
      const gasBound = this.state.gasBound;      
      this.props.handleSubmitTransaction(account,gasBound);
      this.setState({typeChecked:false});
   }

   handleCancel(event){
      this.setState({typeChecked:false});
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
	               <Input
                         className="lined" 
	 		 style={{fontFamily:monospaceFonts}}
			 type="textarea"
			 resize="both"
			 onChange={this.handleTextChange}
			 value={transactionCode}
	                 rows="30"
                         disabled={loading || typeChecked}	 
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
                         defaultValue={this.state.gasBound}
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
		      onClick={this.handleTypeCheck}
		      disabled={loading || typeChecked}		      
		  >
		     Type Check / Elaborate
		  </Button>
	       </Col>
               <Col md="4">
		  <Button
		      className="btn-fill"
		      color="primary"
		      onClick={this.handleSubmit}		      
		      disabled={loading || !typeChecked}		      
		  >
		     Submit
		  </Button>
	       </Col>
               <Col md="3">
		  <Button
		      className="btn-fill"
		      color="primary"
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
