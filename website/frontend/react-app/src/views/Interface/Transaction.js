


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
   }

   handleTextChange(event){
      this.props.handleTextChange(event.target.value);
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
		      onClick={this.props.handleCheckTransaction}
		      disabled={loading || typeChecked}		      
		  >
		     Type Check / Elaborate
		  </Button>
	       </Col>
               <Col md="4">
		  <Button
		      className="btn-fill"
		      color="primary"
		      disabled={loading || !typeChecked}		      
		  >
		     Submit
		  </Button>
	       </Col>
               <Col md="3">
		  <Button
		      className="btn-fill"
		      color="primary"
		      onClick={this.props.cancelTransaction}		      
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
