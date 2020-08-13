
import React from "react";

import {
   Button,
   Modal,
   ModalBody,
} from "reactstrap";


class ModalButton extends React.Component {

   constructor(props) {
      super(props);
      this.state = {      
	 modalVisible: false
      };
      this.toggleModal = this.toggleModal.bind(this);
   }

   toggleModal(){
      this.setState({
         modalVisible: !this.state.modalVisible
      })
   }  

   render() { return (
      <div>
	 <Button size="sm" color="secondary" onClick={this.toggleModal}>
	    {this.props.buttonLabel}
	 </Button>
	 <Modal isOpen={this.state.modalVisible} toggle={this.toggleModal} size="lg">
	    <div className="modal-header">
	       <h4 className="modal-title">
		  {this.props.modalTitle}
	       </h4>
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
	        {this.props.modalBody}
	    </ModalBody>
	    {/* <ModalFooter>
	    <Button color="secondary" onClick={this.toggleModal}>
	    Close
	    </Button>
	    </ModalFooter> */}
	 </Modal>
      </div>
   )}
}


export default ModalButton
