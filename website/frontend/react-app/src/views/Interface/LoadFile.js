
import React from "react";

import {
   Card,
   CardBody,
   UncontrolledDropdown,
   DropdownToggle,
   DropdownMenu,
   DropdownItem,
   Form
} from "reactstrap";


import nomosFiles from "./NomosFiles.js"






function LoadFile(props) {
   const updateText = props.updateText;

   const loadFile = file => (
      fetch(file)
	 .then(response => response.text())
	 .then(text => (updateText(text)))
   );
   

   const fileItem = arg =>
      (
	 <DropdownItem
	     onClick={() => loadFile(arg.file)}
	 >
	    {arg.name}
	 </DropdownItem>
      );

   
   return(
      <Card className="text-center">
	 <CardBody>
	    <Form>
		     <UncontrolledDropdown data-toggle="dropdown">
			<DropdownToggle caret color="primary" data-toggle="dropdown">
			   Load Transaction
			</DropdownToggle>
			<DropdownMenu>
			   {nomosFiles.map(fileItem)}
			</DropdownMenu>
		     </UncontrolledDropdown>
	    </Form>
	 </CardBody>
      </Card>  
   );
}


export default LoadFile
