
import React from "react";

import {
   Card,
   CardHeader,
   CardTitle,  
   CardBody,
   Table
} from "reactstrap";

import ModalButton from "./ModalButton.js"

const printRow = arg =>
   (
      <tr>
	 <td>{arg.channel}</td>
	 <td className="text-center">
	    <ModalButton
		buttonLabel = "Show"
                modalTitle = {"Type of Channel " + arg.channel}
		modalBody = <pre>{arg.type}</pre>
	       />
	 </td>
	 <td className="text-center">
	    <ModalButton
		buttonLabel = "Show"
                modalTitle = {"Process State @ " + arg.channel}
		modalBody = <pre>{arg.code}</pre>
	       />
	 </td>
	 <td className="text-center">
	    {String(arg.gas)}
	 </td>
      </tr>
   )

const ListContracts = props =>
   (
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
			<th className="text-center">Process State</th>
			<th className="text-center">(Work,Gas)</th>		
		     </tr>
		  </thead>
		  <tbody>
		     {props.contList.map(printRow)}
		  </tbody>
	       </Table>
	    </div>		  
	 </CardBody>
      </Card>
   )

export default ListContracts
