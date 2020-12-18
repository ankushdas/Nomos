
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
	 <td>{String(arg.number)}</td>
	 <td className="text-center">
	    <ModalButton
		buttonLabel = "Show"
                modalTitle = {"Transaction #" + String(arg.number)}
 	        modalBody = <div>
		              <p>
	      		        <b>Account of Sender: </b>
  			        {arg.account}
			      </p>
		              <p>
	      		        <b>Gas Bound: </b>
  			        {String(arg.gasCost)}
						</p>
						<p>
						  <b>Execution Trace Messages: </b>
						</p>
						  <pre>{arg.messages}</pre>
			      <p>
 			        <b>Transaction Code:</b>
			      </p>
	                      <pre>{arg.code}</pre>
                           </div>
	       />
	 </td>
      </tr>
   )


const ListTransactions = props =>
   (
      <Card className="card-plain">
	 <CardHeader>
	    <CardTitle tag="h4">Transaction History</CardTitle>
	    <p className="category">The "Blockchain"</p>
	 </CardHeader>
	 <CardBody>
	    <div  style={{height:"35rem",overflowY:"auto"}}>
	       <Table className="tablesorter" responsive>
		  <thead className="text-primary">
		     <tr>
			<th>Transaction #</th>
			<th className="text-center">Execution Details</th>
		     </tr>
		  </thead>
		  <tbody>
		     {props.transList.map(printRow)}
		  </tbody>
	       </Table>
	    </div>		  
	 </CardBody>
      </Card>
   )


export default ListTransactions
