
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
	      		        <em>Account of Sender: </em>
  			        {arg.account}
			      </p>
			      <p>
 			        <em>Transaction Code:</em>
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
		     {props.transList.map(printRow)}
		  </tbody>
	       </Table>
	    </div>		  
	 </CardBody>
      </Card>
   )


export default ListTransactions
