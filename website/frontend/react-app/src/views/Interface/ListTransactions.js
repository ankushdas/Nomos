
import React from "react";

import {
   Card,
   CardHeader,
   CardTitle,  
   CardBody,
   CardFooter,
   Table
} from "reactstrap";

import ModalButton from "./ModalButton.js"

function printRow(arg) {
   return (
      <tr>
	 <td>{String(arg.number)}</td>
	 <td className="text-center">
	    <ModalButton
		buttonLabel = "Show"
                modalTitle = {"Transaction #" + String(arg.number)}
		modalBody = <pre>{arg.code}</pre>
	       />
	 </td>
      </tr>
   );
}


class ListTransactions extends React.Component {

   constructor(props) {
      super(props);
   }


   render() { return (
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
		     {this.props.transList.map(printRow)}
		  </tbody>
	       </Table>
	    </div>		  
	 </CardBody>
      </Card>
   )}
}


export default ListTransactions
