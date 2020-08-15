
import React from "react";

import {
   Card,
   CardHeader,
   CardTitle,  
   CardBody,
   Table
} from "reactstrap";



const printRow = arg =>
   (
      <tr>
	 <td>{arg.account}</td>
	 <td className="text-center">
	    {String(arg.balance)}
	 </td>
      </tr>
   )


const ListAccounts = props =>
   (
      <Card className="card-plain">
	 <CardHeader>
	    <CardTitle tag="h4">Gas Accounts</CardTitle>
	    <p className="category">Existing Accounts in the Current State</p>
	 </CardHeader>
	 <CardBody>
	    <div  style={{height:"35rem",overflowY:"auto"}}>
	       <Table className="tablesorter" responsive>
		  <thead className="text-primary">
		     <tr>
			<th>Account Name</th>
			<th className="text-center">Balance</th>
		     </tr>
		  </thead>
		  <tbody>
		     {props.accList.map(printRow)}		     
		  </tbody>
	       </Table>
	    </div>		  
	 </CardBody>
      </Card>
   )


export default ListAccounts
