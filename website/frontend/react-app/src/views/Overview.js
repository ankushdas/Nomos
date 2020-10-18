import React from "react";

// reactstrap components
import { Card, CardHeader, CardBody, CardTitle, Row, Col } from "reactstrap";

const overview =
  <Card>
    <CardHeader>
      <h3 className="card-category">Overview</h3>
      <CardTitle tag="h2">
        Nomos - A Programming Language for Digital Contracts
      </CardTitle>
    </CardHeader>
    <CardBody>
      <p>
        <b>Nomos is a domain-specific language based on resource-aware session types for programming digital
        contracts.</b>
      </p>
    </CardBody>
  </Card>

const benefits =
  <Card>
    <CardHeader>
      <CardTitle tag="h3">
        Benefits
      </CardTitle>
    </CardHeader>
    <CardBody>
      <p>
        <b>The Nomos language provides 4 key benefits:</b>
        <ol>
        <li>a session-type based mechanism for expressing the contract protocol</li>
        <li>a resource type system for expressing and automatically inferring the gas bound of a transaction</li>
        <li>a linear type system to handle assets</li>
        <li>a design guarantee of no re-entrancy attacks</li>
        </ol>
      </p>
    </CardBody>
  </Card>

const webinterface =   
  <Card>
    <CardHeader>
      <CardTitle tag="h3">
        Online Interface
      </CardTitle>
    </CardHeader>
    <CardBody>
      <p>
        <b>Try out the web interface for Nomos! You can </b>
          <ol>
          <li>create gas account</li>
          <li>type check and submit transactions</li>
          <li>automatically infer the gas cost of transactions</li>
          </ol> <a href="/interface">Click Here</a>
      </p>
    </CardBody>
  </Card>


const code =   
  <Card>
    <CardHeader>
      <CardTitle tag="h3">
        Source Code
      </CardTitle>
    </CardHeader>
    <CardBody>
      <p>
        <b>Nomos is available open-source with comprehensive documentation and installation instructions. <a href="https://github.com/ankushdas/Nomos" target="_blank" rel="noopener noreferrer">Click Here</a> to
        go the GitHub repository.</b>
      </p>
    </CardBody>
  </Card>
  

class Overview extends React.Component {
  render() {
    return (
      <>
        <div className="content">
          <Row>
            <Col md="12">
	            {overview}
            </Col>
          </Row>
	  <Row>
	    <Col md="5">
	    {benefits}
	    </Col>
	    <Col md="4">
	    {webinterface}
	    </Col>
	    <Col md="3">
	    {code}
	    </Col>
	  </Row>
        </div>
      </>
    );
  }
}

export default Overview;
