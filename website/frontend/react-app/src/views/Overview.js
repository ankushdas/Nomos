import React from "react";

// reactstrap components
import { Card, CardHeader, CardBody, CardTitle, Row, Col } from "reactstrap";

const overview =
  <Card>
    <CardHeader>
      <h5 className="card-category">Overview</h5>
      <CardTitle tag="h2">
        Nomos - A Programming Language for Digital Contracts
      </CardTitle>
    </CardHeader>
    <CardBody>
      <p>
        What is Nomos?
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
        List some of the benefits.
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
        you can try out Nomos online
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
        Nomos is open source software. Link to github.
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
	    <Col md="4">
	    {benefits}
	    </Col>
	    <Col md="4">
	    {webinterface}
	    </Col>
	    <Col md="4">
	    {code}
	    </Col>
	  </Row>
        </div>
      </>
    );
  }
}

export default Overview;
