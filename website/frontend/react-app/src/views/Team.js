import React from "react";

// reactstrap components
import {
  Card,
  CardBody,
  CardFooter,
  CardText,
  Row,
  Col
} from "reactstrap";

const ankush = {
  name : "Ankush Das",
  inst : "Carnegie Mellon University",
  link : "https://www.cs.cmu.edu/~ankushd/",
  img  : require("assets/img/ankush.jpg"),
  desc : "Lead Developer of Nomos and PhD Student at Carnegie Mellon"
}

const jan = {
  name : "Jan Hoffmann",
  inst : "Carnegie Mellon University",
  link : "https://www.cs.cmu.edu/~janh/",
  img  : require("assets/img/jan.jpg"),
  desc : "Nomos Developer and Associate Professor of Computer Science at Carnegie Mellon"
}

const frank = {
  name : "Frank Pfenning",
  inst : "Carnegie Mellon University",
  link : "https://www.cs.cmu.edu/~fp/",
  img  : require("assets/img/frank.jpg"),
  desc : "Nomos Developer and Professor of Computer Science at Carnegie Mellon"
}

const ishani = {
  name : "Ishani Santurkar",
  inst : "Carnegie Mellon University",
  img  : require("assets/img/ishani.jpg"),
  desc : "Nomos Developer and Bachelor student majoring in Computer Science at Carnegie Mellon"
}

const stephen = {
  name : "Stephen McIntosh",
  inst : "Carnegie Mellon University",
  img  : require("assets/img/stephen.jpg"),
  desc : "Nomos Developer and Bachelor student majoring in Computer Science at Carnegie Mellon"
}

const stephanie = {
  name : "Stephanie Balzer",
  inst : "Carnegie Mellon University",
  link : "https://www.cs.cmu.edu/~balzers/",
  img  : require("assets/img/sb.jpg"),
  desc : "Nomos contributor and Research Faculty at Carnegie Mellon"
}

const teamMember = arg => (
  <Card className="card-user">
    <CardBody>
      <CardText />
      <div className="author">
        <div className="block block-one" />
        <div className="block block-two" />
        <div className="block block-three" />
        <div className="block block-four" />
        <a href={arg.link} target="_blank" rel="noopener noreferrer">		    
          <img
            alt="..."
            className="avatar"
            src={arg.img}
          />
          <h5 className="title">{arg.name}</h5>
        </a>
        <p className="description">{arg.inst}</p>
      </div>
      <div className="card-description">
       {arg.desc}
      </div>
    </CardBody>
    <CardFooter>
      <div className="button-container">
         {arg.link ?
	   <a href={arg.link} target="_blank" rel="noopener noreferrer">website</a>
	 : null
	 } 
      </div>
    </CardFooter>
  </Card>
)


class Team extends React.Component {
  render() {
    return (
      <>
        <div className="content">
          <Row>
            <Col md="4">
 	      {teamMember (ankush)}
            </Col>
            <Col md="4">
 	      {teamMember (jan)}
            </Col>
            <Col md="4">
              {teamMember (frank)}	      
            </Col>
          </Row>
          <Row>
            <Col md="4">
              {teamMember (ishani)}
            </Col>
            <Col md="4">
              {teamMember (stephen)}	    
            </Col>
            <Col md="4">
              {teamMember (stephanie)}	    
            </Col>
          </Row>
        </div>
      </>
    );
  }
}

export default Team;
