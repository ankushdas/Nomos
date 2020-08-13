import React from "react";

// reactstrap components
import {
  Button,
  Card,
  CardHeader,
  CardBody,
  CardTitle,
  Table,
  Row,
  UncontrolledTooltip
} from "reactstrap";


const publications =
[
  { title : "Resource-Aware Session Types for Digital Contracts",
    authors : "Ankush Das, Stephanie Balzer, Jan Hoffmann, Frank Pfenning, and Ishani Santurkar",
    venue : "IEEE Computer Security Foundations Symposium (CSF’21)",
    year : "2021",
    pdf : "https://www.cs.cmu.edu/~janh/assets/pdf/DasBHP19.pdf"
  },
  
  { title : "Please Add more",
    authors : "also the RaML popl 11 paper; frank's papers about session types that we rely on",
    venue : "etc",
    year : "etc",
    pdf : ""
  }
]

const printPub = pub =>
   <tr>
     <td>
       <i className="tim-icons icon-paper" />			  
     </td>
     <td>
       <p className="title">{pub.title}</p>
       <p className="text-muted">
         <em>{pub.authors}.</em> {pub.venue}. {pub.year}
       </p>
     </td>
     <td className="td-actions text-right">
      <a href={pub.pdf}>
      <Button
        color="link"
        id="tooltip636901683"
        title=""
        type="button"
        >
        <i className="tim-icons icon-cloud-download-93" />
      </Button>
      </a>
      <UncontrolledTooltip
        delay={0}
        target="tooltip636901683"
        placement="right"
       >
        Download PDF
      </UncontrolledTooltip>
     </td>
   </tr>
 

class Publications extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      bigChartData: "data1"
    };
  }
  setBgChartData = name => {
    this.setState({
      bigChartData: name
    });
  };
  render() {
    return (
      <>
        <div className="content">
          <Row>
              <Card className="card-tasks">
                <CardHeader>
                  <CardTitle tag="h2">Peer-Reviewed Publications</CardTitle>
                </CardHeader>
                <CardBody>
                  <div className="table-full-width table-responsive">
                    <Table>
                      <tbody>
                        {publications.map(printPub)}
                      </tbody>
                    </Table>
                  </div>
                </CardBody>
              </Card>
          </Row>
        </div>
      </>
    );
  }
}

export default Publications;
