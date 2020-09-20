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
    authors : "Ankush Das, Stephanie Balzer, Jan Hoffmann, Frank Pfenning and Ishani Santurkar",
    venue : "34th IEEE Computer Security Foundations Symposium (CSF)",
    year : "2021",
    pdf : "https://www.cs.cmu.edu/~ankushd/docs/contract20.pdf"
  },
  
  { title : "Exact and Linear-Time Gas-Cost Analysis",
    authors : "Ankush Das and Shaz Qadeer",
    venue : "27th International Static Analysis Symposium (SAS)",
    year : "2020",
    pdf : "https://www.cs.cmu.edu/~ankushd/docs/move20.pdf"
  },

  { title : "Parallel Complexity Analysis with Temporal Session Types",
    authors : "Ankush Das, Jan Hoffmann and Frank Pfenning",
    venue : "23rd International Conference on Functional Programming (ICFP)",
    year : "2018",
    pdf : "https://www.cs.cmu.edu/~ankushd/docs/icfp18.pdf"
  },

  { title : "Work Analysis with Resource-Aware Session Types",
    authors : "Ankush Das, Jan Hoffmann and Frank Pfenning",
    venue : "33rd Annual ACM/IEEE Symposium on Logic in Computer Science (LICS)",
    year : "2018",
    pdf : "https://www.cs.cmu.edu/~ankushd/docs/work18.pdf"
  },

  { title : "Manifest Sharing with Session Types",
    authors : "Stephanie Balzer and Frank Pfenning",
    venue : "22nd International Conference on Functional Programming (ICFP)",
    year : "2017",
    pdf : "https://www.cs.cmu.edu/~fp/papers/icfp17.pdf"
  },

  { title : "Towards Automatic Resource Bound Analysis for OCaml",
    authors : "Jan Hoffmann, Ankush Das and Shu-Chun Weng",
    venue : "44th ACM SIGPLAN Symposium on Principles of Programming Languages (POPL)",
    year : "2017",
    pdf : "https://www.cs.cmu.edu/~ankushd/docs/popl17.pdf"
  },

  { title : "Polarized Substructural Session Types",
    authors : "Ankush Das and Shaz Qadeer",
    venue : "18th International Conference on Foundations of Software Science and Computation Structures (FoSSaCS).",
    year : "2015",
    pdf : "https://www.cs.cmu.edu/~fp/papers/fossacs15.pdf"
  },

  { title : "Higher-Order Processes, Functions, and Sessions: a Monadic Integration",
    authors : "Bernardo Toninho, Luís Caires, and Frank Pfenning",
    venue : "22nd European Symposium on Programming (ESOP)",
    year : "2013",
    pdf : "https://www.cs.cmu.edu/~fp/papers/esop13a.pdf"
  },

  { title : "Multivariate Amortized Resource Analysis",
    authors : "Jan Hoffmann, Klaus Aehlig, and Martin Hofmann",
    venue : "ACM Transactions on Programming Languages and Systems",
    year : "2012",
    pdf : "https://www.cs.cmu.edu/~janh/assets/pdf/HoffmannAH12.pdf"
  },

  { title : "Session Types as Intuitionistic Linear Propositions",
    authors : "Luís Caires and Frank Pfenning",
    venue : "21st International Conference on Concurrency Theory (CONCUR)",
    year : "2010",
    pdf : "https://www.cs.cmu.edu/~fp/papers/concur10.pdf"
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
     <a href={pub.pdf} target="_blank" rel="noopener noreferrer">
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
                  <CardTitle tag="h2">Publications</CardTitle>
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
