// reactstrap components
import { Card, CardHeader, CardBody, CardTitle, Row, Col } from "reactstrap";

import React, { Component } from 'react'
import ReactMarkdown from 'react-markdown'
import docPath from './doc.md'

class Documentation extends React.Component {
  constructor(props) {
    super(props)

    this.state = { terms: null }
  }

  componentWillMount() {
    fetch(docPath).then((response) => response.text()).then((text) => {
      this.setState({ terms: text })
    })
  }

  render() {

    return (
      <>
        <div className="content">
          <Row>
            <Col md="12">
              <Card>
                <CardHeader>
                  <CardTitle tag="h2">
                    Nomos Documentation
                  </CardTitle>
                </CardHeader>
                <CardBody>
                  <p>
                    <div className="content">
                      <ReactMarkdown source={this.state.terms} />
                    </div>
                  </p>
                </CardBody>
              </Card>
            </Col>
          </Row>
        </div>
      </>
    );
  }
}

export default Documentation;
