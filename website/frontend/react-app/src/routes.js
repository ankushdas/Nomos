/*!

=========================================================
* Black Dashboard React v1.1.0
=========================================================

* Product Page: https://www.creative-tim.com/product/black-dashboard-react
* Copyright 2020 Creative Tim (https://www.creative-tim.com)
* Licensed under MIT (https://github.com/creativetimofficial/black-dashboard-react/blob/master/LICENSE.md)

* Coded by Creative Tim

=========================================================

* The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

*/
import Team from "views/Team.js";
import Publications from "views/Publications.js";
import Documentation from "views/Documentation.js";
import Overview from "views/Overview.js";
import Interface from "views/Interface.js";

var routes = [
  {
    path: "/overview",
    name: "Overview",
    icon: "tim-icons icon-compass-05",
    component: Overview,
    layout: "/admin"
  },
  {
    path: "/doc",
    name: "Documentation",
    icon: "tim-icons icon-map-big",
    component: Documentation,
    layout: "/admin"
  },
  {
    path: "/interface",
    name: "Web Interface",
    icon: "tim-icons icon-app",
    component: Interface,
    layout: "/admin"
  },
  {
    path: "/publications",
    name: "Publications",
    icon: "tim-icons icon-paper",
    component: Publications,
    layout: "/admin"
  },
  {
    path: "/team",
    name: "Team",
    icon: "tim-icons icon-badge",
    component: Team,
    layout: "/admin"
  }
];
export default routes;
