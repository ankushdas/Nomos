import React from "react";

const Messages = {

   serverContacted : msg => (
      { place: "br",
        message: "Server contacted. " + msg + " ..." ,
	type: "info",
	icon: "tim-icons icon-bell-55",
	autoDismiss: 10
      }),

   success : msg => ({
      place: "br",
      message: msg,
      type: "success",
      icon: "tim-icons icon-bell-55",
      autoDismiss: 10      
   }),

   error : (heading,msg) => ({
      place: "br",
      message: <div>{heading}<br></br><em>{msg}</em></div>,
      type: "danger",
      icon: "tim-icons icon-bell-55"
   })

}

export default Messages;
