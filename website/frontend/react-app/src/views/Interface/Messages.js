

const Messages = {

    typeCheckStarted :
	{ place: "br",
          message: "Server contacted. Type Checking ...",
	  type: "info",
	  icon: "tim-icons icon-bell-55",
	  autoDismiss: 10
	},

    typeCheckResponse : msg => ({
	place: "br",
	message: msg,
	type: "success",
	icon: "tim-icons icon-bell-55"
    })

}

export default Messages;
