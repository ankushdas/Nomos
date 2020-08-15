
const url = "https://5v1khg1b7b.execute-api.us-east-2.amazonaws.com/version1/execute";

const jsonMessage = msg => ({
  method:"post",
  headers: {"Content-Type": "application/json" },
  body:JSON.stringify(msg)
});


async function requestTypeCheck (ocamlState,transactionCode) {
    const body = {state : ocamlState
		  , transaction : transactionCode
		 };
    const msg = jsonMessage ({ request : "typecheck"
			       , body : body
			     });
    const response = await fetch(url,msg);
    const result = await response.json();
    return result
}

async function requestSubmit (transactionCode) {
    const msg = jsonMessage ({ code:transactionCode });
    // const response = await fetch(url,msg);
    // const result = await response.text();
    return "Server.js: Not implemented yet"
}

const Server = {
    requestTypeCheck : requestTypeCheck,
    requestSubmit : requestSubmit
}

export default Server;
