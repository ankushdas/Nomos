
const url = "https://5v1khg1b7b.execute-api.us-east-2.amazonaws.com/version1/execute";

const jsonMessage = msg => ({
  method:"post",
  headers: {"Content-Type": "application/json" },
  body:JSON.stringify(msg)
});


async function requestTypeCheck (ocamlState,transactionCode) {
    const body = {state : ocamlState,
		  transaction : transactionCode
		 };
    const msg = jsonMessage ({ request : "typecheck"
			       , body : body
			     });
    const response = await fetch(url,msg);
    const result = await response.json();
    return result
}

async function requestSubmit (ocamlState,transactionCode,account) {
    const body = { state : ocamlState,
		   transaction : transactionCode,
		   account : account
		 };
    const msg = jsonMessage ({ request : "submit"
			       , body : body
			     });
    const response = await fetch(url,msg);
    const result = await response.json();
    return result
}

async function createAccount (ocamlState,account,balance) {
    const body = { state : ocamlState,
		   account : account,
		   balance : balance
		 };
    const msg = jsonMessage ({ request : "create"
			       , body : body
			     });
    const response = await fetch(url,msg);
    const result = await response.json();
    return result
}


const Server = {
    requestTypeCheck : requestTypeCheck,
    requestSubmit : requestSubmit
}

export default Server;
