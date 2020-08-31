
# JSON Definitions for the Web Interface

We use JSON (https://www.w3schools.com/js/js_json_syntax.asp) to exchante data between the nomos
binary and the frontend of the web interface.

The encoding should hopefully be UTF-8.


## Share Definitions

STATE = *OCaml's serialized blockchain state*

ERROR = { msg : STRING
        , details : STRING
        }

ACCOUNT_LIST = [ { account : STRING
                 , balance : INT}
	       ]

CONTRACT_LIST = [ { channel : STRING
                   , type : STRING
                   , code : STRING
                   , gas : INT
		   }
		]

## Creating an Account

### Request

{ request : "create"
, body : { state : STATE
         , account : STRING
	 , balance : INT
         }
}

### Responses

{ response : "create"
, status : "success"
, body : { state : STATE
         , acclist : ACCOUNT_LIST
         }
}

{ response : "create"
, status : "error"
, error : ERROR
}



## Typecheck Transaction

### Request

{ request : "typecheck"
, body : { state : STATE
         , transaction : STRING
         }
}

### Responses

{ response : "typecheck"
, status : "success"
, body : { transaction : STRING
         , gasbound : INT
         }
}

{ response : "typecheck"
, status : "error"
, error : ERROR
}


## Submit Transaction

### Request

{ request : "submit"
, body : { state : STATE
         , transaction : STRING
	 , account : STRING	 
         }
}

### Responses

{ response : "submit"
, status : "success"
, body : { state : STATE
         , contlist : CONTRACT_LIST
	 , acclist : ACCOUNT_LIST
         }
}

{ response : "submit"
, status : "error"
, error : ERROR
}


