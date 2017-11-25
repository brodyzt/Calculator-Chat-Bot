# ohttp

ohttp is a lightweight wrapper of [cohttp](https://github.com/mirage/ocaml-cohttp) for HTTP servers in OCaml

**\* Please note that this should only be used for a lack of familiarity with monads**

## Goals
- provide a simple interface for adding routes with callbacks to consume requests and produce responses
- abstract the monadic semantics of [cohttp](https://github.com/mirage/ocaml-cohttp)
- abstract the asynchrony of [cohttp](https://github.com/mirage/ocaml-cohttp)

## Usage

### Repository Sample Code

##### Compiling

    make

##### Running

    ./server.byte

### Server Definition

See the `server.ml` file for a concrete example of the following outline:

#### Starting the Server

```ocaml
let _ =
  HttpServer.run ~port:8000 ()
```

#### Adding Routes

```ocaml
open Cohttp
open HttpServer

let test_get req =
  (* HTTP response headers *)
  let headers = Header.init_with "Content-Type" "application/json" in
  (* HTTP response code *)  
  let status = `OK in
  (* HTTP response body *)
  let res_body = "{\"test\": 1}" in
  {headers; status; res_body}

let test_post req =
  let res_body = "Request Body: " ^ req.req_body in
  {
    headers = req.headers;
    status = `OK;
    res_body
  }

let _ =
  HttpServer.add_route (`GET, "/test/get") test_get;
  HttpServer.add_route (`POST, "/test/post") test_post;
  HttpServer.run ~port:8000 ()
```

#### Extensibility

For further customization, leverage the `HttpServer.add_custom_route` function which opens up the [cohttp](https://github.com/mirage/ocaml-cohttp) response structure for more complex use cases such as Server-Sent Events (SSE).

## Design Decisions
- `req_body` and `res_body` field names were chosen as opposed to just simply `body` due to the weak type inference of records and the potential confusion it causes in the OCaml compiler when no other record fields are used within an expression to distinguish between the `request` and `response` types

## Client Options
- leverage a client in another programming language
- leverage the `XHRClient` in [here](https://github.com/RamV13/scrabble/tree/master/public/js) (for browser requests)
- adapt the `XHRClient` above to perform requests via bytecode or native code

## Acknowledgements

Adapted from [Scrabble](https://github.com/RamV13/scrabble) (2017 CS 3110 Final Project)
