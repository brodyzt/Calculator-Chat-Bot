open HttpServer
open Cohttp
open Yojson.Basic.Util

(* [test req] responds with a "Hello <name>!" given the request [req] containing
 * a plain string representing <name> *)
let test req =
  let headers = Header.init_with "Content-Type" "text/plain" in
  let status = `OK in
  let res_body = "Hello " ^ req.req_body ^ "!" in
  {headers; status; res_body}

let webhook req =
  let headers = Header.init_with "Content-Type" "application/json" in
  let j = Yojson.Basic.from_string req.req_body in
    if (j |> member "object" |> to_string) = "page" then
      let status = `OK in
      let res_body = "EVENT_RECEIVED" in
        {headers; status; res_body}
    else
      let status = `Not_found in
      let res_body = "sad times" in
        {headers; status; res_body}

let webhook_verif req =
  let headers = Header.init_with "Content-Type" "text/plain" in
  let verify_token = "erybcnasdgkadsda" in
    if List.mem_assoc "hub.mode" req.params
    && List.mem_assoc "hub.verify_token" req.params
    && List.mem_assoc "hub.challenge" req.params
    then
      let mode = List.assoc "hub.mode" req.params in
      let token = List.assoc "hub.verify_token" req.params in
      let challenge = List.assoc "hub.challenge" req.params in

      if mode = "subscribe" && token = verify_token then
        (print_string "WEBHOOK VERIFIED!!!!!!";
        let status = `OK in
        let res_body = challenge in
          {headers; status; res_body})
      else
        let status = `Unauthorized in
        let res_body = "sad times" in
          {headers; status; res_body}
    else
      let status = `Unauthorized in
      let res_body = "sad times" in
        {headers; status; res_body}


let _ =
  HttpServer.add_route (`POST, "/test") test;
  HttpServer.add_route (`GET, "/test/get") test;
  HttpServer.add_route (`POST, "/webhook") webhook;
  HttpServer.add_route (`GET, "/webhook") webhook_verif;
  HttpServer.run ~port:8000 ()
