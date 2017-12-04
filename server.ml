open Lwt
open Cohttp_lwt_unix
open Cohttp
open Yojson.Basic.Util
open Types
let init_enviro =
  PMap.empty
  |> PMap.add "`prime" (E "`prime has not be not bound")
  |> PMap.add "`p" (E "`p has not be not bound")
  |> PMap.add "`q" (E "`q has not be not bound")
  |> PMap.add "`n" (E "`n has not be not bound")
  |> PMap.add "`d" (E "`d has not be not bound")
  |> PMap.add "`e" (E "`e has not be not bound")
  |> PMap.add "`prime_prob" (E "`prime_prob has not be not bound")


type meth = Code.meth
type uri = Uri.t

type request = {
                 headers : Header.t;
                 params : (string * string) list;
                 req_body : string
               }

type response = {
                  headers : Header.t;
                  status : Code.status_code;
                  res_body : string
                }

type callback = request -> response

type custom_callback = request -> (Response.t * Cohttp_lwt.Body.t) Lwt.t

type route = (meth * uri) * callback

type t = route list

let server = ref []

let add_route (meth,uri) callback =
  let server_callback req =
    let res = callback req in
    Server.respond ~headers:res.headers ~status:res.status
                   ~body:(Cohttp_lwt.Body.of_string res.res_body) ()
  in
  server := ((meth,uri),server_callback)::!server

let add_custom_route (meth,uri) custom_callback =
  server := ((meth,uri),custom_callback)::!server

let callback _ req body =
  let meth = Request.meth req in
  let uri = req |> Request.uri in
  try
    let headers = Request.headers req in
    let params =
      List.map (fun query -> ((fst query),List.hd (snd query))) (Uri.query uri)
    in
    body
    |> Cohttp_lwt.Body.to_string
    >>= fun req_body ->
        begin
          {headers;params;req_body}
          |> List.assoc (meth,Uri.path uri) !server
        end
  with Not_found -> Server.respond_string ~status:`Not_found ~body:"" ()


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
    let command =  (j |> member "command" |> to_string) in
    let status = `OK in
    let res_body = "Test:" ^ (fst (Eval.evaluate_line init_enviro command)) in
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

let run ?(port=8000) _ =
  Lwt.async_exception_hook := ignore;
  Server.create ~mode:(`TCP (`Port port)) (Server.make ~callback ())
  |> Lwt_main.run
  |> ignore

let _ =
  add_route (`POST, "/test") test;
  add_route (`GET, "/test/get") test;
  add_route (`POST, "/webhook") webhook;
  add_route (`GET, "/webhook") webhook_verif;
  run ~port:8000 ()



