open Import

let action_kind = "insert_rec"

let log = Logger.log ~section:"ocamllsp" ~title:Logger.Title.Info

let code_action_of_intf _res =
  let title = String.capitalize_ascii "Add rec keword" in
  CodeAction.create ~title ~kind:CodeActionKind.QuickFix
    ~isPreferred:false ()

let code_action doc (params : CodeActionParams.t) =
  let command =
    let start = Position.logical params.range.start in
    Query_protocol.Locate_type start
  in
  let open Fiber.O in
  let+ res = Document.dispatch doc command in
  match res with
  | Ok _res -> log "Got a good result";
    Ok (Some (code_action_of_intf _res))
  | Error _e -> log "Got a failure";
    Error (Jsonrpc.Response.Error.of_exn _e)
