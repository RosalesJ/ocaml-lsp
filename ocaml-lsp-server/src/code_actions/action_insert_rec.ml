open Import

let action_kind = "insert_rec"

let log = Logger.log ~section:"ocamllsp" ~title:Logger.Title.Info

let code_action_of_intf errors =
  List.iter errors ~f:(fun (err : Loc.error) ->
      let loc = Loc.loc_of_report err in
      let range = Range.of_loc loc in
      let severity =
        match err.source with
        | Warning -> "warning"
        | Lexer -> "lexer"
        | Parser -> "parser"
        | Typer -> "typer"
        | Unknown -> "unknown"
        | Env -> "env"
        | Config -> "config"
      in
      let rangeJson = Range.yojson_of_t range in
      log "%s: %s" severity (Yojson.Safe.to_string rangeJson)
    );
  let title = String.capitalize_ascii "Add rec keword" in
  CodeAction.create ~title ~kind:CodeActionKind.QuickFix ~isPreferred:false ()

let code_action doc (params : CodeActionParams.t) =
  let command =
    let _start = Position.logical params.range.start in
    Query_protocol.(Errors {typing = true; lexing = false; parsing = false})
  in
  let open Fiber.O in
  let+ errors = Document.dispatch doc command in
  match errors with
  | Ok errors -> Ok (Some (code_action_of_intf errors))
  | Error _e ->
    Logger.log ~section:"ocamllsp" ~title:Logger.Title.Info "got a failure";
    Error (Jsonrpc.Response.Error.of_exn _e)
