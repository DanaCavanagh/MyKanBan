open Opium

type task = {
  id : int;
  title : string;
  description : string;
  status : string;
}

let tasks = ref []

let get_tasks _req =
  let json = `List (List.map (fun task ->
    `Assoc [
      ("id", `Int task.id);
      ("title", `String task.title);
      ("description", `String task.description);
      ("status", `String task.status)
    ]) !tasks) in
  Response.of_json json |> Lwt.return

let create_task req =
  let%lwt body = Request.to_json_exn req in
  let id = List.length !tasks + 1 in
  let title = body |> Yojson.Safe.Util.member "title" |> Yojson.Safe.Util.to_string in
  let description = body |> Yojson.Safe.Util.member "description" |> Yojson.Safe.Util.to_string in
  let status = body |> Yojson.Safe.Util.member "status" |> Yojson.Safe.Util.to_string in
  let task = { id; title; description; status } in
  tasks := task :: !tasks;
  Response.of_json (`Assoc [("id", `Int id)]) |> Lwt.return

let update_task req =
  let id = Router.param req "id" |> int_of_string in
  let%lwt body = Request.to_json_exn req in
  let title = body |> Yojson.Safe.Util.member "title" |> Yojson.Safe.Util.to_string in
  let description = body |> Yojson.Safe.Util.member "description" |> Yojson.Safe.Util.to_string in
  let status = body |> Yojson.Safe.Util.member "status" |> Yojson.Safe.Util.to_string in
  tasks := List.map (fun task ->
    if task.id = id then { task with title; description; status }
    else task) !tasks;
  Response.of_plain_text "Task updated" |> Lwt.return

let delete_task req =
  let id = Router.param req "id" |> int_of_string in
  tasks := List.filter (fun task -> task.id <> id) !tasks;
  Response.of_plain_text "Task deleted" |> Lwt.return

let serve_html_file filename _req =
  let filepath = Filename.concat (Sys.getcwd ()) ("bin/html/" ^ filename) in
  Lwt.catch
    (fun () ->
      let%lwt content = Lwt_io.(with_file ~mode:input filepath read) in
      let headers = Httpaf.Headers.of_list [("Content-Type", "text/html")] in
      Response.make ~headers ~body:(Body.of_string content) () |> Lwt.return)
    (fun exn ->
      let error_message = Printexc.to_string exn in
      Response.of_plain_text ("Internal server error: " ^ error_message) |> Lwt.return)

let () =
  let app = App.empty
    |> App.get "/tasks" get_tasks
    |> App.post "/tasks" create_task
    |> App.put "/tasks/:id" update_task
    |> App.delete "/tasks/:id" delete_task
    |> App.get "/create_task" (serve_html_file "create_task.html")
    |> App.get "/update_task" (serve_html_file "update_task.html")
    |> App.get "/delete_task" (serve_html_file "delete_task.html")
  in
  App.run_command app