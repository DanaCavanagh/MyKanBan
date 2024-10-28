open Opium

let hello_handler _req =
  Response.of_plain_text "Hello, World!" 
    |> Lwt.return
let () = 
  App.empty
    |> App.get "/" hello_handler
    |> App.run_command
