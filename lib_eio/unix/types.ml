type source_ty = [`Unix_fd | Eio.Resource.close_ty | Eio.Flow.source_ty]
type sink_ty   = [`Unix_fd | Eio.Resource.close_ty | Eio.Flow.sink_ty]
type 'a source = ([> source_ty] as 'a) Eio.Resource.t
type 'a sink = ([> sink_ty] as 'a) Eio.Resource.t

type Eio.Exn.Backend.t += Unix_error of Unix.error * string * string

let () =
  Eio.Exn.Backend.register_pp (fun f -> function
      | Unix_error (code, name, arg) -> Fmt.pf f "Unix_error (%s, %S, %S)" (Unix.error_message code) name arg; true
      | _ -> false
    )
