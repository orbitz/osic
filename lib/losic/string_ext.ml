let safe f =
  try
    Some (f ())
  with
    | _ ->
      None

let index s ch =
  safe (fun () -> String.index s ch)

let lsplit2 ~on s =
  match index s on with
    | Some idx ->
      let l = String.sub s 0 idx in
      let r = String.sub s (idx + 1) (String.length s - idx - 1) in
      Some (l, r)
    | None ->
      None

let rec split_on_char ~on s =
  match lsplit2 ~on s with
    | Some (s1, s2) ->
      s1::(split_on_char ~on s2)
    | None ->
      [s]

let splice s e str n =
  String.sub str 0 s ^
    n ^
    String.sub str (e + 1) (String.length str - e - 1)
