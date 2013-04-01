open Core.Std

let encode msg = msg ^ "\r\n"

let pass p = encode ("PASS " ^ p)

let nick n = encode ("NICK " ^ n)

let user ~u ~n = encode ("USER " ^ u ^ " 8 * :" ^ n)

let join ~p ~c =
  let p =
    match p with
      | Some v -> " " ^ v
      | None   -> ""
  in
  encode ("JOIN " ^ c ^ p)

let part c = encode ("PART " ^ c)

let msg ~d ~m = encode ("PRIVMSG " ^ d ^ " :" ^ m)

let pong servers = encode ("PONG " ^ (String.concat ~sep:" " servers))

let quit msg = encode ("QUIT :" ^ msg)
