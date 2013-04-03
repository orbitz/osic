module Monad_infix = struct
  let (>>=) d f =
    match d with
      | Some v ->
	f v
      | None ->
	None
end
