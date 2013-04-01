open Core.Std
open Async.Std

type t = { file   : string
	 ; pipe_w : string Pipe.Writer.t
	 ; r      : Reader.t
	 }

let rec loop t =
  Reader.read_line t.r >>= function
    | `Ok line -> begin
      Pipe.write_without_pushback t.pipe_w line;
      loop t
    end
    | `Eof -> begin
      Reader.close t.r >>= fun () ->
      start_reading t.file t.pipe_w
    end
and start_reading file pipe_w =
  Reader.open_file file >>= fun r ->
  loop { file; pipe_w; r }

let create file =
  let pipe_r, pipe_w = Pipe.create () in
  ignore (start_reading file pipe_w);
  pipe_r
