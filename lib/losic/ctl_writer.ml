let write ctl c =
  let fout = open_out_gen [Open_append] 0o666 ctl in
  output_string fout (Ctl_builder.build c);
  close_out fout
