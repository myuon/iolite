fun f() {
  let text = "lib.io\n";
  extcall_write(1, text.ptr as rawptr, text.length);

  return nil;
}
