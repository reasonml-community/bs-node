type t (* can be either string or buffer *)

type _ stringBufferKind =
  | String :  Js.String.t stringBufferKind
  | Buffer :  NodeBuffer.t stringBufferKind

(** We except a good inliner will eliminate such boxing in the future *)
let test (type a) (x : t) : (a stringBufferKind * a)=
  if Js.typeof x = "string" then
    (Obj.magic String : a stringBufferKind), (Obj.magic x : a)
  else
    (Obj.magic Buffer : a stringBufferKind), (Obj.magic x : a)
