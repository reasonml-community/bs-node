module ArrayBuffer = struct
  type t
end

module Buffer = struct
  type t
end

module NodeExports = struct
  type t
end

module NodeModule = struct
  type t
end

module NodeRequire = struct
  type t
end

module Options = struct
  type t
  external options : ?cwd:string -> ?encoding:string -> unit -> t = "" [@@bs.obj]
end

module StringBuffer = struct (* can be either string or buffer *)
  type t
end
