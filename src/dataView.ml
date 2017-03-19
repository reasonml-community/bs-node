(* FIXME remove after https://github.com/bloomberg/bucklescript/pull/1401 merge *)
(** The DataView view provides a low-level interface for reading and writing
    multiple number types in an ArrayBuffer irrespective of the platform's endianness.

  @see <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DataView> MDN
*)

type t
type array_buffer = Js.Typed_array.ArrayBuffer.t

external make : array_buffer -> t = "DataView" [@@bs.new]
external fromBuffer : array_buffer -> t = "DataView" [@@bs.new]
external fromBufferOffset : array_buffer -> int -> t = "DataView" [@@bs.new]
external fromBufferRange : array_buffer -> offset:int -> length:int -> t = "DataView" [@@bs.new]

external buffer : t -> array_buffer = "" [@@bs.get]
external byteLength : t -> int = "" [@@bs.get]
external byteOffset : t -> int = "" [@@bs.get]

external getInt8 : t -> int -> int = "" [@@bs.send]
external getUint8 : t -> int -> int = "" [@@bs.send]

external getInt16: t -> int -> int = "" [@@bs.send]
external getInt16LittleEndian : t -> int -> (_ [@bs.as 1]) -> int = "" [@@bs.send]

external getUint16: t -> int -> int = "" [@@bs.send]
external getUint16LittleEndian : t -> int -> (_ [@bs.as 1]) -> int = "" [@@bs.send]

external getInt32: t -> int -> int = "" [@@bs.send]
external getInt32LittleEndian : t -> int -> (_ [@bs.as 1]) -> int = "" [@@bs.send]

external getUint32: t -> int -> int = "" [@@bs.send]
external getUint32LittleEndian : t -> int -> (_ [@bs.as 1]) -> int = "" [@@bs.send]

external getFloat32: t -> int -> float = "" [@@bs.send]
external getFloat32LittleEndian : t -> int -> (_ [@bs.as 1]) -> float = "" [@@bs.send]

external getFloat64: t -> int -> float = "" [@@bs.send]
external getFloat64LittleEndian : t -> int -> (_ [@bs.as 1]) -> float = "" [@@bs.send]

external setInt8 : t -> int -> int -> unit = "" [@@bs.send]
external setUint8 : t -> int -> int -> unit = "" [@@bs.send]

external setInt16: t -> int -> int -> unit = "" [@@bs.send]
external setInt16LittleEndian : t -> int -> int -> (_ [@bs.as 1]) -> unit = "" [@@bs.send]

external setUint16: t -> int -> int -> unit = "" [@@bs.send]
external setUint16LittleEndian : t -> int -> int -> (_ [@bs.as 1]) -> unit = "" [@@bs.send]

external setInt32: t -> int -> int -> unit = "" [@@bs.send]
external setInt32LittleEndian : t -> int -> int -> (_ [@bs.as 1]) -> unit = "" [@@bs.send]

external setUint32: t -> int -> int -> unit = "" [@@bs.send]
external setUint32LittleEndian : t -> int -> int -> (_ [@bs.as 1]) -> unit = "" [@@bs.send]

external setFloat32: t -> int -> float -> unit = "" [@@bs.send]
external setFloat32LittleEndian : t -> int -> float -> (_ [@bs.as 1]) -> unit = "" [@@bs.send]

external setFloat64: t -> int -> float -> unit = "" [@@bs.send]
external setFloat64LittleEndian : t -> int -> float -> (_ [@bs.as 1]) -> unit = "" [@@bs.send]
