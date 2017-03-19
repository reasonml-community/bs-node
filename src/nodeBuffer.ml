(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

open Js.Typed_array

type t

external fromString : Js.String.t -> t = "Buffer.from" [@@bs.val]
external fromStringWithEncoding : Js.String.t -> encoding:Js.String.t -> t =
  "Buffer.from" [@@bs.val]
external fromArray : int array -> t = "Buffer.from" [@@bs.val]
external fromArrayBuffer : ArrayBuffer.t -> t = "Buffer.from" [@@bs.val]
external fromArrayBufferOffset: ArrayBuffer.t -> offset:int -> t =
  "Buffer.from" [@@bs.val]
external fromArrayBufferRange: ArrayBuffer.t -> offset:int ->
  length:int -> t = "Buffer.from" [@@bs.val]
external fromBuffer: t -> t = "Buffer.from" [@@bs.val]
external alloc: int -> t = "Buffer.alloc" [@@bs.val]
external allocFillInt: int -> fill:int -> t = "Buffer.alloc" [@@bs.val]
external allocFillString: int -> fill:Js.String.t -> t = "Buffer.alloc" [@@bs.val]
external allocFillStringWithEncoding: int -> fill:Js.String.t ->
  encoding:Js.String.t -> t = "Buffer.alloc" [@@bs.val]
external allocFillBuffer: int -> fill:t -> t = "Buffer.alloc" [@@bs.val]
external allocUnsafe: int -> t = "Buffer.allocUnsafe" [@@bs.val]
external allocUnsafeSlow: int -> t = "Buffer.allocUnsafeSlow" [@@bs.val]
external toString: t -> Js.String.t = "" [@@bs.send]
external unsafeGet : t -> int -> int = "" [@@bs.get_index]
external unsafeSet : t -> int -> int -> unit = "" [@@bs.set_index]
external byteLengthString : Js.String.t -> int = "Buffer.byteLength" [@@bs.val]
external byteLengthStringWithEncoding : Js.String.t -> encoding:Js.String.t -> int =
  "Buffer.byteLength" [@@bs.val]
external byteLengthBuffer : t -> int = "Buffer.byteLength" [@@bs.val]
external byteLengthInt8Array : int Int8Array.typed_array -> int =
  "Buffer.byteLength" [@@bs.val]
external byteLengthUint8Array : int Uint8Array.typed_array -> int =
  "Buffer.byteLength" [@@bs.val]
external byteLengthInt16Array : int Int16Array.typed_array -> int =
  "Buffer.byteLength" [@@bs.val]
external byteLengthUint16Array : int Uint16Array.typed_array -> int =
  "Buffer.byteLength" [@@bs.val]
external byteLengthInt32Array : int Int32Array.typed_array -> int =
  "Buffer.byteLength" [@@bs.val]
external byteLengthUint32Array : int Uint32Array.typed_array -> int =
  "Buffer.byteLength" [@@bs.val]
external byteLengthFloat32Array : float Float32Array.typed_array -> int =
  "Buffer.byteLength" [@@bs.val]
external byteLengthFloat64Array : float Float64Array.typed_array -> int =
  "Buffer.byteLength" [@@bs.val]
external byteLengthDataView : DataView.t -> int = "Buffer.byteLength" [@@bs.val]
external byteLengthArrayBuffer : ArrayBuffer.t -> int = "Buffer.byteLength" [@@bs.val]
external compare : t -> t -> int = "Buffer.compare" [@@bs.val]
