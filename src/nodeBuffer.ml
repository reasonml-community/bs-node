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
external byteLengthInt8Array : Int8Array.t -> int =
  "Buffer.byteLength" [@@bs.val]
external byteLengthUint8Array : Uint8Array.t -> int =
  "Buffer.byteLength" [@@bs.val]
external byteLengthInt16Array : Int16Array.t -> int =
  "Buffer.byteLength" [@@bs.val]
external byteLengthUint16Array : Uint16Array.t -> int =
  "Buffer.byteLength" [@@bs.val]
external byteLengthInt32Array : Int32Array.t -> int =
  "Buffer.byteLength" [@@bs.val]
external byteLengthUint32Array : Uint32Array.t -> int =
  "Buffer.byteLength" [@@bs.val]
external byteLengthFloat32Array : Float32Array.t -> int =
  "Buffer.byteLength" [@@bs.val]
external byteLengthFloat64Array : Float64Array.t -> int =
  "Buffer.byteLength" [@@bs.val]
external byteLengthDataView : DataView.t -> int = "Buffer.byteLength" [@@bs.val]
external byteLengthArrayBuffer : ArrayBuffer.t -> int = "Buffer.byteLength" [@@bs.val]
external compare : t -> t -> int = "Buffer.compare" [@@bs.val]
external concat : t array -> t = "Buffer.concat" [@@bs.val]
external concatLength : t array -> length:int -> t = "Buffer.concat" [@@bs.val]
external isEncoding : Js.String.t -> bool = "Buffer.isEncoding" [@@bs.val]
type buffer
external buffer : buffer = "Buffer" [@@bs.val]
external poolSize : int = "Buffer.poolSize" [@@bs.val]
external setPoolSize : buffer -> int -> int = "poolSize" [@@bs.set]
let setPoolSize n = setPoolSize buffer n
external copy : t -> t -> int = "" [@@bs.send]
external copyOffset : t -> t -> targetStart:int -> int = "copy" [@@bs.send]
external copyOffsetSource : t -> t -> targetStart:int -> sourceStart:int ->
  int = "copy" [@@bs.send]
external copyRangeSource : t -> t -> targetStart:int -> sourceStart:int ->
  sourceEnd:int -> int = "copy" [@@bs.send]
external copyToUint8Array : t -> Uint8Array.t -> int = "copy" [@@bs.send]
external copyToUint8ArrayOffset : t -> Uint8Array.t -> targetStart:int ->
  int = "copy" [@@bs.send]
external copyToUint8ArrayOffsetSource : t -> Uint8Array.t -> targetStart:int ->
  sourceStart:int -> int = "copy" [@@bs.send]
external copyToUint8ArrayRangeSource : t -> Uint8Array.t -> targetStart:int ->
  sourceStart:int -> sourceEnd:int -> int = "copy" [@@bs.send]
(* FIXME after iterators support *)
(* external entries : t -> Iterator = "entries" [@@bs.get] *)
external equals : t -> t -> bool = "" [@@bs.send]
external fillString : t -> Js.String.t -> t = "fill" [@@bs.send]
external fillStringOffset : t -> value:Js.String.t -> offset:int -> t =
  "fill" [@@bs.send]
external fillStringRange : t -> value:Js.String.t -> offset:int ->
  offsetEnd:int -> t = "fill" [@@bs.send]
external fillStringRangeWithEncoding : t -> value:Js.String.t -> offset:int ->
  offsetEnd:int -> encoding:Js.String.t -> t = "fill" [@@bs.send]
external fillBuffer : t -> t -> t = "fill" [@@bs.send]
external fillBufferOffset : t -> value:t -> offset:int -> t = "fill" [@@bs.send]
external fillBufferRange : t -> value:t -> offset:int -> offsetEnd:int -> t =
  "fill" [@@bs.send]
external fillInt : t -> int -> t = "fill" [@@bs.send]
external fillIntOffset : t -> value:int -> offset:int -> t = "fill" [@@bs.send]
external fillIntRange : t -> value:int -> offset:int -> offsetEnd:int -> t =
  "fill" [@@bs.send]
external includesString : t -> Js.String.t -> bool = "includes" [@@bs.send]
external includesStringOffset : t -> value:Js.String.t -> offset:int -> bool =
  "includes" [@@bs.send]
external includesStringOffsetWithEncoding : t -> value:Js.String.t -> offset:int ->
  encoding:Js.String.t -> bool ="includes" [@@bs.send]
external includesBuffer : t -> t -> bool = "includes" [@@bs.send]
external includesBufferOffset : t -> value:t -> offset:int -> bool =
  "includes" [@@bs.send]
external includesInt : t -> int -> bool = "includes" [@@bs.send]
external includesIntOffset : t -> value:int -> offset:int -> bool =
  "includes" [@@bs.send]
