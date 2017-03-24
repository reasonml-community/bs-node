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
(* external entries : t -> Iterator = "" [@@bs.get] *)

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

external indexOfString : t -> Js.String.t -> int = "indexOf" [@@bs.send]
external indexOfStringOffset : t -> value:Js.String.t -> offset:int -> int =
  "indexOf" [@@bs.send]
external indexOfStringOffsetWithEncoding : t -> value:Js.String.t -> offset:int ->
  encoding:Js.String.t -> int ="indexOf" [@@bs.send]
external indexOfBuffer : t -> t -> int = "indexOf" [@@bs.send]
external indexOfBufferOffset : t -> value:t -> offset:int -> int =
  "indexOf" [@@bs.send]
external indexOfInt : t -> int -> int = "indexOf" [@@bs.send]
external indexOfIntOffset : t -> value:int -> offset:int -> int =
  "indexOf" [@@bs.send]

(* FIXME after iterators support *)
(* external keys : t -> Iterator = "" [@@bs.send] *)

external lastIndexOfString : t -> Js.String.t -> int = "lastIndexOf" [@@bs.send]
external lastIndexOfStringOffset : t -> value:Js.String.t -> offset:int -> int =
  "lastIndexOf" [@@bs.send]
external lastIndexOfStringOffsetWithEncoding : t -> value:Js.String.t -> offset:int ->
  encoding:Js.String.t -> int ="lastIndexOf" [@@bs.send]
external lastIndexOfBuffer : t -> t -> int = "lastIndexOf" [@@bs.send]
external lastIndexOfBufferOffset : t -> value:t -> offset:int -> int =
  "lastIndexOf" [@@bs.send]
external lastIndexOfInt : t -> int -> int = "lastIndexOf" [@@bs.send]
external lastIndexOfIntOffset : t -> value:int -> offset:int -> int =
  "lastIndexOf" [@@bs.send]

external length : t -> int = "" [@@bs.get]

external readDoubleBE : t -> offset:int -> float = "" [@@bs.send]
external readDoubleBENoAssert : t -> offset:int -> noAssert:Js.boolean -> float =
  "readDoubleBE" [@@bs.send]
let readDoubleBENoAssert buffer ~offset = readDoubleBENoAssert buffer ~offset ~noAssert:Js.true_

external readDoubleLE : t -> offset:int -> float = "" [@@bs.send]
external readDoubleLENoAssert : t -> offset:int -> noAssert:Js.boolean -> float =
  "readDoubleLE" [@@bs.send]
let readDoubleLENoAssert buffer ~offset = readDoubleLENoAssert buffer ~offset ~noAssert:Js.true_

external readFloatBE : t -> offset:int -> float = "" [@@bs.send]
external readFloatBENoAssert : t -> offset:int -> noAssert:Js.boolean -> float =
  "readFloatBE" [@@bs.send]
let readFloatBENoAssert buffer ~offset = readFloatBENoAssert buffer ~offset ~noAssert:Js.true_

external readFloatLE : t -> offset:int -> float = "" [@@bs.send]
external readFloatLENoAssert : t -> offset:int -> noAssert:Js.boolean -> float =
  "readFloatLE" [@@bs.send]
let readFloatLENoAssert buffer ~offset = readFloatLENoAssert buffer ~offset ~noAssert:Js.true_

external readInt8 : t -> offset:int -> float = "" [@@bs.send]
external readInt8NoAssert : t -> offset:int -> noAssert:Js.boolean -> float =
  "readInt8" [@@bs.send]
let readInt8NoAssert buffer ~offset = readInt8NoAssert buffer ~offset ~noAssert:Js.true_

external readInt16BE : t -> offset:int -> float = "" [@@bs.send]
external readInt16BENoAssert : t -> offset:int -> noAssert:Js.boolean -> float =
  "readInt16BE" [@@bs.send]
let readInt16BENoAssert buffer ~offset = readInt16BENoAssert buffer ~offset ~noAssert:Js.true_

external readInt16LE : t -> offset:int -> float = "" [@@bs.send]
external readInt16LENoAssert : t -> offset:int -> noAssert:Js.boolean -> float =
  "readInt16LE" [@@bs.send]
let readInt16LENoAssert buffer ~offset = readInt16LENoAssert buffer ~offset ~noAssert:Js.true_

external readInt32BE : t -> offset:int -> float = "" [@@bs.send]
external readInt32BENoAssert : t -> offset:int -> noAssert:Js.boolean -> float =
  "readInt32BE" [@@bs.send]
let readInt32BENoAssert buffer ~offset = readInt32BENoAssert buffer ~offset ~noAssert:Js.true_

external readInt32LE : t -> offset:int -> float = "" [@@bs.send]
external readInt32LENoAssert : t -> offset:int -> noAssert:Js.boolean -> float =
  "readInt32LE" [@@bs.send]
let readInt32LENoAssert buffer ~offset = readInt32LENoAssert buffer ~offset ~noAssert:Js.true_

external readIntBE : t -> offset:int -> length:int -> float = "" [@@bs.send]
external readIntBENoAssert : t -> offset:int -> length:int -> noAssert:Js.boolean ->
  float = "readIntBE" [@@bs.send]
let readIntBENoAssert buffer ~offset ~length = readIntBENoAssert buffer ~offset
  ~length ~noAssert:Js.true_

external readIntLE : t -> offset:int -> length:int -> float = "" [@@bs.send]
external readIntLENoAssert : t -> offset:int -> length:int -> noAssert:Js.boolean ->
  float = "readIntLE" [@@bs.send]
let readIntLENoAssert buffer ~offset ~length = readIntLENoAssert buffer ~offset
  ~length ~noAssert:Js.true_

external readUint8 : t -> offset:int -> float = "" [@@bs.send]
external readUint8NoAssert : t -> offset:int -> noAssert:Js.boolean -> float =
  "readUint8" [@@bs.send]
let readUint8NoAssert buffer ~offset = readUint8NoAssert buffer ~offset ~noAssert:Js.true_

external readUint16BE : t -> offset:int -> float = "" [@@bs.send]
external readUint16BENoAssert : t -> offset:int -> noAssert:Js.boolean -> float =
  "readUint16BE" [@@bs.send]
let readUint16BENoAssert buffer ~offset = readUint16BENoAssert buffer ~offset ~noAssert:Js.true_

external readUint16LE : t -> offset:int -> float = "" [@@bs.send]
external readUint16LENoAssert : t -> offset:int -> noAssert:Js.boolean -> float =
  "readUint16LE" [@@bs.send]
let readUint16LENoAssert buffer ~offset = readUint16LENoAssert buffer ~offset ~noAssert:Js.true_

external readUint32BE : t -> offset:int -> float = "" [@@bs.send]
external readUint32BENoAssert : t -> offset:int -> noAssert:Js.boolean -> float =
  "readUint32BE" [@@bs.send]
let readUint32BENoAssert buffer ~offset = readUint32BENoAssert buffer ~offset ~noAssert:Js.true_

external readUint32LE : t -> offset:int -> float = "" [@@bs.send]
external readUint32LENoAssert : t -> offset:int -> noAssert:Js.boolean -> float =
  "readUint32LE" [@@bs.send]
let readUint32LENoAssert buffer ~offset = readUint32LENoAssert buffer ~offset ~noAssert:Js.true_

external readUintBE : t -> offset:int -> length:int -> float = "" [@@bs.send]
external readUintBENoAssert : t -> offset:int -> length:int -> noAssert:Js.boolean ->
  float = "readUintBE" [@@bs.send]
let readUintBENoAssert buffer ~offset ~length = readUintBENoAssert buffer ~offset
  ~length ~noAssert:Js.true_

external readUintLE : t -> offset:int -> length:int -> float = "" [@@bs.send]
external readUintLENoAssert : t -> offset:int -> length:int -> noAssert:Js.boolean ->
  float = "readUintLE" [@@bs.send]
let readUintLENoAssert buffer ~offset ~length = readUintLENoAssert buffer ~offset
  ~length ~noAssert:Js.true_

external slice : t -> t = "" [@@bs.send]
external sliceOffset : t -> start:int -> t = "slice" [@@bs.send]
external sliceRange : t -> start:int -> end_:int -> t = "slice" [@@bs.send]

external swap16 : t -> t = "" [@@bs.send]
external swap32 : t -> t = "" [@@bs.send]
external swap64 : t -> t = "" [@@bs.send]

external toJSON : t -> < .. > Js.t = "" [@@bs.send]

external toString: t -> Js.String.t = "" [@@bs.send]
external toStringWithEncoding: t -> encoding:Js.String.t -> Js.String.t =
  "toString" [@@bs.send]
external toStringWithEncodingOffset: t -> encoding:Js.String.t -> start:int
  -> Js.String.t = "toString" [@@bs.send]
external toStringWithEncodingRange: t -> encoding:Js.String.t -> start:int
  -> end_:int -> Js.String.t = "toString" [@@bs.send]

(* FIXME after iterators support *)
(* external values : t -> Iterator = "" [@@bs.get] *)

external write : t -> Js.String.t -> int = "" [@@bs.send]
external writeOffset : t -> value:Js.String.t -> offset:int -> int =
  "write" [@@bs.send]
external writeRange : t -> value:Js.String.t -> offset:int -> length:int -> int =
  "write" [@@bs.send]
external writeRangeWithEncoding : t -> value:Js.String.t -> offset:int ->
  length:int -> encoding:Js.String.t -> int = "write" [@@bs.send]

external writeDoubleBE : t -> value:float -> offset:int -> float = "" [@@bs.send]
external writeDoubleBENoAssert : t -> value:float -> offset:int -> noAssert:Js.boolean -> float =
  "writeDoubleBE" [@@bs.send]
let writeDoubleBENoAssert buffer ~value ~offset = writeDoubleBENoAssert buffer ~value ~offset ~noAssert:Js.true_

external writeDoubleLE : t -> value:float -> offset:int -> float = "" [@@bs.send]
external writeDoubleLENoAssert : t -> value:float -> offset:int -> noAssert:Js.boolean -> float =
  "writeDoubleLE" [@@bs.send]
let writeDoubleLENoAssert buffer ~value ~offset = writeDoubleLENoAssert buffer ~value ~offset ~noAssert:Js.true_

external writeFloatBE : t -> value:float -> offset:int -> float = "" [@@bs.send]
external writeFloatBENoAssert : t -> value:float -> offset:int -> noAssert:Js.boolean -> float =
  "writeFloatBE" [@@bs.send]
let writeFloatBENoAssert buffer ~value ~offset = writeFloatBENoAssert buffer ~value ~offset ~noAssert:Js.true_

external writeFloatLE : t -> value:float -> offset:int -> float = "" [@@bs.send]
external writeFloatLENoAssert : t -> value:float -> offset:int -> noAssert:Js.boolean -> float =
  "writeFloatLE" [@@bs.send]
let writeFloatLENoAssert buffer ~value ~offset = writeFloatLENoAssert buffer ~value ~offset ~noAssert:Js.true_

external writeInt8 : t -> value:int -> offset:int -> float = "" [@@bs.send]
external writeInt8NoAssert : t -> value:int -> offset:int -> noAssert:Js.boolean -> float =
  "writeInt8" [@@bs.send]
let writeInt8NoAssert buffer ~value ~offset = writeInt8NoAssert buffer ~value ~offset ~noAssert:Js.true_

external writeInt16BE : t -> value:int -> offset:int -> float = "" [@@bs.send]
external writeInt16BENoAssert : t -> value:int -> offset:int -> noAssert:Js.boolean -> float =
  "writeInt16BE" [@@bs.send]
let writeInt16BENoAssert buffer ~value ~offset = writeInt16BENoAssert buffer ~value ~offset ~noAssert:Js.true_

external writeInt16LE : t -> value:int -> offset:int -> float = "" [@@bs.send]
external writeInt16LENoAssert : t -> value:int -> offset:int -> noAssert:Js.boolean -> float =
  "writeInt16LE" [@@bs.send]
let writeInt16LENoAssert buffer ~value ~offset = writeInt16LENoAssert buffer ~value ~offset ~noAssert:Js.true_

external writeInt32BE : t -> value:int -> offset:int -> float = "" [@@bs.send]
external writeInt32BENoAssert : t -> value:int -> offset:int -> noAssert:Js.boolean -> float =
  "writeInt32BE" [@@bs.send]
let writeInt32BENoAssert buffer ~value ~offset = writeInt32BENoAssert buffer ~value ~offset ~noAssert:Js.true_

external writeInt32LE : t -> value:int -> offset:int -> float = "" [@@bs.send]
external writeInt32LENoAssert : t -> value:int -> offset:int -> noAssert:Js.boolean -> float =
  "writeInt32LE" [@@bs.send]
let writeInt32LENoAssert buffer ~value ~offset = writeInt32LENoAssert buffer ~value ~offset ~noAssert:Js.true_

external writeIntBE : t -> value:int -> offset:int -> length:int -> float = "" [@@bs.send]
external writeIntBENoAssert : t -> value:int -> offset:int -> length:int -> noAssert:Js.boolean ->
  float = "writeIntBE" [@@bs.send]
let writeIntBENoAssert buffer ~value ~offset ~length = writeIntBENoAssert buffer ~value ~offset
  ~length ~noAssert:Js.true_

external writeIntLE : t -> value:int -> offset:int -> length:int -> float = "" [@@bs.send]
external writeIntLENoAssert : t -> value:int -> offset:int -> length:int -> noAssert:Js.boolean ->
  float = "writeIntLE" [@@bs.send]
let writeIntLENoAssert buffer ~value ~offset ~length = writeIntLENoAssert buffer ~value ~offset
  ~length ~noAssert:Js.true_

external writeUint8 : t -> value:int -> offset:int -> float = "" [@@bs.send]
external writeUint8NoAssert : t -> value:int -> offset:int -> noAssert:Js.boolean -> float =
  "writeUint8" [@@bs.send]
let writeUint8NoAssert buffer ~value ~offset = writeUint8NoAssert buffer ~value ~offset ~noAssert:Js.true_

external writeUint16BE : t -> value:int -> offset:int -> float = "" [@@bs.send]
external writeUint16BENoAssert : t -> value:int -> offset:int -> noAssert:Js.boolean -> float =
  "writeUint16BE" [@@bs.send]
let writeUint16BENoAssert buffer ~value ~offset = writeUint16BENoAssert buffer ~value ~offset ~noAssert:Js.true_

external writeUint16LE : t -> value:int -> offset:int -> float = "" [@@bs.send]
external writeUint16LENoAssert : t -> value:int -> offset:int -> noAssert:Js.boolean -> float =
  "writeUint16LE" [@@bs.send]
let writeUint16LENoAssert buffer ~value ~offset = writeUint16LENoAssert buffer ~value ~offset ~noAssert:Js.true_

external writeUint32BE : t -> value:int -> offset:int -> float = "" [@@bs.send]
external writeUint32BENoAssert : t -> value:int -> offset:int -> noAssert:Js.boolean -> float =
  "writeUint32BE" [@@bs.send]
let writeUint32BENoAssert buffer ~value ~offset = writeUint32BENoAssert buffer ~value ~offset ~noAssert:Js.true_

external writeUint32LE : t -> value:int -> offset:int -> float = "" [@@bs.send]
external writeUint32LENoAssert : t -> value:int -> offset:int -> noAssert:Js.boolean -> float =
  "writeUint32LE" [@@bs.send]
let writeUint32LENoAssert buffer ~value ~offset = writeUint32LENoAssert buffer ~value ~offset ~noAssert:Js.true_

external writeUintBE : t -> value:int -> offset:int -> length:int -> float = "" [@@bs.send]
external writeUintBENoAssert : t -> value:int -> offset:int -> length:int -> noAssert:Js.boolean ->
  float = "writeUintBE" [@@bs.send]
let writeUintBENoAssert buffer ~value ~offset ~length = writeUintBENoAssert buffer ~value ~offset
  ~length ~noAssert:Js.true_

external writeUintLE : t -> value:int -> offset:int -> length:int -> float = "" [@@bs.send]
external writeUintLENoAssert : t -> value:int -> offset:int -> length:int -> noAssert:Js.boolean ->
  float = "writeUintLE" [@@bs.send]
let writeUintLENoAssert buffer ~value ~offset ~length = writeUintLENoAssert buffer ~value ~offset
  ~length ~noAssert:Js.true_

external inspectMaxBytes : t -> int = "INSPECT_MAX_BYTES" [@@bs.get]
external kMaxLength : t -> int = "" [@@bs.get]

external transcode : t -> source:t -> fromEnc:Js.String.t -> toEnc:Js.String.t ->
  t = "" [@@bs.send]
