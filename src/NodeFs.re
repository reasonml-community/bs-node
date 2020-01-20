/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
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
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. */

/**
  - Refernce documentation: https://nodejs.org/api/fs.html
  - Most fs functions let you omit the callback argument. If you do, a default
  callback is used that rethrows errors. To get a trace to the original call
  site, set the `NODE_DEBUG` environment variable:
*/
[@bs.module "fs"]
external readdirSync: string => array(string) = "";

[@bs.module "fs"] external renameSync: string => string = "";

type fd = pri int;

/**
   The relative path to a filename can be used. Remember, however, that this path
   will be relative to [process.cwd()].
*/

type path = string;

module Constants = {
  /**
    The following constants are meant for use with `access`.
    For reference: https://nodejs.org/api/fs.html#fs_fs_constants_1
  */

  /* == File Access Constants == */

  type accessFlag;

  /**
    Flag indicating that the file is visible to the calling process. This is useful for determining if a file exists, but says nothing about rwx permissions. Default if no mode is specified.
  */
  [@bs.module "fs"] [@bs.scope "constants"] [@bs.val]
  external exists: accessFlag = "F_OK";

  /**
    Flag indicating that the file can be read by the calling process.
  */
  [@bs.module "fs"] [@bs.scope "constants"] [@bs.val]
  external readable: accessFlag = "R_OK";

  /**
    Flag indicating that the file can be written by the calling process.
  */
  [@bs.module "fs"] [@bs.scope "constants"] [@bs.val]
  external writable: accessFlag = "W_OK";

  /**
    Flag indicating that the file can be executed by the calling process. This has no effect on Windows.
  */
  [@bs.module "fs"] [@bs.scope "constants"] [@bs.val]
  external executable: accessFlag = "X_OK";

  /* == File Copy Constants == */

  /**
    The following constants are meant for use with fs.copyFile().
  */
  type copyFlag;

  /**
    If present, the copy operation will fail with an error if the destination path already exists.
  */
  [@bs.module "fs"] [@bs.scope "constants"] [@bs.val]
  external failIfDestinationExists: copyFlag = "COPYFILE_EXCL";

  /**
    If present, the copy operation will attempt to create a copy-on-write reflink. If the underlying
    platform does not support copy-on-write, then a fallback copy mechanism is used.
  */
  [@bs.module "fs"] [@bs.scope "constants"] [@bs.val]
  external copyOnWrite: copyFlag = "COPYFILE_FICLONE";

  /**
    If present, the copy operation will attempt to create a copy-on-write reflink. If the underlying
    platform does not support copy-on-write, then the operation will fail with an error.
  */
  [@bs.module "fs"] [@bs.scope "constants"] [@bs.val]
  external forceCopyOnWrite: copyFlag = "COPYFILE_FICLONE_FORCE";
};

module Watch = {
  type t;
  type config;
  [@bs.obj]
  external config:
    (
      ~persistent: bool=?,
      ~recursive: bool=?,
      ~encoding: Js_string.t=?,
      unit
    ) =>
    config =
    "";

  /** there is no need to accept listener, since we return a [watcher]
      back it can register event listener there .
      Currently we introduce a type [stringBuffer], for the
      [filename], it will be [Buffer] when the encoding is `utf8.
      This is dependent type which can be tracked by GADT in some way,
      but to make things simple, let's just introduce an or type
  */
  [@bs.module "fs"]
  external watch: (string, ~config: config=?, unit) => t = "";

  /** there is no need to accept listener, since we return a [watcher]
      back it can register event listener there .
      Currently we introduce a type [stringBuffer], for the
      [filename], it will be [Buffer] when the encoding is `utf8.
      This is dependent type which can be tracked by GADT in some way,
      but to make things simple, let's just introduce an or type
  */
  [@bs.send]
  external on:
    (
      t,
      [@bs.string] [
        | `change(
            (. string /*eventType*/, NodeStringBuffer.t /* filename */) => unit,
          )
        | `error((. unit) => unit)
      ]
    ) =>
    t =
    "";

  [@bs.send] external close: (t, unit) => unit = "";
};

[@bs.module "fs"] external ftruncateSync: (fd, int) => unit = "";

[@bs.module "fs"] external truncateSync: (string, int) => unit = "";

[@bs.module "fs"]
external chownSync: (string, ~uid: int, ~gid: int) => unit = "";

[@bs.module "fs"] external fchownSync: (fd, ~uid: int, ~gid: int) => unit = "";

[@bs.module "fs"] external fchmodSync: (fd, ~uid: int) => unit = "";

[@bs.module "fs"] external readlinkSync: string => string = "";

[@bs.module "fs"] external unlinkSync: string => unit = "";

[@bs.module "fs"] external rmdirSync: string => unit = "";

/* TODO: [flags] support */
[@bs.module "fs"]
external openSync:
  (
    path,
    [@bs.string] [
      | [@bs.as "r"] `Read
      | [@bs.as "r+"] `ReadWrite
      | [@bs.as "rs+"] `ReadWriteSync
      | [@bs.as "w"] `Write
      | [@bs.as "wx"] `WriteFailIfExists
      | [@bs.as "w+"] `WriteRead
      | [@bs.as "wx+"] `WriteReadFailIfExists
      | [@bs.as "a"] `Append
      | [@bs.as "ax"] `AppendFailIfExists
      | [@bs.as "a+"] `AppendRead
      | [@bs.as "ax+"] `AppendReadFailIfExists
    ]
  ) =>
  unit =
  "";

[@bs.val] [@bs.module "fs"]
external readFileSync:
  (
    string,
    [@bs.string] [
      | `hex
      | `utf8
      | `ascii
      | `latin1
      | `base64
      | `ucs2
      | `base64
      | `binary
      | `utf16le
    ]
  ) =>
  string =
  "readFileSync";

[@bs.val] [@bs.module "fs"]
external readFileAsUtf8Sync: (string, [@bs.as "utf8"] _) => string =
  "readFileSync";

[@bs.val] [@bs.module "fs"] external existsSync: string => bool = "";

[@bs.val] [@bs.module "fs"]
external writeFileSync: (~filename: string, ~text: string) => unit = "";

[@bs.val] [@bs.module "fs"] external mkdirSync: string => unit = "";

[@bs.val] [@bs.module "fs"]
external accessSync: (string, Constants.accessFlag) => unit = "accessSync";

[@bs.val] [@bs.module "fs"]
external copyFileSync: (string, string) => unit = "copyFileSync";

[@bs.val] [@bs.module "fs"]
external copyFileWithFlagSync: (string, string, Constants.copyFlag) => unit =
  "copyFileSync";
