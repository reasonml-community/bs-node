module Hmac: {
  type t;
  let create: ([ | `SHA256 | `SHA512 | `SHA384 | `SHA1 | `MD5], string) => t;
  let appendString: (t, string) => t;
  let appendBuffer: (t, NodeBuffer.t) => t;
  let toBase64: t => string;
  let toHex: t => string;
  let toBuffer: t => NodeBuffer.t;
} = {
  type t;
  [@bs.module "crypto"]
  external create:
    (
      [@bs.string] [
        | [@bs.as "sha256"] `SHA256
        | [@bs.as "sha512"] `SHA512
        | [@bs.as "sha384"] `SHA384
        | [@bs.as "sha1"] `SHA1
        | [@bs.as "md5"] `MD5
      ],
      string
    ) =>
    t =
    "createHmac";
  [@bs.send]
  external update:
    (t, [@bs.unwrap] [ | `String(string) | `Buffer(NodeBuffer.t)]) => t =
    "";
  let appendString = (hmac, str) => update(hmac, `String(str));
  let appendBuffer = (hmac, buffer) => update(hmac, `Buffer(buffer));
  [@bs.send] external digest: (t, Js.Nullable.t(string)) => 'a = "";
  let toBase64: t => string =
    hmac => digest(hmac, Js.Nullable.return("base64"));
  let toHex: t => string = hmac => digest(hmac, Js.Nullable.return("hex"));
  let toBuffer: t => NodeBuffer.t =
    hmac => digest(hmac, Js.Nullable.undefined);
};

module Hash: {
  type t;
  let create: [ | `SHA256 | `SHA512 | `SHA384 | `SHA1 | `MD5] => t;
  let appendString: (t, string) => unit;
  let appendBuffer: (t, NodeBuffer.t) => unit;
  let toBase64: t => string;
  let toBuffer: t => NodeBuffer.t;
} = {
  type t;
  [@bs.module "crypto"]
  external create:
    (
    [@bs.string]
    [
      | [@bs.as "sha256"] `SHA256
      | [@bs.as "sha512"] `SHA512
      | [@bs.as "sha384"] `SHA384
      | [@bs.as "sha1"] `SHA1
      | [@bs.as "md5"] `MD5
    ]
    ) =>
    t =
    "createHash";
  [@bs.send]
  external update:
    (t, [@bs.unwrap] [ | `String(string) | `Buffer(NodeBuffer.t)]) => unit =
    "";
  let appendString = (hmac, str) => update(hmac, `String(str));
  let appendBuffer = (hmac, buffer) => update(hmac, `Buffer(buffer));
  [@bs.send] external digest: (t, Js.Nullable.t(string)) => 'a = "";
  let toBase64: t => string =
    hmac => digest(hmac, Js.Nullable.return("base64"));
  let toBuffer: t => NodeBuffer.t =
    hmac => digest(hmac, Js.Nullable.undefined);
};
