type subcommand =
  | Move of (int * int)
  [@@deriving yojson]

type command =
  | FetchData
  | Command of (string * subcommand)
  [@@deriving yojson]

exception InvalidCommand of string
