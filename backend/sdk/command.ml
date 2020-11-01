type subcommand =
  | Move of string
  | Attack of string
  | Pick of (string * int)
  | Construct of (int * int * int)
  [@@deriving yojson]

type command =
  | FetchData
  | Command of (string * subcommand)
  [@@deriving yojson]

exception InvalidCommand of string
