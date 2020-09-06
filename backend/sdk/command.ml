type command =
  | FetchData
  | Command of string
  [@@deriving yojson]

type subcommand =
  | Move of (int * int)
  [@@deriving yojson]
