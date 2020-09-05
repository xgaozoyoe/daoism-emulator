type command =
  | FetchData
  | Command of string
  [@@deriving yojson]
