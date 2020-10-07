type rule = {
  require:int Js.Dict.t;
  result:string;
}[@@bs.deriving abstract]

type pre_event = {
  feature: string Js.Dict.t;
  target: string;
}[@@bs.deriving abstract]

type environ = {
  features:int Js.Dict.t;
  rules: rule array
}[@@bs.deriving abstract]

type location = {
  x:int;
  y:int;
}[@@bs.deriving abstract]

type state = {
  description: string;
  extra: string Js.Dict.t;
  deliver: pre_event array;
}[@@bs.deriving abstract]




