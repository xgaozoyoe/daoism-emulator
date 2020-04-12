open Core
type wuxing =
  | Jing (* 金 *)
  | Mu   (* 木 *)
  | Shui (* 水 *)
  | Huo  (* 火 *)
  | Tu   (* 土 *)

type potential =
  | Wu (* 悟性 *)
  | Ji (* 机缘 *)
  | Qi (* 潜质 *)

class wuXing wx = object
  method name = match wx with
    | Jing -> "Jing"
    | Mu -> "Mu"
    | Shui -> "Shui"
    | Huo -> "Huo"
    | Tu -> "Tu"
end

class growth p = object
  method name = match p with
    | Wu -> "Wu"
    | Ji -> "Ji"
    | Qi -> "Qi"
end

module Make (O:Object.Interface) = struct

  let make_basic_features total =
    let ratio = [|Random.int 10; Random.int 10; Random.int 10; Random.int 10|] in
    let sum = Array.fold_left (fun acc c -> c + acc) 0 ratio in
    let ratio = Array.map (fun c -> (c * total) / sum) ratio in
    let wx = [|
        new wuXing Jing;
        new wuXing Mu;
        new wuXing Shui;
        new wuXing Huo;
        new wuXing Tu;
    |] in
    Array.map2 (fun amount wx -> O.Env.Feature.mk_produce wx amount) ratio wx

  let make_apprentice full_name quality =
    let obj = O.mk_empty full_name full_name in
    let total = Config.total_wuxing_amount quality in
    let basic_features = make_basic_features total in
    O.take_features (Array.to_list basic_features) obj

end
