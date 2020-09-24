open Core
type 'a common_state = {
  description: string;
  deliver: ((Object.t Feature.t) * Object.t option) array;
  state: 'a;
}

let state_to_json ns info_builder =
  `Assoc [
    ("description", `String ns.description)
    ; ("extra", info_builder ns.state)
    ; ("features", `List (List.map (fun pre -> Object.pre_event_to_json pre)
        (Array.to_list ns.deliver)))
   ]


module CommonState = struct

  open Quality

  let mk_obj_attr notice = Attribute.Api.Object notice

  let quality_to_total = function
    | Unique -> 200
    | Rare -> 150
    | Elite -> 100
    | Normal -> 80
    | Antique -> Random.int 200

  let idle_state state =
    { state with deliver = [||]; description = "空闲"}, None

  let practise_state quality state =
    let es = Array.map (fun x-> x, None)
      (Level.BasicSystem.make_basic_features (quality_to_total quality))
    in
    { state with deliver = es; description = "修炼" }, Some (Timer.of_int 9)

  let move_state state src target =
    let es = begin
      if src = target then [||]
      else [|
        Feature.mk_hold (mk_obj_attr Attribute.Api.Leave) 1, Some src
        ; Feature.mk_hold (mk_obj_attr Attribute.Api.Enter) 1, Some target
      |]
    end in
    { state with deliver = es; description = "移动"}, Some (Timer.of_int 4)

  let explore_state state universe =
    let es = [|Feature.mk_hold (mk_obj_attr Attribute.Api.Stay) 1, Some universe|] in
    { state with deliver = es; description = "探索"}, Some (Timer.of_int 5)

  let dead_state state tile universe =
    let es = [|
      Feature.mk_hold (mk_obj_attr Attribute.Api.Leave) 1, Some tile
      ; Feature.mk_hold (mk_obj_attr Attribute.Api.Dead) 1, Some universe
    |] in
    {state with deliver = es; description="死亡"}, Timer.of_int 1
end

module SpawnSystem = struct
  module SpawnMap = Map.Make(String)

  let spawn_map: ((Object.t -> Object.t) SpawnMap.t) ref
      = ref (SpawnMap.empty)

  (* Register spawner with name and call back *)
  let register_spawner name cb =
    spawn_map := SpawnMap.add name cb !spawn_map

  let get_spanwer name =
    SpawnMap.find name !spawn_map

  let get_spawners () =
    List.of_seq @@ SpawnMap.to_seq !spawn_map
end

