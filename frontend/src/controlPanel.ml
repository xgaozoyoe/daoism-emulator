open Common

let mk_inventory_info inventory =
  Menu.mk_info @@
    Array.mapi (fun i a ->
      let r = "slot" ^ (string_of_int i) in
        match Js.Nullable.toOption a with
      | None -> (r, "Empty")
      | Some equip ->
        let (n, i) = (Common.get_the_entry equip) in
        (r, n ^ ":" ^ string_of_int i)
    ) inventory

