let generate_player _ =
  Attribute.Api.Spawn (Attribute.Spawn.Apprentice (Player.mk_player))
