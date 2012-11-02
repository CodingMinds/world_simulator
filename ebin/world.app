{application, world,
  [{description, "Simple world based on the style of WOOD1 from Wilson."},
   {vsn, "0.1.0"},
   {modules, [world, world_sup, world_env, world_sservsup, world_sserv]},
   {registered, [world_sup, world_env, world_sservsup, world_sserv]},
   {env, [{port, 4567}]},
   {mod, {world, []}}
  ]}.
