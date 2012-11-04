{application, world,
  [{description, "Simple world based on the style of WOOD1 from Wilson."},
   {vsn, "0.2.0"},
   {modules, [world, world_sup, world_env, world_ctl_sservsup, world_ctl_sserv, world_sservsup, world_sserv, world_helper]},
   {registered, [world_sup, world_env, world_ctl_sservsup, world_ctl_sserv, world_sservsup, world_sserv]},
   {env, [{port, 4567}, {ctl_port, 4568}]},
   {mod, {world, []}}
  ]}.
