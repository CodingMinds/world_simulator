{application, world,
  [{description,
     "Simple world based on the style of WOOD1 from Wilson."},
   {vsn, "0.3.0"},
   {modules, [world, world_sup, world_env, world_ctl_sservsup,
     world_ctl_sserv, world_sservsup, world_sserv, world_logging,
     world_helper]},
   {registered, [world_sup, world_env, world_ctl_sservsup,
     world_ctl_sserv, world_sservsup, world_sserv, world_logging]},
   {env, [
     {port, 4567},
     {ctl_port, 4568},
     {log_client, "../logs/clients.log"},
     {log_env, "../logs/environment.log"},
     {log_info, "../logs/info.log"}
    ]},
   {mod, {world, []}}
  ]}.
