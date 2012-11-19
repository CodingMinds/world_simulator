{application, world,
  [{description,
     "A system which provides some simple worlds based on the style of
     WOOD1 from Wilson."},
   {vsn, "0.7.0"},
   {modules, [world, world_sup, world_ofo_sup, world_env,
     world_ctl_sservsup, world_ctl_sserv, world_sservsup, world_sserv,
     world_logging, world_helper, world_http]},
   {registered, [world_sup, world_ofo_sup, world_env,
     world_ctl_sservsup, world_ctl_sserv, world_sservsup, world_sserv,
     world_logging]},
   {env, [
     {port, 4567},
     {ctl_port, 4568},
     {http_port, 8080},
     {log_client, "log/clients.log"},
     {log_env, "log/environment.log"},
     {log_info, "log/info.log"},
     {log_http, "log/http_transfer.log"},
     {log_http_error, "log/http_error.log"}
    ]},
   {applications, [stdlib, kernel]},
   {mod, {world, []}}
  ]}.
