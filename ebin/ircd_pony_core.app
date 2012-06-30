{application,ircd_pony_core,
             [{description,"An experimental IRC client"},
              {vsn,"0.1"},
              {registered,[]},
              {applications,[kernel,stdlib]},
              {mod,{ircd_pony_core_app,[]}},
              {env,[]},
              {modules,[hostmask,ircd_pony_core_app,ircd_pony_core_sup,
                        protocol]}]}.
