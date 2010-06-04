{application, uurl,
 [{description, "uurl"},
  {vsn, "0.01"},
  {modules, [
    uurl,
    uurl_app,
    uurl_sup,
    uurl_web,
    uurl_deps,
	uurl_db
  ]},
  {registered, []},
  {mod, {uurl_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
