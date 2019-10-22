{application, ebloom,
 [
  {description, "A NIF wrapper around a basic bloom filter"},
  {vsn, "2.0.0"},
  {modules, [
             ebloom
            ]},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib
                 ]},
  {env, []}
 ]}.
