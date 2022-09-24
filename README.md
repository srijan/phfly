phfly
=====

Solving https://protohackers.com challenges using Erlang.

Dependencies:
* [Ranch](https://github.com/ninenines/ranch) as a socket acceptor.
* [Thoas](https://github.com/lpil/thoas) json parser
* [gproc](https://github.com/uwiger/gproc) for process registry and pubsub

Each solution is in a module implementing the `ranch_protocol` behavior.

The config file at `config/sys.config` controls which behavior is used at runtime.

To pass the problems, a server must be running. A fly.toml file is provided to host on
https://fly.io.

