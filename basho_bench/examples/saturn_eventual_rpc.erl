{mode, max}.

{duration, 1}.

{concurrent, 20}.

{driver, basho_bench_driver_saturn_eventual_rpc}.

{key_generator, {uniform_int, 1000}}.

{value_generator, {fixed_bin, 100}}.

{saturn_dcs_nodes, [['dev1@127.0.0.1'], ['dev2@127.0.0.1']]}.
{saturn_bucket, 1}.
{saturn_cookie, saturn_leaf}.
{saturn_mynode, ['saturn_bench@127.0.0.1', longnames]}.

{operations, [{update, 1},{read, 10}]}.
%{operations, [{read, 1}]}.
