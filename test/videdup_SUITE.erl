-module(videdup_SUITE).

-compile(export_all).
-include_lib("common_test/include/ct.hrl").

suite() -> [{timetrap, {seconds, 120}}].

all() -> [videos_exist, write_signature_creates_sig, read_sig_parses_sig].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(vice),
    Config.

end_per_suite(_Config) ->
    ok = application:stop(vice).


end_per_testcase(_TestCase, _Config) ->
    lists:foreach(fun file:delete/1, filelib:wildcard("*.sig")),
    ok.

videos_exist(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    Files = filelib:wildcard(filename:join(DataDir, "*.mp4")),
    case length(Files) >= 3 of
        true -> ok;
        false -> ct:fail({not_enough_files, Files})
    end.

write_signature_creates_sig(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    Video = filename:join(DataDir, "t_1.mp4"),
    {ok, SigPath} = videdup:write_signature(Video),
    true = filelib:is_file(filename:absname("t_1.sig")),
    "./t_1.sig" = SigPath,
    ok.

read_sig_parses_sig(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    Video = filename:join(DataDir, "t_2.mp4"),
    {ok, _} = videdup:write_signature(Video),
    {ok, #{}} = videdup:read_sig("t_2.sig"),
    ok.


