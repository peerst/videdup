-module(videdup_SUITE).

-compile(export_all).

suite() -> [{timetrap, {seconds, 120}}].

all() -> [videos_exist].

init_per_suite(Config) ->
    %% Ensure suite data dir exists (Common Test provides it if present on disk)
    {ok, Config}.

end_per_suite(_Config) -> ok.

videos_exist(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    Files = filelib:wildcard(filename:join(DataDir, "*.mp4")),
    ?assert(length(Files) >= 3),
    ok.


