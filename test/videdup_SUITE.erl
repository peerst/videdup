-module(videdup_SUITE).

-compile(export_all).
-include_lib("common_test/include/ct.hrl").

suite() -> [{timetrap, {seconds, 120}}].

all() -> [videos_exist, write_signature_creates_sig, read_sig_parses_sig].

init_per_suite(Config) ->
    %% Ensure suite data dir exists (Common Test provides it if present on disk)
    Config.

end_per_suite(_Config) -> ok.

videos_exist(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    Files = filelib:wildcard(filename:join(DataDir, "*.mp4")),
    case length(Files) >= 3 of
        true -> ok;
        false -> ct:fail({not_enough_files, Files})
    end.

write_signature_creates_sig(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    Video = filename:join(DataDir, "t_1.mp4"),
    {ok, Cwd} = file:get_cwd(),
    ok = file:set_cwd(PrivDir),
    try
        _ = application:ensure_all_started(vice),
        {ok, SigPath} = videdup:write_signature(Video),
        true = filelib:is_file(filename:absname("t_1.sig")),
        true = (SigPath =:= "./t_1.sig"),
        ok
    after
        file:delete("t_1.sig"),
        ok = file:set_cwd(Cwd),
        application:stop(vice)
    end.

read_sig_parses_sig(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    Video = filename:join(DataDir, "t_2.mp4"),
    {ok, Cwd} = file:get_cwd(),
    ok = file:set_cwd(PrivDir),
    try
        _ = application:ensure_all_started(vice),
        {ok, _} = videdup:write_signature(Video),
        %% Now parse the generated sig
        case videdup:read_sig("./t_2.sig") of
            {ok, #{format := sgp_v1, fine := Fine, coarse := Coarse}} when is_list(Fine), is_list(Coarse) ->
                ok;
            {ok, #{format := ffmpeg_signature, version := _V, size_bytes := Size}} when is_integer(Size), Size > 0 ->
                ok;
            Other -> ct:fail({unexpected_read_sig_result, Other})
        end
    after
        file:delete("t_2.sig"),
        ok = file:set_cwd(Cwd),
        application:stop(vice)
    end.


