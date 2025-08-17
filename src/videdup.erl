-module(videdup).

-export([read_sig/1,
         write_signature/1]).

read_sig(File) ->
    {ok,Bin} = file:read_file(File),
    {error, not_yet_implemented}.
   
%% Generate an ffmpeg signature file for the given video using vice.
%% Writes ./<basename>.sig in the current working directory and returns {ok, SigFile}.
write_signature(VideoPath) when is_list(VideoPath) ->
    BaseName = filename:basename(VideoPath),
    RootName = filename:rootname(BaseName),
    SigFile  = filename:join(".", RootName ++ ".sig"),

    Options = [
        {map, "0:v"},
        {video_filtergraph, lists:flatten(io_lib:format("signature=filename=~s", [SigFile]))},
        {output_format, "null"}
    ],

    case vice:convert(VideoPath, "-", Options, sync) of
        ok -> {ok, SigFile};
        {ok, _In, _Out} -> {ok, SigFile};
        {ok, _Something} -> {ok, SigFile};
        {error, _}=Error -> Error;
        Other -> {error, {unexpected_convert_result, Other}}
    end.
    