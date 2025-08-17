-module(videdup).

-export([read_sig/1,
         write_signature/1]).

-record(fine,  {idx, confidence, words, frame_sig}).   %% one frame
-record(coarse,{bitmaps}).                             %% 5×31 bytes

read_sig(File) ->
    {ok,Bin} = file:read_file(File),
    <<$S,$G,$P,0,         %% magic
      1:32/little,        %% version
      FineN:32/little,
      CoarseN:32/little,
      Rest/binary>> = Bin,
    {Fines,AfterFine} = read_fines(FineN, Rest, []),
    Coarses          = read_coarses(CoarseN, AfterFine, []),
    #{fine => lists:reverse(Fines), coarse => lists:reverse(Coarses)}.

read_fines(0, Bin, Acc) -> {Acc,Bin};
read_fines(N, <<Idx:32/little,Conf:8,Words:40/binary,
                Sig:76/binary,Rest/binary>>, Acc) ->
    F = #fine{idx=Idx, confidence=Conf,
              words  = binary_to_list(Words),
              frame_sig = Sig},
    read_fines(N-1, Rest, [F|Acc]).

read_coarses(0, _Bin, Acc) -> Acc;
read_coarses(N, <<Bitmaps:155/binary,Rest/binary>>, Acc) ->
    read_coarses(N-1, Rest, [#coarse{bitmaps=Bitmaps}|Acc]).

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
    