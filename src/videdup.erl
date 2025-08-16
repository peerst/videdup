-module(videdup).

-export([read_sig/1,
         write_signature/1]).

-record(fine,  {idx, confidence, words, frame_sig}).   %% one frame
-record(coarse,{bitmaps}).                             %% 5×31 bytes

read_sig(File) ->
    case file:read_file(File) of
        {ok, Bin} ->
            case parse_sgp(Bin) of
                {ok, _}=Ok -> Ok;
                {error, _} -> parse_ffmpeg_signature(Bin)
            end;
        {error, _}=Error -> Error
    end.

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

parse_sgp(<<$S,$G,$P,0, 1:32/little, FineN:32/little, CoarseN:32/little, Rest/binary>>) ->
    {Fines,AfterFine} = read_fines(FineN, Rest, []),
    Coarses          = read_coarses(CoarseN, AfterFine, []),
    {ok, #{format => sgp_v1,
           fine => lists:reverse(Fines),
           coarse => lists:reverse(Coarses)}};
parse_sgp(_Bin) -> {error, not_sgp}.

parse_ffmpeg_signature(
    <<1:32/big, Flags1:32/big, Word3:32/big, Flags2:32/big, HeaderSz:32/big, Rest/binary>> = Bin
) when byte_size(Bin) >= 32 ->
    %% Basic structural validation observed from real FFmpeg signature files
    case {Flags1 band 16#80000000, Flags2 band 16#80000000} of
        {16#80000000, 16#80000000} ->
            {ok, #{format => ffmpeg_signature,
                   version => 1,
                   flags1 => Flags1,
                   flags2 => Flags2,
                   word3 => Word3,
                   header_size => HeaderSz,
                   payload_bytes => byte_size(Rest),
                   size_bytes => byte_size(Bin)}};
        _ ->
            {error, unsupported_signature_format}
    end;
parse_ffmpeg_signature(_Bin) -> {error, unsupported_signature_format}.
