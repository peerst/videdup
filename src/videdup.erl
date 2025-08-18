-module(videdup).

-export([read_sig/1,
         write_signature/1]).

read_sig(File) ->
    case file:read_file(File) of
        {ok, Bin} ->
            parse_signature_header(Bin);
        {error, _}=Error -> Error
    end.

parse_signature_header(
    <<1:32/big,                      %% NumOfSpatialRegions (expect 1)
      SpatialFlag:1,                 %% SpatialLocationFlag (expect 1)
      PixelX1:16/big,                %% PixelX1 (lower 16 bits)
      PixelY1:16/big,                %% PixelY1 (lower 16 bits)
      PixelX2:16/big,                %% PixelX2 = width-1 (lower 16 bits)
      PixelY2:16/big,                %% PixelY2 = height-1 (lower 16 bits)
      StartFrame:32/big,             %% StartFrameOfSpatialRegion (expect 0)
      NumFrames:32/big,              %% NumOfFrames
      MediaTimeUnit:16/big,          %% MediaTimeUnit (timebase scalar)
      MediaTimeFlag:1,               %% MediaTimeFlagOfSpatialRegion (expect 1)
      StartMediaTime:32/big,         %% StartMediaTimeOfSpatialRegion (expect 0)
      EndMediaTime:32/big,           %% EndMediaTimeOfSpatialRegion (low 32 of last pts)
      NumSegments:32/big,            %% NumOfSegments
      Rest/bitstring>>
) when SpatialFlag =:= 1, MediaTimeFlag =:= 1 ->
    Width  = PixelX2 + 1,
    Height = PixelY2 + 1,
    {Segments, AfterSegments} = parse_segments(Rest, NumSegments, []),
    case parse_frames_section(AfterSegments, NumFrames) of
        {ok, AfterFrames} ->
            Map = #{spatial_location_flag => SpatialFlag,
                    pixel_x1 => PixelX1,
                    pixel_y1 => PixelY1,
                    width => Width,
                    height => Height,
                    start_frame => StartFrame,
                    num_frames => NumFrames,
                    media_time_unit => MediaTimeUnit,
                    start_media_time => StartMediaTime,
                    end_media_time => EndMediaTime,
                    num_segments => NumSegments,
                    segments => Segments,
                    rest => AfterFrames},
            {ok, Map};
        {error, _}=Error -> Error
    end;
parse_signature_header(_Other) ->
    {error, unsupported_signature_format}.

parse_segments(Bits, 0, Acc) -> {lists:reverse(Acc), Bits};
parse_segments(Bits, N, Acc) when N > 0 ->
    case parse_one_segment(Bits) of
        {ok, SegMap, NextBits} ->
            parse_segments(NextBits, N-1, [SegMap|Acc]);
        {error, _}=Error -> Error
    end.
 
parse_frames_section(
    <<CompressionFlag:1, Bits/bitstring>>, NumFrames
) when CompressionFlag =:= 0 ->
    parse_frames(Bits, NumFrames);
parse_frames_section(_Other, _NumFrames) -> {error, unsupported_signature_format}.

parse_frames(Bits, 0) -> {ok, Bits};
parse_frames(Bits, N) when N > 0 ->
    case parse_one_frame(Bits) of
        {ok, Next} -> parse_frames(Next, N-1);
        {error, _}=Error -> Error
    end.

parse_one_frame(
    <<MediaTimeFlag:1,
      _PTS32:32/big,
      _Confidence:8,
      _W0:8, _W1:8, _W2:8, _W3:8, _W4:8,
      _:608,
      Rest/bitstring>>
) when MediaTimeFlag =:= 1 ->
    {ok, Rest};
parse_one_frame(_Other) -> {error, truncated}.

parse_one_segment(
    <<StartFrame:32/big,
      EndFrame:32/big,
      MediaTimeFlag:1,
      StartMediaTime:32/big,
      EndMediaTime:32/big,
      Bits/bitstring>>
) when MediaTimeFlag =:= 1 ->
    case skip_bow_sets(Bits, 5) of
        {ok, After} ->
            {ok, #{start_frame => StartFrame,
                    end_frame => EndFrame,
                    start_media_time => StartMediaTime,
                    end_media_time => EndMediaTime}, After};
        {error, _}=Error -> Error
    end;
parse_one_segment(_Other) -> {error, unsupported_signature_format}.

skip_bow_sets(Bits, 0) -> {ok, Bits};
skip_bow_sets(Bits, K) when K > 0 ->
    case skip_bits(Bits, 243) of
        {ok, AfterOne} -> skip_bow_sets(AfterOne, K-1);
        {error, _}=Error -> Error
    end.

skip_bits(Bits, N) when is_integer(N), N >= 0 ->
    case Bits of
        <<_:N, Rest/bitstring>> -> {ok, Rest};
        _ -> {error, truncated}
    end.
   
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
    