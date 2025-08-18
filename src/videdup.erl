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
            num_segments => NumSegments},
    {ok, Map};
parse_signature_header(_Other) ->
    {error, unsupported_signature_format}.
   
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
    