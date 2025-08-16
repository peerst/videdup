Commandline of ffmpeg for snippet generation:

 ffmpeg -i sample.mp4 -map 0:v -vf signature=filename=snippet.sig -f null -

Equivalent vice invocation (Erlang):

```
ok = application:ensure_all_started(vice),
vice:convert(
  "sample.mp4",
  "-",
  [
    {map, "0:v"},
    {video_filtergraph, "signature=filename=test/snippet.sig"},
    {output_format, "null"}
  ],
  sync
).
```

Notes:
- `{video_filtergraph, ...}` maps to ffmpeg `-vf ...`.
- `{map, "0:v"}` maps to ffmpeg `-map 0:v`.
- `{output_format, "null"}` with output `"-"` maps to ffmpeg `-f null -`.
