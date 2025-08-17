Commandline of ffmpeg for snippet generation:

 ffmpeg -i sample.mp4 -map 0:v -vf signature=filename=snippet.sig -f null -

Equivalent vice invocation (Erlang):

```
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

 videdup helper
 
 The `videdup:write_signature/1` function runs ffmpeg via `vice` to generate a signature file next to the current working directory with the same basename as the input and a `.sig` extension.
 
 Example:
 
 ```
 1> {ok, Sig} = videdup:write_signature("sample.mp4").
 {ok,"./sample.sig"}
 ```
 
 Internally equivalent to:
 
 ```
 vice:convert(
   "sample.mp4",
   "-",
   [
     {map, "0:v"},
     {video_filtergraph, "signature=filename=./sample.sig"},
     {output_format, "null"}
   ],
   sync
 ).
 ```
 
 Production note:
 - Do not call `application:ensure_all_started(vice)` in production. `vice` is listed as an application dependency in `videdup.app.src` and included in the release via `relx` in `rebar.config`, so it will be started automatically by the release.

  Common Test conventions for this project:

  - Start long-running deps (like `vice`) in `init_per_suite/1` and stop them in `end_per_suite/1`.
  - Do not change directories within test cases; the default working directory is a per-test folder under the suite `priv_dir` and is suitable for emitting artifacts like `.sig` files.
  - Prefer cleanup in `end_per_testcase/2` (e.g., delete generated `*.sig` files) instead of `try ... after` in each test.
  - Use pattern matching for assertions where appropriate to allow tests to fail fast (e.g., `{ok, Path} = videdup:write_signature(Video)`).
