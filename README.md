## Test video generator

See `doc/generate_test_videos.sh` for a ready-to-use script to create small labeled MP4 test videos on macOS using FFmpeg. It writes files and a manifest.json to a target directory.

### Common Test suite and sample data

- Suite: `test/videdup_SUITE.erl`
- Sample videos directory: `test/videdup_SUITE_data/`
  - Contains three pre-generated samples: `t_1.mp4`, `t_2.mp4`, `t_3.mp4` (15s, 320x180) labeled "Test 1/2/3" with a running time counter.

Run tests:

```
rebar3 ct
```

Regenerate videos (optional example):

```
BG_COLOR=#202020 count=3 duration_s=15 size=320x180 fps=10 label_prefix=Test counter=time \
  out_dir=test/videdup_SUITE_data name_pattern='t_${i}.mp4' \
  ./doc/generate_test_videos.sh
```

videdup
=====

An OTP application

Build
-----

    $ rebar3 compile
