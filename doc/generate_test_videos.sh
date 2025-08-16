#!/usr/bin/env bash
set -euo pipefail

# -------- defaults --------
COUNT=${count:-5}
DURATION=${duration_s:-3}
SIZE=${size:-640x360}
FPS=${fps:-10}
CODEC=${codec:-h264}
CRF=${crf:-34}
BG_COLOR=${bg_color:-#202020}
LABEL_PREFIX=${label_prefix:-Test}
COUNTER=${counter:-time}         # time | frame
AUDIO=${audio:-off}              # on | off
OUT_DIR=${out_dir:-./test_videos}
FONT_MAIN=${font_main:-/Library/Fonts/Arial.ttf}
FONT_MONO=${font_mono:-/System/Library/Fonts/Menlo.ttc}
BACKGROUND_IMAGE=${background_image:-}
NAME_PATTERN=${name_pattern:-}

mkdir -p "$OUT_DIR"

need() { command -v "$1" >/dev/null 2>&1 || { echo "$1 not found."; return 1; }; }
if ! need ffmpeg || ! need ffprobe; then
  echo "Install FFmpeg with: brew install ffmpeg" >&2
  exit 1
fi

# font fallbacks
[ -f "$FONT_MAIN" ] || FONT_MAIN="/System/Library/Fonts/Supplemental/Arial Unicode.ttf"
[ -f "$FONT_MONO" ] || FONT_MONO="$FONT_MAIN"

# codec flags
VFLAGS=""
case "$CODEC" in
  h264) VFLAGS="-c:v libx264 -profile:v baseline -pix_fmt yuv420p -crf ${CRF} -preset veryslow" ;;
  hevc|h265) VFLAGS="-c:v libx265 -pix_fmt yuv420p -crf ${CRF} -preset veryslow -tag:v hvc1" ;;
  av1)  VFLAGS="-c:v libaom-av1 -pix_fmt yuv420p -crf 40 -b:v 0 -cpu-used 4" ;;
  *) echo "Unknown codec: $CODEC" >&2; exit 2 ;;
esac

COUNTER_TEXT="%{pts\\:hms}"
[ "$COUNTER" = "frame" ] && COUNTER_TEXT="%{n}"

manifest_entries=()

for ((i=1; i<=COUNT; i++)); do
  if [ -n "$NAME_PATTERN" ]; then
    OUT_BASENAME=$(eval echo "$NAME_PATTERN")
  else
    OUT_BASENAME="test_${i}_${SIZE}_${FPS}fps_${DURATION}s.mp4"
  fi
  OUTFILE="${OUT_DIR}/${OUT_BASENAME}"
  LABEL_TEXT="${LABEL_PREFIX} ${i}"

  if [ -n "$BACKGROUND_IMAGE" ]; then
    # image background
    ffmpeg -hide_banner -loglevel error -y \
      -loop 1 -t "$DURATION" -i "$BACKGROUND_IMAGE" \
      -vf "scale=${SIZE%x*}:-2:force_original_aspect_ratio=decrease,\
           pad=${SIZE}:(ow-iw)/2:(oh-ih)/2,fps=${FPS},\
           drawtext=fontfile='${FONT_MAIN}':fontsize=48:fontcolor=white:text='${LABEL_TEXT}':x=(w-text_w)/2:y=(h-text_h)/2,\
           drawtext=fontfile='${FONT_MONO}':fontsize=26:fontcolor=yellow:text='${COUNTER_TEXT}':x=w-text_w-20:y=h-text_h-20" \
      $VFLAGS -r "$FPS" -g "$FPS" -movflags +faststart "$OUTFILE"
  else
    # solid color background, optional audio
    if [ "$AUDIO" = "on" ]; then
      ffmpeg -hide_banner -loglevel error -y \
        -f lavfi -i "color=c=${BG_COLOR}:s=${SIZE}:d=${DURATION}" \
        -f lavfi -t "$DURATION" -i "anullsrc=r=48000:cl=mono" \
        -shortest \
        -vf "drawtext=fontfile='${FONT_MAIN}':fontsize=48:fontcolor=white:text='${LABEL_TEXT}':x=(w-text_w)/2:y=(h-text_h)/2,\
             drawtext=fontfile='${FONT_MONO}':fontsize=26:fontcolor=yellow:text='${COUNTER_TEXT}':x=w-text_w-20:y=h-text_h-20" \
        $VFLAGS -r "$FPS" -g "$FPS" -movflags +faststart -c:a aac -b:a 48k "$OUTFILE"
    else
      ffmpeg -hide_banner -loglevel error -y \
        -f lavfi -i "color=c=${BG_COLOR}:s=${SIZE}:d=${DURATION}" \
        -vf "drawtext=fontfile='${FONT_MAIN}':fontsize=48:fontcolor=white:text='${LABEL_TEXT}':x=(w-text_w)/2:y=(h-text_h)/2,\
             drawtext=fontfile='${FONT_MONO}':fontsize=26:fontcolor=yellow:text='${COUNTER_TEXT}':x=w-text_w-20:y=h-text_h-20" \
        $VFLAGS -r "$FPS" -g "$FPS" -movflags +faststart "$OUTFILE"
    fi
  fi

  # gather metadata
  meta=$(ffprobe -v error -select_streams v:0 \
         -show_entries stream=width,height,avg_frame_rate,pix_fmt,codec_name,duration \
         -show_entries format=size,format_name,duration \
         -of json "$OUTFILE")
  size_bytes=$(echo "$meta" | grep -o '"size":"[^"]*' | grep -o '[0-9]\+')
  codec=$(echo "$meta" | grep -o '"codec_name":"[^"]*' | head -1 | cut -d\" -f4)
  pix_fmt=$(echo "$meta" | grep -o '"pix_fmt":"[^"]*' | head -1 | cut -d\" -f4)
  fps=$(echo "$meta" | grep -o '"avg_frame_rate":"[^"]*' | head -1 | cut -d\" -f4)
  width=$(echo "$meta" | grep -o '"width":[0-9]\+' | head -1 | cut -d: -f2)
  height=$(echo "$meta" | grep -o '"height":[0-9]\+' | head -1 | cut -d: -f2)
  has_audio="false"
  ffprobe -v error -select_streams a:0 -show_streams "$OUTFILE" >/dev/null 2>&1 && has_audio="true"

  manifest_entries+=("{\"filename\":\"$(basename "$OUTFILE")\",\"label\":\"$LABEL_TEXT\",\"width\":$width,\"height\":$height,\"fps\":\"$fps\",\"duration_s\":$DURATION,\"codec\":\"$codec\",\"pix_fmt\":\"$pix_fmt\",\"size_bytes\":$size_bytes,\"has_audio\":$has_audio}")
  echo "$(basename "$OUTFILE")  —  $size_bytes bytes"
done

# write manifest.json
printf "[\n  %s\n]\n" "$(IFS=$',\n  '; echo "${manifest_entries[*]}")" > "${OUT_DIR}/manifest.json"
echo "Wrote manifest: ${OUT_DIR}/manifest.json"


