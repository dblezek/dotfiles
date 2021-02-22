


function expand {
  for f in "$@"; do
    if [[ -d "$f" ]]; then
      find "$f" -type f -print0
    else
      printf "%s\0" "$f"
    fi
  done
}

function expand_test {
  expand "$@" | while read -d $'\0' fn; do
    echo "$fn"
  done
}

function dicom_to_png {
  expand "$@" | while read -d $'\0' f; do
    /usr/local/bin/dcm2pnm --write-png "$f" "${f/.dcm/.png}"
  done
}

function dicom_decompress {
  expand "$@" | while read -d $'\0' f; do
    dcmdjpeg "$f" "$f.uncompressed"
    mv -f "$f.uncompressed" "$f"
  done
  
}

export -f expand expand_test dicom_to_png dicom_decompress
