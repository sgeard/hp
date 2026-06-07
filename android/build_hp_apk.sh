#!/usr/bin/env bash
#
# build_hp_apk.sh - rebuild (and optionally install) the HP AndroWish APK.
#
# The APK is produced by repackaging a self-consistent 64-bit AndroWish build
# (decoded/ - an apktool tree already rebranded to com.simon.hp / "HP", trimmed
# to arm64-v8a). This script just refreshes the two app-specific inputs into
# that tree, then builds, zip-aligns and signs:
#
#   decoded/lib/arm64-v8a/libhp.so   <- the aarch64 calculator binary
#   decoded/assets/app/main.tcl      <- the Tk front-end (hp_tk.tcl)
#
# Usage:
#   ./build_hp_apk.sh [hp_exe] [main_tcl] [--install]
#
#   hp_exe     aarch64 calculator binary   (default: ../hp_android_exe)
#   main_tcl   Tk front-end script         (default: ../hp_tk.tcl)
#   --install  adb install -r the result onto a connected device
#
set -euo pipefail

here="$(cd "$(dirname "$0")" && pwd)"
proj="$(cd "$here/.." && pwd)"           # the hp project directory
decoded="$here/decoded"
apktool="$here/tools/apktool.jar"
bt="/home/simon/Android/Sdk/build-tools/35.0.0"
keystore="/home/simon/awsdk/keystore"

# Collect args: two optional positionals plus an --install flag anywhere.
install=0
positional=()
for a in "$@"; do
    if [ "$a" = "--install" ]; then install=1; else positional+=("$a"); fi
done
hp_exe="${positional[0]:-$proj/hp_android_exe}"
main_tcl="${positional[1]:-$proj/hp_tk.tcl}"

for f in "$hp_exe" "$main_tcl" "$apktool" "$keystore"; do
    [ -e "$f" ] || { echo "***Error: missing $f" >&2; exit 1; }
done

echo "Refreshing inputs into $decoded ..."
cp "$hp_exe"   "$decoded/lib/arm64-v8a/libhp.so"
cp "$main_tcl" "$decoded/assets/app/main.tcl"

echo "Building APK ..."
rm -f "$here/hp-unsigned.apk" "$here/hp-aligned.apk" "$here/hp.apk"
java -jar "$apktool" b "$decoded" -o "$here/hp-unsigned.apk"
"$bt/zipalign" -p -f 4 "$here/hp-unsigned.apk" "$here/hp-aligned.apk"
"$bt/apksigner" sign --ks "$keystore" --ks-pass pass:secret --key-pass pass:secret \
    --ks-key-alias alias --out "$here/hp.apk" "$here/hp-aligned.apk"
rm -f "$here/hp-unsigned.apk" "$here/hp-aligned.apk"
echo "Built $here/hp.apk"

if [ "$install" = 1 ]; then
    echo "Installing ..."
    adb install -r "$here/hp.apk"
fi
