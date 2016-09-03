#!/bin/sh

cli=/Applications/Seil.app/Contents/Library/bin/seil

$cli set enable_jis_kana 1
/bin/echo -n .
$cli set enable_jis_nfer 1
/bin/echo -n .
$cli set enable_jis_xfer 1
/bin/echo -n .
$cli set keycode_capslock 59
/bin/echo -n .
$cli set keycode_command_l 58
/bin/echo -n .
$cli set keycode_option_l 55
/bin/echo -n .
