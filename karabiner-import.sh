#!/bin/sh

cli=/Applications/Karabiner.app/Contents/Library/bin/karabiner

$cli set general.dont_remap_apple_keyboard 1
/bin/echo -n .
$cli set general.dont_remap_apple_pointing 1
/bin/echo -n .
$cli set general.dont_remap_internal 1
/bin/echo -n .
$cli set remap.commandL2optionL 1
/bin/echo -n .
$cli set remap.fkeys_to_consumer_f1 1
/bin/echo -n .
$cli set remap.fkeys_to_consumer_f10 1
/bin/echo -n .
$cli set remap.fkeys_to_consumer_f3_lion 1
/bin/echo -n .
$cli set remap.optionL2commandL 1
/bin/echo -n .
$cli set remap.optionrcommandr 1
/bin/echo -n .
$cli set remap.pc_application2rightclick 1
/bin/echo -n .
/bin/echo
