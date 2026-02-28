#!/bin/bash
# Install flutter
source ./bin/detectos.sh

# ref. https://qiita.com/tomy0610/items/896dc8ec9ba95c33194f
if [ ! "$OSDIST" = "macos" ]; then
    echo "Not supported OS."
    exit 1
fi
if ! type gem >/dev/null 2>&1; then
    echo "Run ruby.sh first to install gem for CocoaPods"
    exit 1
fi
if ! type flutter >/dev/null 2>&1; then
    echo "Downloading flutter..."
    cd
    git clone https://github.com/flutter/flutter.git -b stable
    export PATH="$PATH:$HOME/flutter/bin"
    #    curl -L https://storage.googleapis.com/flutter_infra/releases/stable/macos/flutter_macos_1.22.6-stable.zip -o ~/flutter.zip
    #   mkdir -p $DESTDIR
    #    cd $DESTDIR
    #    unzip ~/flutter.zip
    #DESTDIR="$HOME/development"
    #export PATH="$PATH:$DESTDIR/flutter/bin"
    #rm ~/flutter.zip
fi

# ref https://flutter.dev/docs/get-started/install/macos
echo "Installing Xcode ..."
echo "If it does not work, download Command Line Tools first from: https://developer.apple.com/download/more/"
sudo xcode-select --switch /Applications/Xcode.app/Contents/Developer
sudo xcodebuild -runFirstLaunch

echo "Installing CocoaPods for Xcode ..."
# ref https://qiita.com/ShinokiRyosei/items/3090290cb72434852460
sudo gem install cocoapods
pod setup

~/flutter/bin/flutter precache

# Change channel to beta is required, otherwise flutter doctor does not detect Android Studio plugin.
# ref https://stackoverflow.com/questions/52336654/android-studio-flutter-and-dart-plugins-not-recognized-by-flutter-doctor-but-p
flutter channel beta

# ref https://stackoverflow.com/questions/61993738/flutter-doctor-android-licenses-gives-a-java-error
echo "Also Install Android SDK Command-line tools from Android Studio."

~/flutter/bin/flutter doctor
