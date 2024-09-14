#!/bin/bash

PROGRAM_NAME="iolite"

# プラットフォームごとのバイナリ置き場を定義
if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    # Linux環境の場合
    DEST_DIR="$HOME/.local/bin"
elif [[ "$OSTYPE" == "darwin"* ]]; then
    # macOS環境の場合
    DEST_DIR="$HOME/.local/bin"
elif [[ "$OSTYPE" == "cygwin" || "$OSTYPE" == "msys" || "$OSTYPE" == "win32" ]]; then
    # Windows環境の場合 (Cygwin, Git Bash, または純粋なWindows)
    DEST_DIR="$USERPROFILE/bin"
else
    echo "このプラットフォームはサポートされていません。"
    exit 1
fi

# プロジェクトディレクトリを指定（相対パスまたは絶対パスで）
if [ -z "$1" ]; then
  PROJECT_DIR=$(pwd)
else
  PROJECT_DIR="$1"
fi

# プロジェクトディレクトリに移動
cd "$PROJECT_DIR" || { echo "指定したプロジェクトディレクトリが見つかりません: $PROJECT_DIR"; exit 1; }

# バージョン情報を取得 (Cargo.tomlのversionに従う)
VERSION=$(cargo run version)

if [ -z "$VERSION" ]; then
  echo "バージョン情報が取得できませんでした。"
  exit 1
fi

# Cargo build --release を実行
echo "Building the project in release mode..."

if ! cargo build --release
then
  echo "Build failed."
  exit 1
fi

# ビルドで生成されたバイナリを取得
BINARY_PATH="./target/release/$PROGRAM_NAME"

if [ -z "$BINARY_PATH" ]; then
  echo "ビルド後のバイナリが見つかりません。"
  exit 1
fi

# バイナリを配置するディレクトリ [PROJECT_DIR]/bin を作成
BIN_DIR="$PROJECT_DIR/bin"
if [ ! -d "$BIN_DIR" ]; then
  echo "ディレクトリ $BIN_DIR を作成します。"
  mkdir -p "$BIN_DIR"
fi

# バイナリのコピー先を設定
NEW_BINARY_PATH="$BIN_DIR/${PROGRAM_NAME}-${VERSION}"

# バイナリをコピー
cp "$BINARY_PATH" "$NEW_BINARY_PATH"
echo "バイナリを $NEW_BINARY_PATH にコピーしました。"

# シンボリックリンクの作成先を確認
if [ ! -d "$DEST_DIR" ]; then
  echo "ディレクトリ $DEST_DIR が存在しないため、作成します。"
  mkdir -p "$DEST_DIR"
fi

# ~/.local/bin/[BINARY] のシンボリックリンクを作成
ln -sf "$NEW_BINARY_PATH" "$DEST_DIR/$PROGRAM_NAME"

echo "シンボリックリンクを $DEST_DIR/$PROGRAM_NAME に作成しました。"
