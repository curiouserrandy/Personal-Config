case $config_host in
    astibar)
	RANDY_DB_PLUGIN=/usr/local/google/home/rdsmith/Sandboxen/3chrome-newgit/src/third_party/llvm-build/Release+Asserts/lib/libOutputDB.so
	DB_OUTPUT_FILE=/usr/local/google/home/rdsmith/Sandboxen/3chrome-newgit/src/clang-db
	;;
    rdsmith-macbookpro)
	RANDY_DB_PLUGIN=/Users/rdsmith/Sandboxen/chrome/src/third_party/llvm-build/Release+Asserts/lib/libOutputDB.dylib
	DB_OUTPUT_FILE=/Users/rdsmith/Sandboxen/chrome/src/clang-db
	;;
esac

if [ ! -z "$RANDY_DB_PLUGIN" ]; then
    add_plugin() {
      add_gyp clang=1
      add_gyp clang_load=$RANDY_DB_PLUGIN
      add_gyp clang_add_plugin='"complete -Xclang -plugin-arg-complete -Xclang --db='$DB_OUTPUT_FILE'"'
    }
fi
