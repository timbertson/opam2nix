set -ex

PKG_CONFIG_DEPS="gmp-xen mirage-xen-posix"
function check_deps {
	pkg-config --print-errors --exists ${PKG_CONFIG_DEPS}
}
check_deps || exit 1

LDFLAGS=`pkg-config --libs gmp-xen`
export LDFLAGS

# WARNING: if you pass invalid cflags here, zarith will silently
# fall back to compiling with the default flags instead!
CFLAGS="`pkg-config --cflags gmp-xen mirage-xen-posix` -O2 -pedantic -fomit-frame-pointer -fno-builtin"
export CFLAGS
./configure
make
ZARITH_LIB=`ocamlfind query zarith`
dest="$out/lib/zarith-xen"
cp -a "$ZARITH_LIB" "$dest/"
chmod -R u+w "$dest"
cp libzarith.a "$dest/libzarith-xen.a"
cp META "$dest/META"
