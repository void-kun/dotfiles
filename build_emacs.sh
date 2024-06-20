# download emacs 
cd ~/Downloads
curl -LO https://mirror.freedif.org/GNU/emacs/emacs-29.3.tar.xz

# install dependencies
sudo apt install -y autoconf automake autopoint autotools-dev dbus-x11 debhelper dh-autoreconf dh-strip-nondeterminism diffstat dwz ed exim4-base exim4-config exim4-daemon-light  gettext gir1.2-atspi-2.0 gir1.2-rsvg-2.0 intltool-debian libacl1-dev libarchive-zip-perl libasound2-dev libatk-bridge2.0-dev libatk1.0-dev libatspi2.0-dev libattr1-dev  libblkid-dev libcairo-script-interpreter2 libcairo2-dev libdatrie-dev libdbus-1-dev libdebhelper-perl libdeflate-dev libegl-dev libegl1-mesa-dev libepoxy-dev  libfile-stripnondeterminism-perl libfribidi-dev libgccjit-12-dev libgccjit0 libgdk-pixbuf-2.0-dev libgif-dev libgl-dev libgles-dev libgles1 libglib2.0-bin libglib2.0-dev  libglib2.0-dev-bin libglvnd-core-dev libglvnd-dev libglx-dev libgmp-dev libgmpxx4ldbl libgnutls-dane0 libgnutls-openssl27 libgnutls28-dev libgnutlsxx30 libgpm-dev  libgraphite2-dev libgtk-3-dev libharfbuzz-dev libharfbuzz-gobject0 libice-dev libidn2-dev libjansson-dev libjbig-dev libjpeg-dev libjpeg62-turbo-dev liblcms2-dev  liblerc-dev liblockfile-dev liblockfile1 liblzma-dev liblzo2-2 libm17n-0 libm17n-dev libmount-dev libopengl-dev libopengl0 libotf-dev libotf1 libp11-kit-dev  libpango1.0-dev libpcre2-32-0 libpcre2-dev libpcre2-posix3 libpixman-1-dev libpthread-stubs0-dev librsvg2-dev libselinux1-dev libsepol-dev libsm-dev libsub-override-perl  libsystemd-dev libtasn1-6-dev libtext-unidecode-perl libthai-dev libtiff-dev libtiffxx6 libtool libunbound8 libwayland-bin libwayland-dev libwebp-dev libx11-dev  libxau-dev libxaw7-dev libxcb-render0-dev libxcb-shm0-dev libxcb1-dev libxcomposite-dev libxcursor-dev libxdamage-dev libxdmcp-dev libxext-dev libxfixes-dev libxft-dev  libxi-dev libxinerama-dev libxkbcommon-dev libxml-libxml-perl libxml-namespacesupport-perl libxml-sax-base-perl libxml-sax-perl libxmu-dev libxmu-headers libxpm-dev  libxrandr-dev libxrender-dev libxt-dev libxtst-dev libzstd-dev m17n-db m4 nettle-dev pango1.0-tools po-debconf quilt sharutils tex-common texinfo wayland-protocols  x11proto-core-dev x11proto-dev xaw3dg xaw3dg-dev xorg-sgml-doctools xtrans-dev xutils-dev
sudo apt install -y libtree-sitter0 libtree-sitter-dev

tar -xf emacs-29.3.tar.xz
cd ./emacs-29.3

./autogen.sh
./configure --with-native-compilation=aot --with-x-toolkit=gtk3 --with-tree-sitter --with-json --with-threads --with-x --without-mailutils --prefix="/home/zrik/.local/emacs"
make -j 16
sudo make install -j 16

mkdir -p ~/.local/bin/

ln -s ~/.local/emacs/bin/emacs-29.3 ~/.local/bin/emacs
ln -s ~/.local/emacs/bin/emacsclient ~/.local/bin/emacsclient

mkdir -p ~/.local/share/applications 
cp -rf ~/.local/emacs/share/applications/* ~/.local/share/applications/ 

