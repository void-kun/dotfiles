# download emacs 
cd ~/Downloads
curl -LO https://mirror.freedif.org/GNU/emacs/emacs-29.3.tar.xz

# install dependencies
sudo apt build-dep emacs -y
sudo apt install -y libtree-sitter0 libtree-sitter-dev

cd ./emacs-29.3

./autogen.sh
./configure --with-native-compilation=aot --with-x-toolkit=gtk3 --with-tree-sitter --with-json --with-threads --with-x --without-mailutils --prefix="/home/zrik/.local/emacs"

mkdir -p ~/.local/bin/

ln -s ~/.local/emacs/bin/emacs-29.3 ~/.local/bin/emacs
ln -s ~/.local/emacs/bin/emacsclient ~/.local/bin/emacsclient

mkdir -p ~/.local/share/applications 
cp -rf ~/.local/emacs/share/applications/* ~/.local/share/applications/ 

