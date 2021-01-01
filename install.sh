curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

mkdir ~/.local/bin

curl -L https://github.com/rust-analyzer/rust-analyzer/releases/latest/download/rust-analyzer-linux -o ~/.local/bin/rust-analyzer

chmod +x ~/.local/bin/rust-analyzer

sudo apt install openocd
export PATH=/home/christopher/.local/bin:$PATH 

rustup default nightly
rustup target add riscv32imac-unknown-none-elf
cargo install cargo-binutils

sudo apt install git
sudo apt install python2
curl https://bootstrap.pypa.io/get-pip.py -o get-pip.py
python2 get-pip.py
python2 -m pip

sudo apt-get install build-essential zlib1g-dev pkg-config libglib2.0-dev binutils-dev libboost-all-dev autoconf libtool libssl-dev libpixman-1-dev libpython2-dev python-capstone virtualenv
sudo apt install autoconf automake autotools-dev curl libmpc-dev libmpfr-dev libgmp-dev \
                 gawk build-essential bison flex texinfo gperf libtool patchutils bc \
                 zlib1g-dev libexpat-dev git
cd ~/
git clone https://github.com/qemu/qemu
cd qemu
git checkout v5.0.0
./configure --target-list=riscv32-softmmu
make -j $(nproc)
sudo make install
export PATH=/home/christopher/qemu:$PATH
