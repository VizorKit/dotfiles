curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

mkdir ~/.local/bin

curl -L https://github.com/rust-analyzer/rust-analyzer/releases/latest/download/rust-analyzer-linux -o ~/.local/bin/rust-analyzer

chmod +x ~/.local/bin/rust-analyzer

sudo apt install openocd

rustup default nightly
rustup target add riscv32-unknown-none-elf
cargo install cargo-binutils
