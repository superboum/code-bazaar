# [Learnix](https://www.learnix-os.com)

Build:

```bash
cargo build --release --target ./16bit_target.json -Z build-std=core
```

Run:

```bash
qemu-system-x86_64 -drive format=raw,file=target/16bit_target/release/learnix
```
