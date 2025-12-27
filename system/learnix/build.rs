use std::path::Path;

static LINKER_SCRIPT_NAME: &str = "linker.ld";

fn main() {
    let workdir_path = Path::new(env!("CARGO_MANIFEST_DIR"));
    println!("cargo:rustc-link-arg-bins=-T{}", workdir_path.join(LINKER_SCRIPT_NAME).display());
}
