#![no_std]
#![no_main]

#[unsafe(no_mangle)]
fn main() {
}

#[panic_handler]
pub fn panic_handler(_: &core::panic::PanicInfo) -> ! {
  loop {}
}
