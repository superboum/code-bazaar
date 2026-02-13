#![no_std]
#![no_main]

use core::arch::asm;

#[unsafe(no_mangle)]
fn main() {
    unsafe {
        asm!(
            "mov ah, 0x01",   // INT 10h function to setup the cursor
            "int 0x10",       // Call the bios interrupt function
            out("ax") _,      // Lock the 'ax' as output reg, so it won't be used elsewhere
        );
    }



    let msg = b"Hello, World!";
    for &ch in msg {
        unsafe {
            asm!(
                "mov ah, 0x0E",   // INT 10h function to print a char
                "mov al, {0}",    // The input ASCII char
                "int 0x10",       // Call the BIOS Interrupt Function
                                  // --- settings ---
                in(reg_byte) ch,  // {0} Will become the register with the char
                out("ax") _,      // Lock the 'ax' as output reg, so it won't be used elsewhere
            );
        }
    }

    unsafe {
        asm!("hlt"); // Halt the system
    }
}

#[panic_handler]
pub fn panic_handler(_: &core::panic::PanicInfo) -> ! {
    loop {}
}
