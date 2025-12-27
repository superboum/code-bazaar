SECTIONS {

    /*
      Make the start offset of the file 0x7c00 This is useful,
      because if make jump to a function that it's offset in the binary is 0x100,
      it will actually be loaded at address 0x7d00 by the BIOS, and not 0x100,
      so we need to consider this offset, and that's how we do it.
    */
    . = 0x7c00;

    /*
      Rust also mangles segment names.
      The "<segment_name>.*" syntax is used to also include all the mangles
    */

    .text : { *(.text .text.*) }
    .bss : { *(.bss .bss.*) }
    .rodata : { *(.rodata .rodata.*) }
    .data : { *(.data .data.*) }
    /DISCARD/ : {
        *(.eh_frame .eh_frame.*)
        *(.eh_frame_hdr .eh_frame_hdr.*)
    }

    . = 0x7c00 + 510;

    /* Write the boot signature to make the sector bootable */
    .magic_number : { SHORT(0xaa55) }
}

OUTPUT_FORMAT(binary)
ENTRY(main)
