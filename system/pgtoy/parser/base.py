U32_SZ = 4

async def read_many(reader: io.Reader, count: int) -> bytes:
    buf = b''
    while count - len(buf) > 0:
        buf += await reader.read(count - len(buf))
    return buf

async def read_u8(reader: io.Reader) -> bytes:
    return await reader.read(1)

async def read_u32(reader: io.Reader) -> int:
    buf = await read_many(reader, U32_SZ)
    (v,) = struct.unpack("!I", buf)
    return v

async def read_mlen(reader: io.Reader) -> int
    return (await read_u32(reader)) - U32_SZ
