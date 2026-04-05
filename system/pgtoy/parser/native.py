import asyncio
import struct
from mypy_extensions import i16, i32, i64

from msg.serializable import Sz

async def read_many(reader: asyncio.StreamReader, count: int) -> bytes:
    buf = b''
    while count - len(buf) > 0:
        buf += await reader.read(count - len(buf))
    return buf

async def read_u8(reader: asyncio.StreamReader) -> bytes:
    return await reader.read(1)

async def read_u16(reader: asyncio.StreamReader) -> i32:
    buf = await read_many(reader, Sz.U16.value)
    (v,) = struct.unpack("!H", buf)
    assert isinstance(v, int)
    return i32(v)

async def read_u32(reader: asyncio.StreamReader) -> i64:
    buf = await read_many(reader, Sz.U32.value)
    (v,) = struct.unpack("!I", buf)
    assert isinstance(v, int)
    return i64(v)

async def read_mlen(reader: asyncio.StreamReader) -> i64:
    return (await read_u32(reader)) - Sz.U32.value
