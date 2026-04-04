import enum
import struct
from typing import Self

async def proto_version(reader: io.Reader) -> ProtoVersion
    buf = await read_u32(reader)
    major, minor = struct.unpack("!hh", buf)
    try:
        return ProtoVersion((major, minor))
    except Exception:
        return ProtoVersion.Unknown

async def hfront(reader: io.Reader) -> HFrontMsg | None:
    mlen = await read_mlen(reader)
    proto = await proto_version(reader)
    match proto:
        case ProtoVersion.GSSENCRequest: 
            return GSSRequest()
        case ProtoVersion.SSLRequest: 
            return SSLRequest()
        case ProtoVersion.Postgres20:
            print("Protocol 2.0 is not supported.")
            return None
        case ProtoVersion.Postgres30 | ProtoVersion.Postgres32:
            return await startup_message(reader, mlen)
        case ProtoVersion.Unknown | _:
            print("Unknown protocol")
            return None

