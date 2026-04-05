import asyncio
import enum
from typing import Self, assert_never

import msg.proto_version as mproto
import msg.handshake as mhs
import parser.native as native
import parser.startup_message as parse_next

async def proto_version(reader: asyncio.StreamReader) -> mproto.ProtoVersion:
    major = await native.read_u16(reader)
    minor = await native.read_u16(reader)
    try:
        return mproto.ProtoVersion((major, minor))
    except Exception:
        return mproto.ProtoVersion.Unknown

async def hfront(reader: asyncio.StreamReader) -> mhs.FrontMsg | None:
    mlen = await native.read_mlen(reader)
    proto = await proto_version(reader)
    match proto:
        case mproto.ProtoVersion.GSSENCRequest: 
            return mhs.GSSRequest()
        case mproto.ProtoVersion.SSLRequest: 
            return mhs.SSLRequest()
        case mproto.ProtoVersion.Postgres20:
            print("Protocol 2.0 is not supported.")
            return None
        case mproto.ProtoVersion.Postgres30 | mproto.ProtoVersion.Postgres32:
            return await parse_next.startup_message(reader, mlen)
        case mproto.ProtoVersion.Unknown:
            print("Unknown protocol")
            return None
    assert_never()

