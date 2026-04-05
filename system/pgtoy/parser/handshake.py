import asyncio
from typing import assert_never

import error as error
import msg.proto_version as mproto
import msg.handshake as mhs
import msg.serializable as ser
import parser.native as native
import parser.startup_message as parse_next


async def proto_version(reader: asyncio.StreamReader) -> mproto.ProtoVersion:
    major = await native.read_u16(reader)
    minor = await native.read_u16(reader)
    try:
        return mproto.ProtoVersion((major, minor))
    except Exception:
        return mproto.ProtoVersion.Unknown


async def front(reader: asyncio.StreamReader) -> mhs.FrontMsg:
    mlen = await native.read_mlen(reader)
    proto = await proto_version(reader)
    mlen -= ser.Sz.U32.value

    match proto:
        case mproto.ProtoVersion.GSSENCRequest:
            return mhs.GSSRequest()
        case mproto.ProtoVersion.SSLRequest:
            return mhs.SSLRequest()
        case mproto.ProtoVersion.Postgres20:
            raise error.Protocolv2NotSupported()
        case mproto.ProtoVersion.Postgres30 | mproto.ProtoVersion.Postgres32:
            return await parse_next.startup_message(reader, mlen)
        case mproto.ProtoVersion.Unknown:
            raise error.UnknownPostgresProtocolVersion()
        case _:
            assert_never(proto)
