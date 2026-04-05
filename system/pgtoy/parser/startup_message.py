import asyncio

import parser.native as native
import msg.startup_msg as msg
import msg.proto_version as pver


async def startup_message(
    reader: asyncio.StreamReader, proto: pver.ProtoVersion, mlen: int
) -> msg.StartupMessage:
    buf = await native.read_many(reader, mlen)
    res = msg.StartupMessage(proto=proto, params=dict())
    splitted = buf.split(b"\x00")
    for k, v in zip(splitted[::2], splitted[1::2]):
        try:
            res.params[msg.Params(k)] = v
        except Exception:
            print(f"Skip unknown kv pair: {k!r}")

    return res
