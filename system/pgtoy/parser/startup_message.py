import asyncio

import parser.native as native
import msg.startup_msg as msg

async def startup_message(reader: asyncio.StreamReader, mlen: int) -> msg.StartupMessage:
    buf = await native.read_many(reader, mlen)
    res = msg.StartupMessage(params=dict())
    splitted = buf.split(b'\x00')
    for k, v in zip(splitted[::2], splitted[1::2]):
        try:
            res.params[msg.Params(k)] = v
        except Exception as e:
            print(f"Skip unknown kv pair: {k!r}")

    return res
