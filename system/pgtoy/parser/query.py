import asyncio

import error as error
import msg.query as mq
import msg.cmd_id as cmd_id
import parser.native as native


async def front(reader: asyncio.StreamReader) -> mq.FrontMsg:
    kind_raw = await native.read_u8(reader)
    _mlen = await native.read_mlen(reader)

    try:
        kind = cmd_id.FrontMsgType(kind_raw)
    except Exception:
        raise error.InvalidFrontMsgType()

    match kind:
        case cmd_id.FrontMsgType.QUERY:
            return mq.Query()
        case cmd_id.FrontMsgType.PARSE:
            return mq.Parse()
        case cmd_id.FrontMsgType.BIND:
            return mq.Bind()
        case cmd_id.FrontMsgType.CLOSE:
            return mq.Close()
        case cmd_id.FrontMsgType.DESCRIBE:
            return mq.Describe()
        case cmd_id.FrontMsgType.FLUSH:
            return mq.Flush()
        case cmd_id.FrontMsgType.SYNC:
            return mq.Sync()
        case cmd_id.FrontMsgType.EXECUTE:
            return mq.Execute()
        case cmd_id.FrontMsgType.TERMINATE:
            return mq.Terminate()
        case _:
            raise error.ParsingUnimplemented()
