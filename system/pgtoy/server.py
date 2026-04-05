import asyncio
from typing import assert_never

import dispatch.session as session
import dispatch.handshake as handshake
import dispatch.query as query
import msg.serializable as ser
import msg.all as msg
import parser.handshake as parse_handshake
import parser.query as parse_query

async def accept(reader: asyncio.StreamReader, writer: asyncio.StreamWriter) -> None:
    addr = writer.get_extra_info("peername")
    print(f"connection accepted from {addr}")

    msg_writer = ser.Writer(writer)
    cur_sess = session.Dispatcher()

    while cur_sess.is_connected():
        # PARSING (state dependant)
        match cur_sess.state:
            case handshake.Dispatcher():
                recv_msg: msg.FrontMsg = await parse_handshake.front(reader)
            case query.Dispatcher():
                recv_msg = await parse_query.front(reader)
            case session.Disconnected():
                break
            case _:
                assert_never(cur_sess.state)

        # DISPATCH
        to_send = cur_sess.register(recv_msg)

        # SERIALIZE
        for send_msg in to_send:
            send_msg.serialize(msg_writer)

        # FLUSH
        await writer.drain()

    writer.close()
    await writer.wait_closed()


async def main() -> None:
    server = await asyncio.start_server(accept, "127.0.0.1", 5430)

    addrs = ", ".join(str(sock.getsockname()) for sock in server.sockets)
    print(f"Serving on {addrs}")

    async with server:
        await server.serve_forever()


asyncio.run(main())
