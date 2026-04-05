import asyncio
import logging
from typing import assert_never

import dispatch.session as session
import dispatch.handshake as handshake
import dispatch.query as query
import msg.serializable as ser
import msg.all as msg
import parser.handshake as parse_handshake
import parser.query as parse_query
import persist.inmem as persist

logger = logging.getLogger(__name__)


class PgToy:
    host: str
    port: int
    inst: persist.Instance

    def __init__(self, host: str | None = None, port: int | None = None):
        if host is None:
            host = "::1"
        if port is None:
            port = 5430
        self.host = host
        self.port = port
        self.inst = persist.Instance()

    async def listen(self) -> None:
        server = await asyncio.start_server(self.accept, self.host, self.port)
        addrs = ", ".join(str(sock.getsockname()) for sock in server.sockets)
        logger.info(f"Serving on {addrs}")
        async with server:
            await server.serve_forever()

    async def accept(
        self, reader: asyncio.StreamReader, writer: asyncio.StreamWriter
    ) -> None:
        addr = writer.get_extra_info("peername")
        logger.info(f"connection accepted from {addr}")

        msg_writer = ser.Writer(writer)
        cur_sess = session.Dispatcher(self.inst)

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
            logger.debug("Received message: %s", recv_msg)

            # DISPATCH
            to_send = cur_sess.register(recv_msg)

            # SERIALIZE
            for send_msg in to_send:
                logger.debug("Send message: %s", send_msg)
                send_msg.serialize(msg_writer)

            # FLUSH
            await writer.drain()

        logger.info(f"{addr} disconnected")
        writer.close()
        await writer.wait_closed()
