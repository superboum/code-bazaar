import asyncio
import enum
import struct
from typing import Self, Sequence
from dataclasses import dataclass
from abc import abstractmethod, ABC


async def accept(reader, writer):
    addr = writer.get_extra_info('peername')
    print(f"connection accepted from {addr}")

    state = CoState.INITIAL
    while state is not CoState.ENDED:
        match state:
            case CoState.INITIAL:
                handshake = Handshake.deserialize(await read_len_prefixed_msg(reader))
                match handshake.inner:

            case CoState.LOGGED:
                raw_msg = await read_type_and_len_prefixed_msg(reader)
                print(raw_msg)
                state = CoState.ENDED
            case CoState.ENDED:
                break # redundant but for clarity
            case _:
                state = CoState.ENDED


    #writer.write(data)
    #await writer.drain()

    writer.close()
    await writer.wait_closed()


async def main():
    server = await asyncio.start_server(accept, '127.0.0.1', 5430)

    addrs = ', '.join(str(sock.getsockname()) for sock in server.sockets)
    print(f'Serving on {addrs}')

    async with server:
        await server.serve_forever()

asyncio.run(main())

