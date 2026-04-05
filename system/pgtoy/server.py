import asyncio
import enum
import struct
from typing import Self, Sequence
from dataclasses import dataclass
from abc import abstractmethod, ABC


async def accept(reader, writer):
    addr = writer.get_extra_info('peername')
    print(f"connection accepted from {addr}")

    session 

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

