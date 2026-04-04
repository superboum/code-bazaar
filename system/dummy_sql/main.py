import asyncio
import enum
import struct
from dataclasses import dataclass

class CoState(enum.Enum):
    INITIAL = 0x00
    LOGGED = 0x02
    ENDED = 0x03

class ProtoVersion(enum.Enum):
    GSSENCRequest = (1234, 5680)
    SSLRequest = (1234, 5679)
    Postgres20 = (2, 0)
    Postgres30 = (3, 0)
    Postgres32 = (3, 2)

class CoParams(enum.Enum):
    USER = b'user'
    DATABASE = b'database'
    OPTIONS = b'options'
    REPLICATION = b'replication'
    CLIENT_ENCODING = b'client_encoding'
    APPLICATION_NAME = b'application_name'

@dataclass
class SessionCtx:
    user: str|None = None
    database: str = "postgres"
    options: bytes|None = None
    replication: bytes|None = None
    client_encoding: str|None = None
    app_name: str|None = None

def connection_parameters(plain_data) -> SessionCtx:
    splitted = plain_data.split(b'\x00')
    ctx = SessionCtx()
    # @FIXME: maybe we should wait for client encoding before
    # decoding values...
    for k, v in zip(splitted[::2], splitted[1::2]):
        match k:
            case CoParams.USER.value:
                ctx.user = v.decode('utf-8')
            case CoParams.DATABASE.value:
                ctx.database = v.decode('utf-8')
            case CoParams.OPTIONS.value:
                ctx.options = v
            case CoParams.REPLICATION.value:
                ctx.replication = v
            case CoParams.CLIENT_ENCODING.value:
                ctx.client_encoding = v.decode('ascii')
            case CoParams.APPLICATION_NAME.value:
                ctx.app_name = v.decode('utf-8')
            case _:
                print(f"Skip unknown kv pair: {k.decode('utf-8')})")

    print(ctx)
    return ctx


async def handle_startup_message(reader, writer):
    MSG_LEN_SZ = 4
    VERSION_SZ = 4
    raw_len = await reader.read(MSG_LEN_SZ)
    (msg_len,) = struct.unpack("!i", raw_len)
    # include self
    msg_len -= MSG_LEN_SZ
    print("startup message. len:", msg_len)
    raw_msg = b''
    while len(raw_msg) < msg_len:
        raw_msg += await reader.read(msg_len - len(raw_msg))

    major, minor = struct.unpack("!hh", raw_msg[:VERSION_SZ])
    print(major, minor)
    match (major, minor):
        case ProtoVersion.GSSENCRequest.value: 
            writer.write(b'N')
            await writer.drain()
            return CoState.INITIAL, None
        case ProtoVersion.SSLRequest.value: 
            writer.write(b'N')
            await writer.drain()
            return CoState.INITIAL, None
        case ProtoVersion.Postgres20.value:
            print("Protocol 2.0 is not supported, closing co.")
            return CoState.ENDED, None
        case ProtoVersion.Postgres30.value:
            print("Protocol 3.0. Handling connection parameters.")
            ctx = connection_parameters(raw_msg[VERSION_SZ:])
            return CoState.LOGGED, ctx
        case ProtoVersion.Postgres32.value:
            print("Protocol 3.2. Handling connection parameters.")
            ctx = connection_parameters(raw_msg[VERSION_SZ:])
            return CoState.LOGGED, ctx
        case _:
            print("Unknown protocol version, closing co.")
            return CoState.ENDED, None


async def handle_normal_message(reader, writer):
    raise Exception("Not yet implemented")

async def accept(reader, writer):
    print("co accepted")
    state = CoState.INITIAL
    while state is not CoState.ENDED:
        match state:
            case CoState.INITIAL:
                state, ctx = await handle_startup_message(reader, writer)
            case CoState.LOGGED:
                state = await handle_normal_message(reader, writer)


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

