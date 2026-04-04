import asyncio
import enum
import struct
from typing import Self, Sequence
from dataclasses import dataclass
from abc import abstractmethod, ABC


# --- MSG READ UTILS ---
async def read_msg_len(reader) -> int:
    MSG_LEN_SZ = 4
    raw_len = await reader.read(MSG_LEN_SZ)
    (msg_len,) = struct.unpack("!I", raw_len)
    # include self
    msg_len -= MSG_LEN_SZ
    return msg_len

async def read_msg_type(reader) -> bytes:
    MSG_TYPE_SZ = 1
    return await reader.read(MSG_TYPE_SZ)

async def read_len_prefixed_msg(reader) -> bytes:
    msg_len = await read_msg_len(reader)
    raw_msg = b''
    while len(raw_msg) < msg_len:
        raw_msg += await reader.read(msg_len - len(raw_msg))
    return raw_msg

async def read_type_and_len_prefixed_msg(reader) -> tuple[bytes, bytes]:
    msg_type = await read_msg_type(reader)
    raw_msg = await read_len_prefixed_msg(reader)
    return msg_type, raw_msg


# --- End of read utils ---

# --- Message definition ---
class Serializable(ABC):
    def serialize(self) -> bytes:
        pass


class ProtoVersion(enum.Enum):
    Unknown = (0, 0)
    GSSENCRequest = (1234, 5680)
    SSLRequest = (1234, 5679)
    Postgres20 = (2, 0)
    Postgres30 = (3, 0)
    Postgres32 = (3, 2)

    @staticmethod
    def deserialize(raw_msg: bytes) -> Self:
        VERSION_SZ = 4
        major, minor = struct.unpack("!hh", raw_msg[:VERSION_SZ])
        try:
            return ProtoVersion((major, minor))
        except Exception:
            return ProtoVersion.Unknown

class CoParams(enum.Enum):
    USER = b'user'
    DATABASE = b'database'
    OPTIONS = b'options'
    REPLICATION = b'replication'
    CLIENT_ENCODING = b'client_encoding'
    APPLICATION_NAME = b'application_name'
    END_OF_PARAMS = b''

class BackMsgType(enum.Enum):
    AUTHENTICATION = b'R' # multiple alternatives in context
    BACKEND_KEY_DATA = b'K'
    BIND_COMPLETE = b'2'
    COMMAND_COMPLETE = b'3' # also CLOSE_COMPLETE
    COPY_DATA = b'd'
    COPY_DONE = b'c'
    COPY_IN_RESPONSE = b'G'
    COPY_OUT_RESPONSE = b'H'
    COPY_BOTH_RESPONSE = b'W'
    DATA_ROW = b'D'
    EMPTY_RESPONSE = b'E'
    FUNCTION_CALL_RESPONSE = b'V'
    NEGOTIATE_PROTOCOL_VERSIOn = b'v'
    NO_DATA = b'n'
    NOTICE_RESPONSE = b'N'
    NOTIFICATION_RESPONSE = b'A'
    PARAMETER_DESCRIPTION = b'B'
    PARAMETER_STATUS = b'S'
    PARSE_COMPLETE = b'1'
    PORTAL_SUSPENDED = b's'
    READY_FOR_QUERY = b'Z'
    ROW_DESCRIPTION = b'T'

class FrontMsgType(enum.Enum):
    BIND = b'B'
    CLOSE = b'C'
    COPY_DATA = b'd'
    COPY_DONE = b'c'
    COPY_FAIL = b'f'
    DESCRIBE = b'D'
    EXECUTE = b'E'
    FLUSH = b'H'
    FUNCTION_CALL = b'F'
    PARSE = b'P'
    PASSWORD_RESPONSE = b'p'
    QUERY = b'Q'
    SYNC = b'S'
    TERMINATE = b'X'

@dataclass
class StartupMessage:
    params: dict[CoParams, bytes]

    @staticmethod
    def deserialize(plain_data: bytes) -> Self:
        res = StartupMessage(params=dict())
        splitted = plain_data.split(b'\x00')
        for k, v in zip(splitted[::2], splitted[1::2]):
            try:
                res.params[CoParams(k)] = v
            except Exception as e:
                print(f"Skip unknown kv pair: {k}")
        return res

class GSSRequest:
    pass

class SSLRequest:
    pass

class InvalidHandshake:
    pass

class RefuseHandshakeMsg(Serializable):
    def serialize(self) -> bytes:
        return b'N'

class AuthenticationOk(Serializable):
    msg_type = BackMsgType.AUTHENTICATION
    static_len = 8
    success_val = 0

    def serialize(self) -> bytes:
        # msg_type = 1 byte
        # len = 4 bytes
        # success_val = 4 bytes
        return struct.pack(
            "!cII", 
            self.msg_type.value,
            self.static_len,
            self.success_val,
        )

class NoticeSeverity(enum.Enum):
    WARNING = b'WARNING'
    NOTICE = b'NOTICE'
    DEBUG = b'DEBUG'
    INFO = b'INFO'
    LOG = b'LOG'

class ErrorCode(enum.Enum):
    # see: https://www.postgresql.org/docs/current/errcodes-appendix.html
    SUCCESSFUL_COMPLETION = b'00000'
    WARNING = b'00001'
    # ...

class NoticeField(enum.Enum):
    TERMINATOR = b'\x00'
    SEVERITY = b'S'
    EN_SEVERITY = b'V'
    SQLSTATE_CODE = b'C'
    MESSAGE = b'M'

@dataclass
class NoticeResponse(Serializable):
    msg_type: BackMsgType = BackMsgType.NOTICE_RESPONSE

    severity: NoticeSeverity = NoticeSeverity.INFO
    error_code: ErrorCode = ErrorCode.SUCCESSFUL_COMPLETION
    message: bytes = b'hello world'

    def serialize(self) -> bytes:
        MSG_LEN_SZ = 4
        EOS = b'\x00'
        pl = (
            NoticeField.SEVERITY.value + self.severity.value + EOS +
            NoticeField.EN_SEVERITY.value + self.severity.value + EOS +
            NoticeField.SQLSTATE_CODE.value + self.error_code.value + EOS +
            NoticeField.MESSAGE.value + self.message + EOS +
            NoticeField.TERMINATOR.value
        )
        msg_len = MSG_LEN_SZ + len(pl) 
        r = struct.pack(
            "!cI", 
            self.msg_type.value,
            msg_len,
        ) + pl
        return r

class TxStatus(enum.Enum):
    IDLE = b'I'
    IN_TX = b'T'
    FAILED = b'E'

@dataclass
class ReadyForQuery(Serializable):
    msg_type: BackMsgType = BackMsgType.READY_FOR_QUERY
    static_len = 5

    status: TxStatus = TxStatus.IDLE

    def serialize(self) -> bytes:
        return struct.pack(
            "!cIc",
            self.msg_type.value,
            self.static_len,
            self.status.value,
        )


@dataclass
class Handshake:
    proto: ProtoVersion
    inner: StartupMessage | GSSRequest | SSLRequest | InvalidHandshake

    def accept(self) -> Sequence[Serializable]:
        return [ 
            AuthenticationOk(),
            NoticeResponse(),
            ReadyForQuery(),
        ]

    def refuse(self) -> Serializable:
        return RefuseHandshakeMsg()

    @staticmethod
    def deserialize(raw_msg: bytes) -> Self:
        VERSION_SZ = 4
        proto = ProtoVersion.deserialize(raw_msg)
        match proto:
            case ProtoVersion.GSSENCRequest: 
                m = GSSRequest()
            case ProtoVersion.SSLRequest: 
                m = SSLRequest()
            case ProtoVersion.Postgres20:
                print("Protocol 2.0 is not supported.")
                m = InvalidHandshake()
            case ProtoVersion.Postgres30 | ProtoVersion.Postgres32:
                m = StartupMessage.deserialize(raw_msg[VERSION_SZ:])
            case ProtoVersion.Unknown | _:
                print("Unknown protocol")
                m = InvalidHandshake()

        return Handshake(proto, m)


# --- Session management ---
class CoState(enum.Enum):
    INITIAL = 0x00
    LOGGED = 0x02
    ENDED = 0x03

async def accept(reader, writer):
    addr = writer.get_extra_info('peername')
    print(f"connection accepted from {addr}")

    state = CoState.INITIAL
    while state is not CoState.ENDED:
        match state:
            case CoState.INITIAL:
                handshake = Handshake.deserialize(await read_len_prefixed_msg(reader))
                match handshake.inner:
                    case GSSRequest() | SSLRequest():
                        writer.write(handshake.refuse().serialize())
                        await writer.drain()
                    case InvalidHandshake():
                        state = CoState.ENDED
                    case StartupMessage():
                        state = CoState.LOGGED
                        for msg in handshake.accept():
                            writer.write(msg.serialize())
                        await writer.drain()
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

