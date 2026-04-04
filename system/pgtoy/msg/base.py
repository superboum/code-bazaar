from abc import ABC, abstractmethod
import io

MSG_TYPE_SZ = 1
MSG_LEN_SZ = 4

class Serializable(ABC):
    @abstractmethod
    def serialize(self, writer: io.Writer) -> None:
        ...

class Deserializer[T](ABC):
    @abstractmethod
    async def deserialize(self, reader: io.Reader) -> T:
        ...



class LenDsrlz[T](Deserializer[T]):
    async def read_msg_len() -> Self:
        raw_len = await self.reader.read(MSG_LEN_SZ)
        (msg_len,) = struct.unpack("!I", raw_len)
        msg_len -= MSG_LEN_SZ # count inclument the int we just read

        self.msg_len = msg_len

    async def read_msg_type() -> bytes:
        return await self.reader.read(MSG_TYPE_SZ)

    async def read_len_prefixed_msg() -> None:
        msg_len = await self.read_msg_len()
        raw_msg = b''
        while len(raw_msg) < msg_len:
            raw_msg += await self.reader.read(msg_len - len(raw_msg))
        return raw_msg

    async def read_type_and_len_prefixed_msg() -> tuple[bytes, bytes]:
        msg_type = await self.read_msg_type()
        raw_msg = await self.read_len_prefixed_msg()
        return msg_type, raw_msg

