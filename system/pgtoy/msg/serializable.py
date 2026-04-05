import asyncio
import enum
from dataclasses import dataclass
from abc import ABC, abstractmethod
from typing import Literal

import msg.cmd_id as cmd_id

ENDIAN: Literal["big", "little"] = "big"


class Sz(enum.Enum):
    U8 = 1
    U16 = 2
    U32 = 4

@dataclass
class Writer:
    inner: asyncio.StreamWriter

    def write(self, data: bytes) -> None:
        self.inner.write(data)

    def write_type_len_head(self, kind: cmd_id.MsgType, mlen: int) -> None:
        final_mlen = mlen + Sz.U32.value
        self.inner.write(kind.value)
        self.inner.write(final_mlen.to_bytes(Sz.U32.value, ENDIAN))


class Serializable(ABC):
    @abstractmethod
    def serialize(self, writer: Writer) -> None: ...
