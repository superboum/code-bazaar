import asyncio
import enum
from abc import ABC, abstractmethod
from typing import Literal

import msg.cmd_id as cmd_id

ENDIAN: Literal["big", "little"] = "big"


class Sz(enum.Enum):
    U8 = 1
    U16 = 2
    U32 = 4


class ExtStreamWriter(asyncio.StreamWriter):
    def write_type_len_head(self, kind: cmd_id.MsgType, mlen: int) -> None:
        final_mlen = mlen + Sz.U32.value
        self.write(kind.value)
        self.write(final_mlen.to_bytes(Sz.U32.value, ENDIAN))


class Serializable(ABC):
    @abstractmethod
    def serialize(self, writer: ExtStreamWriter) -> None: ...
