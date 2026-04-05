import enum
from dataclasses import dataclass
from typing import ClassVar

import msg.serializable as ser
import msg.cmd_id as cmd_id


class ParamField(enum.Enum):
    APPLICATION_NAME = b"application_name"
    CLIENT_ENCODING = b"client_encoding"
    DATE_STYLE = b"DateStyle"
    DEFAULT_TRANSACTION_READ_ONLY = b"default_transaction_read_only"
    IN_HOT_STANDBY = b"in_hot_standby"
    INTEGER_DATETIMES = b"integer_datetimes"
    INTERVAL_STYLE = b"IntervalStyle"
    IS_SUPERUSER = b"is_superuser"
    SCRAM_ITERATIONS = b"scram_iterations"
    SEARCH_PATH = b"search_path"
    SERVER_ENCODING = b"server_encoding"
    SERVER_VERSION = b"server_version"
    SESSION_AUTHORIZATION = b"session_authorization"
    STANDARD_CONFORMING_STRINGS = b"standard_conforming_strings"
    TIMEZONE = b"TimeZone"


@dataclass
class ParameterStatus(ser.Serializable):
    msg_type: ClassVar[cmd_id.BackMsgType] = cmd_id.BackMsgType.PARAMETER_STATUS
    params: dict[ParamField, str]

    def serialize(self, writer: ser.Writer) -> None:
        EOS = b"\x00"
        pl = b""
        for k, v in self.params.items():
            pl += k.value + EOS + v.encode() + EOS

        writer.write_type_len_head(
            self.msg_type,
            len(pl),
        )
        writer.write(pl)
