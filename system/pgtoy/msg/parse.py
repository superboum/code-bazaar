from dataclasses import dataclass

import msg.serializable as ser


@dataclass
class Parse:
    pass


@dataclass
class ParseComplete(ser.Serializable):
    def serialize(self, writer: ser.Writer) -> None:
        raise Exception("Not yet implemented")
