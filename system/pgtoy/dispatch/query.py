from dataclasses import dataclass
from typing import Sequence

import msg.query as msg


@dataclass
class Dispatcher:
    def init_msg(self) -> Sequence[msg.BackMsg]:
        return [
            msg.NoticeResponse(),
            msg.ReadyForQuery(),
        ]

    def register(self, m: msg.FrontMsg) -> Sequence[msg.BackMsg]:
        raise Exception("Not yet implemented")
