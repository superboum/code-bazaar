from dataclasses import dataclass

import ctx.auth as ctx
import msg.handshake as msg


@dataclass
class Dispatch:
    state: ctx.State

    def register(self, m: msg.FrontMsg) -> None:
        pass
