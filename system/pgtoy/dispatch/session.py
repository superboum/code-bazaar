import asyncio
from dataclasses import dataclass

import ctx.session as ctx
import dispatch.auth as auth
import msg.all as msg

@dataclass
class Dispatcher:
    auth: auth.Dispatch
    state: ctx.Ctx = ctx.Fresh()

    def register(self, m: msg.FrontMsg) -> None:
        pass

