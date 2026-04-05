from dataclasses import dataclass

import 
import ctx.session as ctx
import dispatch.auth as auth

@dataclass
class Dispatcher:
    auth: auth.Dispatch
    state: ctx.Ctx = ctx.Fresh()

    def register(m: msg.Handshake) ->  None:

