import ..ctx.auth as ctx
import ..msg.handshake as msg

@dataclass
class Dispath:
    ctx: State

    def register(m: msg.HFrontMsg) ->  None:
        pass
