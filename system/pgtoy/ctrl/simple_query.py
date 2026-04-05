from typing import Sequence

import msg.query as msg
import msg.command_complete as cc


def simple_query(q: msg.Query) -> Sequence[msg.BackMsg]:
    return [
        msg.CommandComplete(cc.TagSelect(0)),
        msg.ReadyForQuery(),
    ]
