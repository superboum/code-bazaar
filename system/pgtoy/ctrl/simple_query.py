from typing import Sequence

import msg.query as msg


def simple_query(q: msg.Query) -> Sequence[msg.BackMsg]:
    print(q)
    return []
