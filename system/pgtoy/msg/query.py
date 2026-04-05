from msg.simple_query import Query as Query
from msg.parse import Parse as Parse, ParseComplete as ParseComplete
from msg.bind import Bind as Bind
from msg.execute import Execute as Execute
from msg.describe import Describe as Describe
from msg.close import Close as Close, CloseComplete as CloseComplete
from msg.flush import Flush as Flush
from msg.sync import Sync as Sync
from msg.error import ErrorResponse as ErrorResponse
from msg.portal_suspended import PortalSuspended as PortalSuspended
from msg.command_complete import CommandComplete as CommandComplete
from msg.empty_query_response import EmptyQueryResponse as EmptyQueryResponse
from msg.data_row import DataRow as DataRow
from msg.row_description import RowDescription as RowDescription
from msg.ready_for_query import ReadyForQuery as ReadyForQuery
from msg.notice_response import NoticeResponse as NoticeResponse


FrontMsg = Query | Parse | Bind | Execute | Describe | Close | Flush | Sync

BackMsg = (
    ParseComplete
    | ErrorResponse
    | PortalSuspended
    | CommandComplete
    | EmptyQueryResponse
    | RowDescription
    | DataRow
    | CloseComplete
    | ReadyForQuery
    | NoticeResponse
)
