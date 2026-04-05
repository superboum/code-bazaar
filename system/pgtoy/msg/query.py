from msg.simple_query import Query
from msg.parse import Parse, ParseComplete
from msg.bind import Bind
from msg.execute import Execute
from msg.describe import Describe
from msg.close import Close, CloseComplete
from msg.flush import Flush
from msg.sync import Sync
from msg.error import ErrorResponse
from msg.portal_suspended import PortalSuspended
from msg.command_complete import CommandComplete
from msg.empty_query_response import EmptyQueryResponse
from msg.data_row import DataRow
from msg.row_description import RowDescription
from msg.ready_for_query import ReadyForQuery as ReadyForQuery
from msg.notice_response import NoticeResponse as NoticeResponse


FrontMsg = (
    Query 
    | Parse 
    | Bind 
    | Execute 
    | Describe 
    | Close 
    | Flush 
    | Sync
)

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
