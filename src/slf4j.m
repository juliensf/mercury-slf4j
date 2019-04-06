%----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
%-----------------------------------------------------------------------------%

:- module slf4j.
:- interface.

:- import_module io.
:- import_module list.
:- import_module string.

%-----------------------------------------------------------------------------%

:- type logger.

:- pred get_logger(string::in, logger::out, io::di, io::uo) is det.

:- func get_name(logger::in, io::ui) = (string::out) is det.

:- func root_logger_name = string.

%-----------------------------------------------------------------------------%

:- pred debug(logger::in, string::in, io::di, io::uo) is det.

:- pred error(logger::in, string::in, io::di, io::uo) is det.

:- pred info(logger::in, string::in, io::di, io::uo) is det.

:- pred trace(logger::in, string::in, io::di, io::uo) is det.

:- pred warn(logger::in, string::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- pred format_debug(logger::in, string::in, list(poly_type)::in,
    io::di, io::uo) is det.

:- pred format_error(logger::in, string::in, list(poly_type)::in,
    io::di, io::uo) is det.

:- pred format_info(logger::in, string::in, list(poly_type)::in,
    io::di, io::uo) is det.

:- pred format_trace(logger::in, string::in, list(poly_type)::in,
    io::di, io::uo) is det.

:- pred format_warn(logger::in, string::in, list(poly_type)::in,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- pred is_debug_enabled(logger::in, io::ui) is semidet.

:- pred is_error_enabled(logger::in, io::ui) is semidet.

:- pred is_info_enabled(logger::in, io::ui) is semidet.

:- pred is_trace_enabled(logger::in, io::ui) is semidet.

:- pred is_warn_enabled(logger::in, io::ui) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

:- pragma foreign_type("Java", logger, "org.slf4j.Logger").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_logger(Name::in, Logger::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Logger = org.slf4j.LoggerFactory.getLogger(Name);
").

:- pragma foreign_proc("Java",
    get_name(Logger::in, _IO::ui) = (Name::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Name = Logger.getName();
").

:- pragma foreign_proc("Java",
    root_logger_name = (Name::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Name = org.slf4j.Logger.ROOT_LOGGER_NAME;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    debug(Logger::in, Msg::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Logger.debug(Msg);
").

:- pragma foreign_proc("Java",
    error(Logger::in, Msg::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Logger.error(Msg);
").

:- pragma foreign_proc("Java",
    info(Logger::in, Msg::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Logger.info(Msg);
").

:- pragma foreign_proc("Java",
    trace(Logger::in, Msg::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Logger.trace(Msg);
").

:- pragma foreign_proc("Java",
    warn(Logger::in, Msg::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Logger.warn(Msg);
").

%-----------------------------------------------------------------------------%

format_debug(Logger, Spec, Comps, !IO) :-
    ( if is_debug_enabled(Logger, !.IO) then
        string.format(Spec, Comps, Msg),
        debug(Logger, Msg, !IO)
    else
        true
    ).

format_error(Logger, Spec, Comps, !IO) :-
    ( if is_error_enabled(Logger, !.IO) then
        string.format(Spec, Comps, Msg),
        error(Logger, Msg, !IO)
    else
        true
    ).

format_info(Logger, Spec, Comps, !IO) :-
    ( if is_info_enabled(Logger, !.IO) then
        string.format(Spec, Comps, Msg),
        info(Logger, Msg, !IO)
    else
        true
    ).

format_trace(Logger, Spec, Comps, !IO) :-
    ( if is_trace_enabled(Logger, !.IO) then
        string.format(Spec, Comps, Msg),
        slf4j.trace(Logger, Msg, !IO)
    else
        true
    ).

format_warn(Logger, Spec, Comps, !IO) :-
    ( if is_warn_enabled(Logger, !.IO) then
        string.format(Spec, Comps, Msg),
        warn(Logger, Msg, !IO)
    else
        true
    ).

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    is_debug_enabled(Logger::in, _IO::ui),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = Logger.isDebugEnabled();
").

:- pragma foreign_proc("Java",
    is_error_enabled(Logger::in, _IO::ui),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = Logger.isErrorEnabled();
").

:- pragma foreign_proc("Java",
    is_info_enabled(Logger::in, _IO::ui),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = Logger.isInfoEnabled();
").

:- pragma foreign_proc("Java",
    is_trace_enabled(Logger::in, _IO::ui),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = Logger.isTraceEnabled();
").

:- pragma foreign_proc("Java",
    is_warn_enabled(Logger::in, _IO::ui),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = Logger.isWarnEnabled();
").

%-----------------------------------------------------------------------------%
:- end_module slf4j.
%-----------------------------------------------------------------------------%
