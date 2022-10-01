%----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2019, 2022 Julien Fischer.
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

:- type marker.

:- pred get_marker(string::in, marker::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- pred debug(logger::in, string::in, io::di, io::uo) is det.

:- pred debug(logger::in, marker::in, string::in, io::di, io::uo) is det.

:- pred error(logger::in, string::in, io::di, io::uo) is det.

:- pred error(logger::in, marker::in, string::in, io::di, io::uo) is det.

:- pred info(logger::in, string::in, io::di, io::uo) is det.

:- pred info(logger::in, marker::in, string::in, io::di, io::uo) is det.

:- pred trace(logger::in, string::in, io::di, io::uo) is det.

:- pred trace(logger::in, marker::in, string::in, io::di, io::uo) is det.

:- pred warn(logger::in, string::in, io::di, io::uo) is det.

:- pred warn(logger::in, marker::in, string::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- pred format_debug(logger::in, string::in, list(poly_type)::in,
    io::di, io::uo) is det.
:- pragma format_call(pred(format_debug/5), format_string_values(2, 3)).

:- pred format_debug(logger::in, marker::in, string::in, list(poly_type)::in,
    io::di, io::uo) is det.
:- pragma format_call(pred(format_debug/6), format_string_values(3, 4)).

:- pred format_error(logger::in, string::in, list(poly_type)::in,
    io::di, io::uo) is det.
:- pragma format_call(pred(format_error/5), format_string_values(2, 3)).

:- pred format_error(logger::in, marker::in, string::in, list(poly_type)::in,
    io::di, io::uo) is det.
:- pragma format_call(pred(format_error/6), format_string_values(3, 4)).

:- pred format_info(logger::in, string::in, list(poly_type)::in,
    io::di, io::uo) is det.
:- pragma format_call(pred(format_info/5), format_string_values(2, 3)).

:- pred format_info(logger::in, marker::in, string::in, list(poly_type)::in,
    io::di, io::uo) is det.
:- pragma format_call(pred(format_info/6), format_string_values(3, 4)).

:- pred format_trace(logger::in, string::in, list(poly_type)::in,
    io::di, io::uo) is det.
:- pragma format_call(pred(format_trace/5), format_string_values(2, 3)).

:- pred format_trace(logger::in, marker::in, string::in, list(poly_type)::in,
    io::di, io::uo) is det.
:- pragma format_call(pred(format_trace/6), format_string_values(3, 4)).

:- pred format_warn(logger::in, string::in, list(poly_type)::in,
    io::di, io::uo) is det.
:- pragma format_call(pred(format_warn/5), format_string_values(2, 3)).

:- pred format_warn(logger::in, marker::in, string::in, list(poly_type)::in,
    io::di, io::uo) is det.
:- pragma format_call(pred(format_warn/6), format_string_values(3, 4)).

%-----------------------------------------------------------------------------%

:- pred is_debug_enabled(logger::in, io::ui) is semidet.

:- pred is_error_enabled(logger::in, io::ui) is semidet.

:- pred is_info_enabled(logger::in, io::ui) is semidet.

:- pred is_trace_enabled(logger::in, io::ui) is semidet.

:- pred is_warn_enabled(logger::in, io::ui) is semidet.

%-----------------------------------------------------------------------------%

:- pred mdc_put(string::in, string::in, io::di, io::uo) is det.

:- pred mdc_remove(string::in, io::di, io::uo) is det.

:- pred mdc_clear(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

:- pragma foreign_type("Java", logger, "org.slf4j.Logger").

:- pragma foreign_type("Java", marker, "org.slf4j.Marker").

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
    get_marker(Name::in, Marker::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Marker = org.slf4j.MarkerFactory.getMarker(Name);
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    debug(Logger::in, Msg::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Logger.debug(Msg);
").

:- pragma foreign_proc("Java",
    debug(Logger::in, Marker::in, Msg::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Logger.debug(Marker, Msg);
").

:- pragma foreign_proc("Java",
    error(Logger::in, Msg::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Logger.error(Msg);
").

:- pragma foreign_proc("Java",
    error(Logger::in, Marker::in, Msg::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Logger.error(Marker, Msg);
").

:- pragma foreign_proc("Java",
    info(Logger::in, Msg::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Logger.info(Msg);
").

:- pragma foreign_proc("Java",
    info(Logger::in, Marker::in, Msg::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Logger.info(Msg, Marker);
").

:- pragma foreign_proc("Java",
    trace(Logger::in, Msg::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Logger.trace(Msg);
").

:- pragma foreign_proc("Java",
    trace(Logger::in, Marker::in, Msg::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Logger.trace(Marker, Msg);
").

:- pragma foreign_proc("Java",
    warn(Logger::in, Msg::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Logger.warn(Msg);
").

:- pragma foreign_proc("Java",
    warn(Logger::in, Marker::in, Msg::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Logger.warn(Marker, Msg);
").

%-----------------------------------------------------------------------------%

format_debug(Logger, Spec, Comps, !IO) :-
    ( if is_debug_enabled(Logger, !.IO) then
        string.format(Spec, Comps, Msg),
        debug(Logger, Msg, !IO)
    else
        true
    ).

format_debug(Logger, Marker, Spec, Comps, !IO) :-
    ( if is_debug_enabled(Logger, !.IO) then
        string.format(Spec, Comps, Msg),
        debug(Logger, Marker, Msg, !IO)
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

format_error(Logger, Marker, Spec, Comps, !IO) :-
    ( if is_error_enabled(Logger, !.IO) then
        string.format(Spec, Comps, Msg),
        error(Logger, Marker, Msg, !IO)
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

format_info(Logger, Marker, Spec, Comps, !IO) :-
    ( if is_info_enabled(Logger, !.IO) then
        string.format(Spec, Comps, Msg),
        info(Logger, Marker, Msg, !IO)
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

format_trace(Logger, Marker, Spec, Comps, !IO) :-
    ( if is_trace_enabled(Logger, !.IO) then
        string.format(Spec, Comps, Msg),
        slf4j.trace(Logger, Marker, Msg, !IO)
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

format_warn(Logger, Marker, Spec, Comps, !IO) :-
    ( if is_warn_enabled(Logger, !.IO) then
        string.format(Spec, Comps, Msg),
        warn(Logger, Marker, Msg, !IO)
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

:- pragma foreign_proc("Java",
    mdc_put(Key::in, Value::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    org.slf4j.MDC.put(Key, Value);
").

:- pragma foreign_proc("Java",
    mdc_remove(Key::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    org.slf4j.MDC.remove(Key);
").

:- pragma foreign_proc("Java",
    mdc_clear(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    org.slf4j.MDC.clear();
").

%-----------------------------------------------------------------------------%
:- end_module slf4j.
%-----------------------------------------------------------------------------%
