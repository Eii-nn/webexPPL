:- use_module(library(http/http_open)).

% Facts: Define safety status of protocols
safe_protocol(https).
unsafe_protocol(http).

% Rules: Check if URL starts with a protocol prefix
url_starts_with(URL, Prefix) :-
    sub_atom(URL, 0, _, _, Prefix).

% Rules: Extract protocol from URL using pattern matching
url_protocol(URL, http) :-
    url_starts_with(URL, 'http://').

url_protocol(URL, https) :-
    url_starts_with(URL, 'https://').

% Rules: Check if URL has valid protocol (HTTP or HTTPS)
valid_protocol(URL) :-
    url_protocol(URL, _).

% Rules: Determine if URL is safe (logical inference)
is_safe(URL) :-
    url_protocol(URL, Protocol),
    safe_protocol(Protocol).

% Rules: Determine if URL is not safe (logical inference)
is_not_safe(URL) :-
    url_protocol(URL, Protocol),
    unsafe_protocol(Protocol).

% Rules: Get safety status (logical derivation)
safety_status(URL, 'SAFE') :-
    is_safe(URL).

safety_status(URL, 'NOT SAFE') :-
    is_not_safe(URL).

% Facts: Protocol descriptions
protocol_description(https, 'HTTPS - Encrypted').
protocol_description(http, 'HTTP - Unencrypted').

% Rules: Measure access time using logical predicates
measure_access_time(URL, AccessTime, Success) :-
    valid_protocol(URL),
    get_time(StartTime),
    (
        catch(
            (http_open(URL, Stream, [method(head), timeout(10)]),
             close(Stream),
             Success = true),
            _,
            Success = false
        )
    ),
    get_time(EndTime),
    AccessTime is EndTime - StartTime.

measure_access_time(URL, 0.0, false) :-
    \+ valid_protocol(URL).

% Rules: Display result using logical composition
display_result(URL) :-
    write('\nURL: '), write(URL), nl,
    (
        valid_protocol(URL) ->
        (
            url_protocol(URL, Protocol),
            protocol_description(Protocol, Desc),
            safety_status(URL, Status),
            write('Status: '), write(Status), write(' ('), write(Desc), write(')'), nl,
            measure_access_time(URL, AccessTime, Success),
            (
                Success ->
                format('Access Time: ~3f seconds~n', [AccessTime])
            ;
                format('Access Time: ~3f seconds (Failed)~n', [AccessTime])
            )
        )
    ;
        write('Status: Invalid URL (must start with http:// or https://)'), nl
    ).

% Rules: Process list of URLs (recursive logic)
process_urls([]).
process_urls([URL|Rest]) :-
    atom(URL),
    display_result(URL),
    process_urls(Rest).

process_urls([URL|Rest]) :-
    string(URL),
    atom_string(URLAtom, URL),
    display_result(URLAtom),
    process_urls(Rest).

% Main predicate: Entry point using logical query
main :-
    current_prolog_flag(argv, Args),
    (
        Args = [] ->
        write('Usage: swipl main.pl <url1> [url2] ...'), nl,
        halt(1)
    ;
        process_urls(Args)
    ).

% Entry point
:- initialization(main).
