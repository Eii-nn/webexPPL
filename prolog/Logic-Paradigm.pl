
% LOGIC PARADIGM: URL PHISHING CHECKER


%Facts: Protocol safety 
safe_protocol(https).
unsafe_protocol(http).

%Rules: Detect protocol
starts_with(URL, Prefix) :-
    sub_string(URL, 0, _, _, Prefix).

url_protocol(URL, https) :-
    starts_with(URL, "https://").

url_protocol(URL, http) :-
    starts_with(URL, "http://").

%Rules: Determine safety
is_safe(URL) :-
    url_protocol(URL, Protocol),
    safe_protocol(Protocol).

is_not_safe(URL) :-
    url_protocol(URL, Protocol),
    unsafe_protocol(Protocol).

%Rules: Safety status
safety_status(URL, "SAFE") :-
    is_safe(URL).

safety_status(URL, "NOT SAFE") :-
    is_not_safe(URL).

safety_status(_, "INVALID URL").

%io ngani
process_url :-
    write('Enter URL: '),
    read_line_to_string(user_input, URLStr),
    normalize_space(string(URLTrimmed), URLStr),  
    atom_string(URLAtom, URLTrimmed),            
    safety_status(URLAtom, Status),
    nl,
    write('URL: '), write(URLTrimmed), nl,
    write('Status: '), write(Status), nl.

% Entry point (auto consult)
:- initialization(process_url).
