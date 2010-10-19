Nonterminals date_time full_time full_date partial_time time_offset
	     time_secfrac time_second time_minute time_hour
	     date_mday date_month date_fullyear time_numoffset

	     dir.

Terminals integer 'T' 'Z' '+' '-' '.' ':'.

Rootsymbol date_time.

date_fullyear -> integer : unwrap('$1').
date_month    -> integer : unwrap('$1').
date_mday     -> integer : unwrap('$1').

time_hour     -> integer : unwrap('$1').
time_minute   -> integer : unwrap('$1').
time_second   -> integer : unwrap('$1').

time_secfrac -> '$empty' : 0.
time_secfrac -> '.' integer : unwrap('$2').

time_numoffset -> dir time_hour ':' time_minute : {'$1', '$2', '$4'}.

dir -> '+' : '+'.
dir -> '-' : '-'.

time_offset -> 'Z' : 'Z'.
time_offset -> time_numoffset : '$1'.

partial_time -> time_hour ':' time_minute ':' time_second time_secfrac : {'$1', '$3', '$5', '$6'}.

full_date -> date_fullyear '-' date_month '-' date_mday : {'$1', '$3', '$5'}.
full_time -> partial_time time_offset : {'$1', '$2'}.

date_time -> full_date 'T' full_time : {'$1', '$3'}.

Erlang code.

unwrap({_,_,V}) -> V.
