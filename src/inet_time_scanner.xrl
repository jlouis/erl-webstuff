
Definitions.

DIGIT = [0-9]
ALPHA = [A-Z]

Rules.

DIGIT+ :
       {token,{integer,TokenLine,list_to_integer(TokenChars)}}.

(,)   : {token, comma}.
(.)   : {token, dot}.
(\+)  : {token, '+'}.
(-)   : {token, '-'}.
(--)  : {token, mminus}.
(---) : {token, mmminus}.
(:)   : {token, colon}.
ALPHA : {token, case TokenChars of
      	           $Z -> 'Z';
		   $z -> 'Z';
		   $T -> 'T';
		   $t -> 'T';
		   $H -> 'H';
		   $M -> 'M';
		   $S -> 'S' end}.

Erlang code.

