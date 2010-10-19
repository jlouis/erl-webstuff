
Definitions.

DIGIT = [0-9]
ALPHA = [A-Z]

Rules.

{DIGIT}+ :
       {token,{integer,TokenLine,list_to_integer(TokenChars)}}.

(,)   : {token, {comma, TokenLine, ','}}.
(\.)   : {token, {'.', TokenLine, '.'}}.
(\+)  : {token, {'+', TokenLine, '+'}}.
(-)   : {token, {'-', TokenLine, '-'}}.
(--)  : {token, mminus}.
(---) : {token, mmminus}.
(:)   : {token, {':', TokenLine, ':'}}.
{ALPHA} : {token, {case TokenChars of
      	           "Z" -> 'Z';
		   "z" -> 'Z';
		   "T" -> 'T';
		   "t" -> 'T';
		   "h" -> 'H';
		   "m" -> 'M';
		   "s" -> 'S';
		   _   -> unknown
		 end, TokenLine, dummy}}.

Erlang code.

