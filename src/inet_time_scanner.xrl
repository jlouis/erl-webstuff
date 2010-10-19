
Definitions.

DIGIT = [0-9]
ALPHA = [A-Z]

Rules.

DIGIT+ :
       {token,{integer,TokenLine,list_to_integer(TokenChars)}}.

(,)   : {token, comma}.
(.)   : {token, dot}.
(\+)  : {token, plus}.
(-)   : {token, minus}.
(--)  : {token, mminus}.
(---) : {token, mmminus}.
(:)   : {token, colon}.
ALPHA : {token, {alpha, TokenLine, TokenChars}}.

Erlang code.

