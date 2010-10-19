Nonterminals iso_date_time iso_date iso_time.

Terminals integer comma dot plus minus mminus mmminus colon z t h m s.

Rootsymbol iso_date_time.

iso_date -> integer : {date, $1}.

iso_time -> integer : {time, $1}.

iso_date_time -> iso_date t iso_time.

