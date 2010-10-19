Nonterminals iso_date_time iso_date time
	     time_hour time_minute time_second time_fraction time_numoffset
	     direction numoffset_minute time_zone timeopt_hour timeopt_minute
	     timespec_hour timespec_minute timespec_second timespec_base.

Terminals integer comma dot plus minus mminus mmminus colon z t h m s.

Rootsymbol iso_date_time.

time_hour -> integer : {time_hour, $1}.
time_minute -> integer : {time_minute, $1}.
time_second -> integer : {time_second, $1}.

time_fraction -> comma integer : {time_fraction, $2}.
time_fraction -> dot   integer : {time_fraction, $2}.

time_numoffset -> direction time_hour numoffset_minute
	       : {time_numoffset, $1, $2, $2}.

direction -> plus : '+'.
direction -> minus : '-'.

numoffset_minute -> '$empty' : none.
numoffset_minute -> colon time_minute : $2.

time_zone -> z : $1.
time_zone -> time_numoffset : $1.

timeopt_hour -> minus : none.
timeopt_hour -> time_hour colon : $1.

timeopt_minute -> minus : none.
timeopt_minute -> time_minute colon : $1.

timespec_hour -> time_hour : {timespec, $1}.
timespec_hour -> time_hour colon time_minute : {timespec, $1, $3}.
timespec_hour -> time_hour colon time_minute colon time_second : {timespec, $1, $3, $5}.

timespec_minute -> timeopt_hour time_minute : {timespec, $1, $2}.
timespec_minute -> timeopt_hour time_minute colon time_second : {timespec, $1, $2, $4}.

timespec_second -> minus timeopt_minute time_second : {timespec, $2, $3}.

timespec_base -> timespec_hour : $1.
timespec_base -> timespec_minute : $1.
timespec_base -> timespec_second : $1.

time -> timespec_base : {time, $1}.
time -> timespec_base time_fraction : {time, $1, $2}.
time -> timespec_base time_zone : {time, $1, none, $2}.
time -> timespec_base time_fraction time_zone : {time, $1, $2, $3}.


date_century -> integer : $1.
date_decade  -> integer : $1.
date_subdecade -> integer : $1.

date_year -> date_decade date_subdecade.
date_fullyear -> date_century date_year.

date_month -> integer : $1.
date_wday -> integer : $1.
date_mday -> integer : $1.
date_yday -> integer : $1.
date_week -> integer : $1.

datepart_fullyear -> m_date_century date_year m_minus.

m_date_century -> '$empty' : none.
m_date_century -> date_century : $1.

m_minus -> '$empty'.
m_minus -> minus.

datepart_ptyear -> minus m_date_subdecade : $2.

m_date_subdecade -> date_subdecade m_minus : $1.

datepart_wkyear -> datepart_ptyear.
datepart_wkyear -> datepart_fullyear.

dateopt_century -> minus.
dateopt_century -> date_century.

dateopt_fullyear -> minus.
dateopt_fullyear -> datepart_fullyear.

dateopt_year -> minus.
dateopt_year -> m_date_year.
dateopt_month -> minus.
dateopt_month -> m_date_month.
dateopt_week -> minus.
dateopt_week -> m_date_week.

m_date_week -> date_week : $1.
m_date_week -> date_week minus : $1.

m_date_month -> date_month : $1.
m_date_month -> date_month minus :$1.

m_date_year -> date_year : $1.
m_date_year -> date_year minus : $1.

datespec_full -> datepart_fullyear date-month minus date_mday.
datespec_full -> datepart_fullyear date-month date_mday.

datespec_year -> date_century.
datespec_year -> dateopt_century date_year.

datespec_month -> minus dateopt_year date_month.
datespec_month -> minus dateopt_year date_month minus date_mday.


datespec_mday -> mminus dateopt_month date_mday.

datespec_week -> datepart_wkyear w datespec_week_2.
datespec_week_2 -> date_week.
datespec_week_2 -> dateopt_week date_wday.

datespec_wday -> mmminus date_wday.
datespec_yday -> dateopt_fullyear date_yday.

date -> datespec_full : $1.
date -> datespec_year : $1.
date -> datespec_month : $1.
date -> datespec_mday : $1.
date -> datespec_week : $1.
date -> datespec_wday : $1.
date -> datespec_yday : $1.

iso_date_time -> date t time.

