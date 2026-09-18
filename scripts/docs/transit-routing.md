# Where do I change to arrive sooner: the commute view (deferred spec)

This is a design, not an implementation. It records the second phase of
[agfi:transit-board]: going from "what leaves this stop" to "how do I get there fastest
if I leave now".

## The question the boards cannot answer

A departure board tells you when the next vehicle leaves. It does not tell you that the
one after it is faster because it meets a connection the first one misses, and that is the
question you actually have when you are choosing between two doors of the same station.
Answering it needs a routing engine, not a timetable feed.

## Which engine

Transitous is the primary, for one concrete reason: its journey planner takes raw
coordinates for origin and destination rather than stop ids, so the configured home and
work points can be places rather than stops, and the walk to whichever stop is best is
part of the answer instead of an assumption baked into the configuration. It returns
several itineraries with per-leg scheduled and realtime flags, which is what makes a
"leave in N minutes" countdown honest: a leg with no realtime data should not be shown as
though it had some.

MVG's own routing is the second opinion. It wants stop ids and an ISO datetime carrying an
offset, and its connections do not carry the line identifier the departure rows use, so it
cannot be joined to the boards on the direction letter. That is enough to keep it out of
the primary path, and not enough to discard it: when the two disagree about a connection
being reachable, that disagreement is information, and the disagreement is usually about
realtime data one of them has and the other does not.

## Shape of the feature

The configuration already carries the named places this needs. A place is a coordinate
pair with a name, kept in the private configuration repository along with everything else
that identifies a location.

On the command line it is a subcommand taking two place names, printing the next few
itineraries as leg chains with the total duration and the time you have to leave. On the
page it is a third tab beside the profiles, showing the same itineraries as chains of line
badges with a leave-in countdown that ticks locally, refreshing on the same visibility
rule as the boards.

The model grows two types, an itinerary and a leg. A leg is either a walk with a duration
or a ride with a line, a mode, a boarding stop, an alighting stop and the two timestamps,
and it carries whether those timestamps are realtime or scheduled. An itinerary is a list
of legs plus the departure and arrival it implies. Nothing else in the model changes: a
departure row stays a departure row.

## The traps

Reversing the query is not the same question. Asking for itineraries that arrive by a
given time is a different call from asking for ones that leave after now, and a commute
view wants both: leaving now on the way out, arriving by a fixed time when there is a
meeting. Both are supported upstream; do not emulate one with the other by filtering.

A routing call is much more expensive upstream than a departure call, and it is the kind
of call a page could very easily make every thirty seconds. It should refresh on a longer
interval than the boards do, and it should not refresh at all merely because a countdown
ticked. Whatever that interval is, it belongs next to the other intervals in the package,
not spelled out here.

Finally, the second opinion is worth building only if it is displayed as one. A quietly
merged pair of routing results, where a row might have come from either engine, is worse
than one engine's answer, because it cannot be checked. If MVG is consulted, say so on the
itinerary it produced.
