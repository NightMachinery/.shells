# transit-board: departure boards from the shell

`transit-board` prints the next departures from the stops you actually use, in
the terminal, in about the time it takes to put a coat on. The mental model:
the private configuration says which stops matter and which lines at them are
worth walking for, and the tool renders that as a board.

The renderer is a TypeScript CLI in `javascript/transit`, run with `bun`. The
zsh wrappers live in `zshlang/auto-load/others/transit.zsh`:
[agfi:transit-board] is the general entry point, [agfi:departures] and its
variants are the everyday ones, and [agfi:h-transit-run] is the single place
`bun` is invoked, so no two of them can drift apart in how they call the tool.

## The public half and the private half

This repository is public. Stop ids, line numbers, direction letters and
coordinates say where a particular person lives, works and sleeps, so none of
them are here, and none of them are in the TypeScript package either. They live
in a private address configuration file, which the CLI reads through the
`ADDRESS_CONFIG` environment variable. The zsh side is the half that knows
where that file is: the `transit_config_file` knob names it, and
[agfi:h-transit-run] exports it at call time, so overriding the knob for a
single call is obeyed.

The package ships no stops and no default profile. That is deliberate rather
than an oversight: a public tool with a built-in default profile is a tool that
shows a stranger's neighbourhood to everyone who runs it. With the
configuration absent, [agfi:h-transit-conf-assert] refuses to run and says so,
naming the path it looked at and why the values are not in this repository.
`transit-board --help` is the exception, and works on a machine that has never
been configured: being told to go and write a configuration file in order to
read a usage line would be a poor joke.

One word crosses the line. The configuration has a `[defaults]` section whose
`home` key names one of its own profiles, and `home` is the only profile word
the public side knows. What it resolves to is the configuration's business.
Every other profile key is a private name. [agfi:departures-work] is a one-line
`aliasfn` variant and so could any other be, but only `home` and `work` are
defined here, on purpose: a profile key is a word somebody chose to name a place
they go, so the more of them this public file lists, the more it says about a
private one. Reach the rest as `departures <key>`, which needs no line here at
all.

## The commands

[agfi:departures] renders one profile's boards, defaulting to `home`. It is the
command you actually type:

    departures
    departures work

[agfi:departures-home] and [agfi:departures-work] are `aliasfn` variants over
it. They carry no docstring of their own, which is exactly why they are
`aliasfn`s: a variant that needs documentation has to be written as a real
function instead.

[agfi:departures-watch] is the same board, live. It re-renders until you
interrupt it, and it uses the CLI's own `--watch` loop rather than a loop in
zsh: the tool knows when its data changed, it can redraw in place instead of
scrolling a fresh copy past you, and a shell loop would re-pay process startup
on every tick. It is never paged.

[agfi:transit-board] is the general form, and takes whatever the CLI takes:

    transit-board board '<stop-id>'
    transit-board route home --to work
    transit-board search Marienplatz
    transit-board nearby '<lat>' '<lon>'
    transit-board discover '<stop-id>'
    transit-board messages
    transit-board config-export --json

`search` is how you turn a name you know into a stop id you can paste; searching
for a central landmark by name gives you its id and the modes that call there.
`nearby` does the same from a coordinate. `messages` prints service messages,
narrowed to a profile's lines when you name one.

Paging is the wrapper's decision, not the tool's. Human-readable output goes
through [agfi:pager-if-overflow] when stdout is a terminal, so a long board
scrolls and a short one does not disturb the prompt. Output is never paged when
`--json` is present, because its consumer is a program, nor when stdout is not a
terminal, nor under `--watch`, which draws its own screen. When the wrapper does
page, it tells the CLI to colour anyway: the tool colours itself when its own
stdout is a terminal, and its own stdout is a pipe precisely when we are paging,
so that decision has to be made out here where the terminal still is.

## Direction letters, and why not destinations

A board is a filter: at this stop, these lines, going this way. The "going this
way" part is a direction letter, and [agfi:transit-board]'s `discover`
subcommand is how you find it. Point it at a stop and it lists every (line,
direction) pair seen there with a few sample destinations, which is enough to
recognise which letter means the way you go.

The obvious alternative, filtering on the destination shown on the front of the
train, is wrong, and wrong in a way that hides: one direction of a line can
advertise two or more different headsigns. Trains that run the full length and
trains that turn back early carry different destination names while going the
same way out of your stop. Group by destination and one direction silently
becomes two half-boards, each missing the departures that belong to the other,
and nothing in the output says a thing is missing. The direction letter is the
stable key, so it is the one the configuration stores.

## Merged stops: walking times and labels

Two stops a few minutes apart, served by the same line in the same direction,
belong on one board: you are choosing between them, not consulting them
separately. The complication is that they are not equally far from the door, so
a single board-wide walking time is wrong for at least one of them. `walk_minutes`
carries the board's figure and `walk_minutes_by_stop` overrides it for the stops
that differ, and reachability is decided per row from the walking time of that
row's own stop.

Once a board merges stops, every row needs to say which stop it came from, and
the board needs to say what the walking times are, or the dimming looks
arbitrary. Both read off the same short name: a stop written as a table with an
`id` and a `label` supplies one, and a stop written as a bare id falls back to
the last field of the identifier, which is a bare number. So give the stops
labels whenever a board merges more than one. The board then heads itself with
the per-stop figures in board order rather than a count and one number, and the
rows carry `@Label` instead of `@1682`.

## Onward connections

A board can name one interchange it feeds into, and every row then carries the
first onward departure a rider leaving on that row could still catch. That is
the question a departure board cannot otherwise answer: two trams four minutes
apart are interchangeable until you know that only one of them makes the next
underground train.

The configuration gives the interchange stop, the lines to watch there, an
optional direction letter, and two static figures: how long this board's vehicle
takes to reach the interchange, and how long the change itself takes. Both are
static on purpose. A per-departure figure needs a journey lookup for every row,
which is a different order of request volume against a free API, and for a fixed
pair of stops the static pair is right to within a minute or two, which is
enough to answer "do I make it or do I wait".

An empty slot on a row is information rather than a gap: it means the board has
an interchange and nothing at it was catchable from that row.

## The near window and the far window

A board has two parts. The near window lists individual departures: minutes
until it leaves, line, destination, clock time, delay, platform, and how firm
the prediction is. That is the part you read when you are deciding whether to
run.

The far window is a strip. Past the first stretch there is no point repeating a
row per departure, because you are no longer choosing a train, you are checking
a rhythm: is there one every ten minutes all evening, or is the last useful one
soon. So each (line, direction letter) pair collapses to a single row of times
running out to the horizon. It is compact enough that a long horizon stays
readable, which is what makes a long horizon worth asking for at all.

## The commute view

A departure board says when vehicles leave. At a stop served by four lines that
is not the question: the question is which of them gets you there soonest, and
the answer is not always the first one to arrive at the platform.

`transit-board route <profile>` answers it. For every departure on that
profile's opted-in boards it plans a journey to a destination and prints, beside
the row, where you get off the first leg, what you catch there, and when you
arrive. The destination defaults to the other end of the usual commute and
`--to` overrides it.

Boards opt in with a flag in the private configuration, and that opt-in is the
point rather than a convenience. A journey plan is one request sequence per
board per refresh against a free and unauthenticated service, where a departure
board is one cheap call. Most boards are not journeys anyone plans; they are
"is there a bus soon".

Two things about the output are worth knowing before you trust it.

A *tight* option is one that only works if the first leg runs a little early or
the change is quicker than estimated. It is shown because it is sometimes the
one you actually take, and it is never the recommendation. The window is
adjustable.

Journeys are not ranked by arrival alone. Every option carries the minutes you
spend on foot, changes and the final walk together, and a walked minute is
charged at more than a ridden one before the options are sorted. Without that
the recommendation is regularly a train that arrives a couple of minutes
earlier and leaves you a quarter of an hour from the door, beating one that
stops at the end of your street. `walk_weight` in the config's defaults sets
the exchange rate, one turns it off and ranks by arrival the way the planner
itself does, and the browser page lets you move it for one look without saving
it.

### Where a journey can end

A destination is either a doorstep, declared with coordinates, or a station,
declared with a stop id and a label. A station is a destination in its own
right: you are going to Marienplatz, not through it, so the journey ends when the
vehicle does and there is no walk at the end. Stations are offered from every
profile; a profile can name its own order for the picker, and without one the
doorsteps come first.

A plan towards a doorstep is asked several times over, once for the coordinate
and once for each stop the profile that lives there is built from, each with
the walking time the configuration records for it. That is not redundancy. The
planner answers with the best journeys over arrival, changes and departure, and
the walk at the end is in none of those, so a journey that arrives a few
minutes later at the station six minutes from the door is beaten by one that
arrives sooner fourteen minutes away and is never offered at all. Naming the
near station makes that journey exist; the ranking above then decides between
them. The searches run at once rather than one after another.

### Which identifier the planner is given

The planner is a different service from the departure board, and it does not
always know a stop by the identifier the board is configured with: some stops
exist in its data only as their individual platforms. So the identifier is
resolved before anything is planned, trying the stop itself, then the platforms
the departure rows already name, then the station's coordinate. Which one
answered is printed in the board heading and carried in the JSON, because a
plan made from one platform is a slightly narrower claim than one made from the
whole stop. The answer is cached, so this costs one small request the first time
a stop is planned and nothing afterwards.

Destinations go through the same chain, and one thing follows from that which
is worth stating plainly: when a destination stop can only be resolved to the
station's position, the walking time the configuration records for it is
dropped and the planner's own final walk is used instead. The configured figure
is the walk *from that stop*, and once the plan is free to alight anywhere near
the position it is a measurement of something else.

One destination the planner cannot place does not spoil the board: that target
is dropped and the others answer. Only when every destination fails is the
origin blamed, which is the one case where the origin is what they had in
common.

Long-distance trains are left out of the search. The planner routes over the
whole national timetable and would otherwise recommend an inter-city train
through the middle of a commute, which is a real journey and not one a local
ticket covers. `plan_modes` in the config's defaults puts them back for someone
who holds a ticket that does.

Transfer walking times are derived from the distance of the walking leg, not
from the planner's own duration for it. The planner adds a flat padding of
several minutes to every walking leg, which is defensible for a stranger and
wrong for a person who knows the station, and taking it at face value makes
every change look impossible.

The itineraries come back in one wide request rather than a walk through pages
of them: the planner takes a search window, and asking for the span the board
actually covers answers in one round trip what the page walk needed two or more
for. The page walk is still there as the safety net for a board whose horizon
outruns the widest window worth asking a free service for.

## The cache

`transit_board_cache_p` turns the response cache on, and it is off by default.
That default is a judgement about what a board is for: a person standing in a
hallway looking at departure times wants the truth, and a cached board that is
even slightly stale is worse than no board, because it looks exactly as
authoritative as a fresh one.

Turn it on for the opposite kind of caller: something that asks repeatedly,
where the same answer served twice is fine and hammering the upstream API is
not. A status line, a dashboard poll, a script that renders several profiles in
a row. An unreachable cache is not fatal: the tool says so on stderr and
continues with a live fetch.

## The horizon

`transit_board_horizon` sets how far ahead to look, in minutes, and passes
through as `--horizon`. Ask for more when you are planning the evening rather
than the next ten minutes. The reason a long horizon is usable at all is the
strip described above: without the far window collapsing to one row per line
and direction, a few hours of departures would be a wall of text nobody reads.

## Backends, and why a board says which one answered

`transit_board_backend` forces the primary backend; left alone, the tool picks
one and falls back to another when the first does not answer. Each rendered
board names the backend that produced it, and that label is not decoration. The
sources do not agree in detail: they differ in how much real-time data they
carry, in how they name platforms, sometimes in which departures they list at
all. A fallback answer is a slightly different answer, so when a board looks
unfamiliar, the first thing to check is whether it came from somewhere else than
usual.

## Etiquette

The upstream APIs are free, unauthenticated, and meant for private
non-commercial use. They return no rate-limit signal of any kind: no quota
header, no 429 with a retry hint, nothing that tells you that you are asking too
often. Nothing will warn you before access is withdrawn, and the first sign of
trouble would be the tool simply not working any more.

So: fetch on demand. Run it when you want to know when the next one leaves. Do
not put it in a cron job, do not poll it from a status line at a tight interval,
and if you do want repeated calls, turn the cache on so the repeats are served
locally. Being a considerate client here is not politeness, it is the only thing
keeping the service available.

## Troubleshooting

Exit code 0 is a normal board.

Exit code 1 is a runtime error: usually the network, or an upstream API
returning an error for a stop id it does not recognise. The message names the
URL it tried. First thing to check is whether the id in the configuration is
still valid for the backend being used, since stop ids are per-provider.

Exit code 2 is configuration validation: the file was found and rejected. The
message says which field. First thing to check is the file the error names,
which is the private configuration and not anything in this repository. A
mistyped profile key lands here too, with the configured keys listed, since
`board` takes either a profile or raw stop ids and has to tell you which it
thought you meant.

Exit code 3 is not implemented. Only `route` returns it today. Journey planning
is not built yet, so use a map for that.

If the command fails before any of those, with a message about a missing
configuration file, see the public/private split above: the `transit_config_file`
knob names the file it looked for. If it fails naming a missing package
directory or a missing CLI entry point, the `transit_dir` knob names where it
looked; it is already correct for a checkout of this repository, so a failure
there usually means an incomplete checkout.
