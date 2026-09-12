# Supplied MOVE routes during replay

Replay checks a supplied transparent route against the traversal providers, segment
by segment, using the action's input state. Every segment must start where the
previous one ended and match the provider's mode, destination, and gate witness.
The agent must start grounded, and the route must end at a different location.
A support transition remains a separate singleton MOVE and must match a current
configuration-provider transition; it cannot be concatenated with a grounded route.

Search still generates one canonical shortest route per destination. Replay binds
`*replay-action*` dynamically in `apply-action-to-state`; only MOVE's action-specific
result selection uses it. The ordinary movement query remains canonical. Display
connectives are stripped before the supplied action is exposed to providers.

An accepted supplied route uses the normal MOVE effect, cost, propagation, and
happening handling once, at its endpoint. Transparent route segments do not become
separate action boundaries. Other actions retain exact generated-argument matching.

Regression: after loading Wouldwork, load `test/crelay-route-replay.lisp`.
It stages Crelay-Topo and replays the original 27-action prefix, including the
noncanonical location6 -> location4 -> location5 route. Checks cover the endpoint,
action time, existing support transitions, disconnected routes, wrong gate witnesses,
closed gates, illegal destinations, empty routes, mixed support/ground routes,
input-state preservation, and unchanged canonical movement results after replay.
No search is run.