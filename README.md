# node-debugger

This is an implementation of a state machine for troubleshooting the TLA+ modeling work that is happening [here](https://github.com/simplestaking/model-p2p).

## Initial states

To interact with the state machine, one must first create an initial state. This can be done by using the functionality in `lib/initial_state\initial_state.ml` or one can simply create an essentially empty initial state with the `init` function in `lib/node_info/node_info.ml`.

Once an initial state is created, we can apply actions and see what effect they have on the state.

## Viewing states and trace

There is functionality to view the state in `lib/node_info/node_info.ml` and the execution trace in `lib/execution/execution.ml`.

## Enabling conditions

There is also functionality to print out a list of all enabled actions in the current state. These are the actions that one may apply to advance the state.

## Getting started

Clone the repo and use `dune utop` to load all the modules into the OCaml toplevel.

Now I like to do:

```ocaml
open Basic;;
open Execution;;
open Node_info;;
```

because it makes the whole experience a lot less painful.

Next, we create an initial state, e.g. `let state = init 3`, but you can certainly do something fancier with `Initial_state`. I'll call the state `state`.

Now we can view the enabled actions in `state` with `enabled state`. For example, you will see something like:

```txt
Enabled system actions:
  New_chain
  New_branch
  New_block
Enabled node actions:
  Activate
    node 1 :> chain(s) 1
    node 2 :> chain(s) 1
    node 3 :> chain(s) 1
```

This displays the enabled node and system actions. It says the system actions `New_chain`, `New_branch`, and `New_block` are enabled (these actions are always enabled) and the node actions `Activate (1, 1)`, `Activate (2, 1)`, and `Activate (3, 1)` are enabled (since the nodes are currently inactive on chain 1, they can become active on chain 1; chain 1 is currently the only chain that exists).

Now we can apply any of these enabled actions, e.g. let's apply `Activate (2, 1)` (node 2 becomes active on chain 1). To do this, we enter `activate' state 2 1`. At this point, we can `print view state` to display the entire state or `print (view_node' state) 2` to only display node 2's state or `print (view_chain' state) 1` to only display chain 1's state.

Now if we display the enabled actions, we get some new options:

```txt
Enabled system actions:
  New_chain
  New_branch
  New_block
  Advertise: Current_branch on chain(s) 1
  Advertise: Current_head
    chain 1 :> branch(es) 0
Enabled node actions:
  Activate
    node 1 :> chain(s) 1
    node 3 :> chain(s) 1
  Deactivate
    node 2 :> chain(s) 1
```

At any point, we can apply any enabled action to the state or view any part of the state.
