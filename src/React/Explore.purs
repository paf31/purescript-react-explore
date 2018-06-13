-- | We can think of user interfaces as exploring a (pointed) space of states.
-- | We can model that space using a comonad, and different choices of comonad
-- | correspond to different UI patterns:
-- |
-- | - Store corresponds to something like React, where we just have a state,
-- |   and a function taking each state to a user interface.
-- | - The Traced comonad corresponds to something like an incremental game,
-- |   where we have a state for every value in some monoid.
-- | - Mealy machines are more like the Elm architecture, where we respond to
-- |   input events, and update some internal state.
-- | - A cofree comonad is a bit like Halogen. We have some functor which
-- |   describes the transitions to new states, but which can also respond
-- |   to queries on the current state.
-- |
-- | We can use `Co w` to explore the state space. `Co w` actions will be
-- | connected to the user interface. For example:
-- |
-- | - `Co (Store s)` is isomorphic to `State s`, so we can use get and put to
-- |   read and write the stored state.
-- | - `Co (Traced w)` is isomorphic to `Writer`, so we can use `tell` to append some monoidal
-- |   value to our state.
-- | - `Mealy action` is `Cofree (Function action)`, so `Co (Mealy action)` is isomorphic to
-- |   `Free (Tuple action)`. We can emit zero or more actions in response to
-- |   each user event.
-- | - `Co (Cofree f)` is isomorphic to `Free g` whenever `f` pairs with `g`. This
-- |   corresponds to something like the Halogen API.
module React.Explore where

import Prelude

import Control.Comonad (class Comonad, extend, extract)
import Data.Functor.Pairing.Co (Co, runCo)
import Effect (Effect)
import React as R

-- | A `Handler` takes an action and modifies the React component state.
type Handler a = a -> Effect Unit

-- | A UI, which is parameterized by its type of actions. For the purposes of
-- | this implementation, a `UI` is just a `ReactElement` which takes its event
-- | `Handler` as an explicit argument.
type UI a = Handler a -> R.ReactElement

-- | A `Component` is a comonad `w` full of future `UI`s. Those `UI`s can dispatch
-- | actions in the `Co w` monad in order to explore the state space.
type Component w = w (UI (Co w Unit))

-- | Explore a space of states specified by some comonad, and defined by a
-- | value in that comonad.
-- |
-- | This function creates a `ReactClass` which can be rendered using React.
-- | See the test project for an example.
explore :: forall w props. Comonad w => Component w -> R.ReactClass {| props }
explore space = 
  R.pureComponent "ReactExplore" \this -> 
    pure { state: { space }
         , render: do
             { space: state } <- R.getState this
             let send :: Co w Unit -> Effect Unit
                 send m = R.modifyState this \_ -> { space: runCo m (extend const state) }
             pure (extract state send)
         }
