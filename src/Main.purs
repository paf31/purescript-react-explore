module Main where

import React
import React.DOM as D
import React.DOM.Props as P
import Control.Comonad (class Comonad, extract, duplicate)
import Control.Comonad.Cofree (Cofree, mkCofree, head, tail)
import Control.Comonad.Store (StoreT, store)
import Control.Comonad.Traced (TracedT, traced)
import Control.Monad (pure, bind, (>>=))
import Control.Monad.Eff (Eff)
import Control.Monad.State (modify)
import Control.Monad.Writer (tell)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element, ElementId(..), documentToNonElementParentNode)
import Data.Function (id, const, (<<<), ($))
import Data.Functor (class Functor, map, void)
import Data.Functor.Day (Day, day, runDay)
import Data.Functor.Pairing (type (⋈), sym)
import Data.Functor.Pairing.Co (Co, runCo, co, pairCo)
import Data.Identity (Identity)
import Data.Lazy (Lazy, defer, force)
import Data.Maybe (fromJust)
import Data.Monoid.Additive (Additive(..))
import Data.Nullable (toMaybe)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import Partial.Unsafe (unsafePartial)
import Prelude (add, show)
import ReactDOM (render)

type Handler a = forall eff. a -> Eff (state :: ReactState ReadWrite | eff) Unit

type UI a = Handler a -> ReactElement

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
-- | Other comonads might be worth looking at.
-- |
-- | If we can pair the comonad with some monad `m`, then we can use `m` to
-- | explore the state space. `m` actions will be connected to the user
-- | interface. For example:
-- |
-- | - `Store` pairs with `State`, so we can use get and put to read and write the
-- |   stored state.
-- | - `Traced` pairs with `Writer`, so we can use `tell` to append some monoidal
-- |   value to our state.
-- | - `Mealy action` is `Cofree (Function action)`, so pairs with
-- |   `Free (Tuple action)`. We can emit zero or more actions in response to
-- |   each user event.
-- | - `Cofree f` in general pairs with `Free g`, where `f` pairs with `g`. This
-- |   corresponds to what we use in Halogen.
explore :: forall w m props. Comonad w => m ⋈ w -> w (UI (m Unit)) -> ReactClass props
explore pair space = createClass (spec space render) where
  render :: ReactThis props (w (UI (m Unit))) -> Eff _ ReactElement
  render this = do
    state <- readState this
    let send :: forall eff. m Unit -> Eff (state :: ReactState ReadWrite | eff) Unit
        send m = transformState this (pair (const id) m <<< duplicate)
    pure (extract state send)

type Component w = w (UI (Co w Unit))

-- | We don't need to bother with the explicit pairing, we can use the one which
-- | comes for free from the `Co` monad.
-- |
-- | Type class instances are provided for the MTL classes, so we can use `Co` as
-- | our action monad using the functions defined by those classes.
exploreCo :: forall w props. Comonad w => Component w -> ReactClass props
exploreCo = explore (sym pairCo)

-- | A counter component implemented using the `Store` comonad.
storeExample :: Component (StoreT Int Identity)
storeExample = store render 0 where
  render :: Int -> UI (Co (StoreT Int Identity) Unit)
  render count send =
    D.button [ P.onClick \_ ->
                 send (modify (add 1))
             ]
             [ D.text (show count)
             , D.text " Increment"
             ]

-- | A counter component implemented using the `Traced` comonad.
tracedExample :: Component (TracedT (Additive Int) Identity)
tracedExample = traced render where
  render :: Additive Int -> UI (Co (TracedT (Additive Int) Identity) Unit)
  render (Additive count) send =
    D.button [ P.onClick \_ ->
                 send (tell (Additive 1))
             ]
             [ D.text (show count)
             , D.text " Increment"
             ]

-- | A counter component implemented using a `Cofree` comonad.
cofreeExample :: Component (Cofree Lazy)
cofreeExample = iterCofree 0 step where
  iterCofree :: forall a s f. Functor f => s -> (s -> Tuple a (f s)) -> Cofree f a
  iterCofree s f =
    case f s of
      Tuple a fs -> mkCofree a (map (_ `iterCofree` f) fs)

  step :: Int -> Tuple (UI (Co (Cofree Lazy) Unit)) (Lazy Int)
  step count = Tuple ui (defer \_ -> add count 1) where
    ui :: UI (Co (Cofree Lazy) Unit)
    ui send =
      D.button [ P.onClick \_ ->
                   -- This could definitely be nicer.
                   -- Ideally, Co w would have a MonadFree instance.
                   -- Another option is to work directly with the pairing
                   -- between Cofree and Free.
                   send (co \x -> head (force (tail x)) unit)
               ]
               [ D.text (show count)
               , D.text " Increment"
               ]

-- | To combine two components, we can take the Day convolution of their state
-- | spaces.
-- |
-- | This is a bit like taking the smash product of pointed topological spaces.
combine :: forall w1 w2
         . (Comonad w1, Comonad w2)
        => (forall a. UI a -> UI a -> UI a)
        -> Component w1
        -> Component w2
        -> Component (Day w1 w2)
combine with = day build where
  build :: UI (Co w1 Unit) -> UI (Co w2 Unit) -> UI (Co (Day w1 w2) Unit)
  build render1 render2 = with (\send -> render1 \co -> send (introCoDay1 co))
                               (\send -> render2 \co -> send (introCoDay2 co))

-- Helper functions

introCoDay1 :: forall w w' a. (Functor w, Comonad w') => Co w a -> Co (Day w w') a
introCoDay1 a = co (runDay \f w w' -> runCo a (map (_ `f` extract w') w))

introCoDay2 :: forall w w' a. (Functor w, Comonad w') => Co w a -> Co (Day w' w) a
introCoDay2 a = co (runDay \f w' w -> runCo a (map (f (extract w')) w))

-- I have no idea what's going on here.
-- This seems to imply that Co (Day f g) should be equivalent somehow to
-- Day (Co f) (Co g), when f and g are comonads,
-- but one is a monad and the other isn't.
pairDay :: forall f1 f2 g1 g2. f1 ⋈ g1 -> f2 ⋈ g2 -> Day f1 g1 ⋈ Day f2 g2
pairDay p1 p2 f day1 day2 =
  runDay (\g f1 g1 ->
    runDay (\h f2 g2 ->
      p1 (\x1 y2 -> f (g x1 y2) (p2 h f2 g2)) f1 g1) day2) day1

main :: forall eff. Eff (dom :: DOM | eff) Unit
main = void (elm' >>= render ui)
  where
  together = combine (\x y send ->
    D.div' [ D.h1' [ D.text "Store" ]
           , x send
           , y send
           ]) storeExample (combine (\x y send ->
             D.div' [ D.h1' [ D.text "Traced" ]
                    , x send
                    , D.h1' [ D.text "Cofree" ]
                    , y send
                    ]) tracedExample cofreeExample)

  ui :: ReactElement
  ui = D.div' [ createFactory (exploreCo together) {} ]

  elm' :: Eff (dom :: DOM | eff) Element
  elm' = do
    win <- window
    doc <- document win
    elm <- getElementById (ElementId "example") (documentToNonElementParentNode (htmlDocumentToDocument doc))
    pure $ unsafePartial fromJust (toMaybe elm)
