module Test.Main where

import Prelude

import React (ReactElement, createFactory)
import React.DOM as D
import React.DOM.Props as P
import React.Explore (Component, UI, explore)
import React.Explore.Sum as Sum
import Control.Comonad.Cofree (Cofree, mkCofree, head, tail)
import Control.Comonad.Store (StoreT, store)
import Control.Comonad.Traced (TracedT, traced)
import Control.Monad.Eff (Eff)
import Control.Monad.State (modify)
import Control.Monad.Writer (tell)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element, ElementId(..), documentToNonElementParentNode)
import Data.Functor.Pairing.Co (Co, co)
import Data.Identity (Identity)
import Data.Lazy (Lazy, defer, force)
import Data.Maybe (fromJust)
import Data.Monoid.Additive (Additive(..))
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import ReactDOM (render)

-- | A counter component implemented using the `Store` comonad.
storeExample :: Component (StoreT Int Identity)
storeExample = store render 0 where
  render :: Int -> UI (Co (StoreT Int Identity) Unit)
  render count send =
    D.div' [ D.p' [ D.text ("State: " <> show count) ]
           , D.button [ P.onClick \_ ->
                         send (modify (add 1))
                      ]
                      [ D.text "Increment"
                      ]
           , D.button [ P.onClick \_ ->
                         send (modify (_ `sub` 1))
                      ]
                      [ D.text "Decrement"
                      ]
           ]

-- | A counter component implemented using the `Traced` comonad.
tracedExample :: Component (TracedT (Additive Int) Identity)
tracedExample = traced render where
  render :: Additive Int -> UI (Co (TracedT (Additive Int) Identity) Unit)
  render (Additive count) send =
    D.div' [ D.p' [ D.text ("State: " <> show count) ]
           , D.button [ P.onClick \_ ->
                          send (tell (Additive 1))
                      ]
                      [ D.text "+ 1"
                      ]
           , D.button [ P.onClick \_ ->
                          send (tell (Additive 10))
                      ]
                      [ D.text "+ 10"
                      ]
           , D.button [ P.onClick \_ ->
                          send (tell (Additive 100))
                      ]
                      [ D.text "+ 100"
                      ]
           ]

-- | A counter component implemented using a `Cofree` comonad.
cofreeExample :: Component (Cofree Lazy)
cofreeExample = iterCofree 0 step where
  moveRight :: Co (Cofree Lazy) Unit
  moveRight = co \x -> head (force (tail x)) unit

  iterCofree :: forall a s f. Functor f => s -> (s -> Tuple a (f s)) -> Cofree f a
  iterCofree s f =
    case f s of
      Tuple a fs -> mkCofree a (map (_ `iterCofree` f) fs)

  step :: Int -> Tuple (UI (Co (Cofree Lazy) Unit)) (Lazy Int)
  step count = Tuple ui (defer \_ -> add count 1) where
    ui :: UI (Co (Cofree Lazy) Unit)
    ui send =
      D.div' [ D.p' [ D.text ("State: " <> show count) ]
             , D.button [ P.onClick \_ ->
                            send moveRight
                        ]
                        [ D.text "Increment"
                        ]
             ]

main :: forall eff. Eff (dom :: DOM | eff) Unit
main = void (elm' >>= render ui) where
  together = map addControls
    (storeExample `Sum.combine` tracedExample `Sum.combine` cofreeExample)

  addControls render send =
    D.div' [ D.p' [ D.a [ P.onClick \_ -> send (Sum.moveLeft *> Sum.liftLeft Sum.moveLeft)
                        , P.href "#"
                        ]
                        [ D.text "Store example"
                        ]
                  , D.text " — "
                  , D.a [ P.onClick \_ -> send (Sum.moveLeft *> Sum.liftLeft Sum.moveRight)
                        , P.href "#"
                        ]
                        [ D.text "Traced example"
                        ]
                  , D.text " — "
                  , D.a [ P.onClick \_ -> send Sum.moveRight
                        , P.href "#"
                        ]
                        [ D.text "Cofree example"
                        ]
                  ]
           , render send
           ]

  ui :: ReactElement
  ui = D.div' [ createFactory (explore together) {} ]

  elm' :: Eff (dom :: DOM | eff) Element
  elm' = do
    win <- window
    doc <- document win
    elm <- getElementById (ElementId "example") (documentToNonElementParentNode (htmlDocumentToDocument doc))
    pure $ unsafePartial fromJust elm
