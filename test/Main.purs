module Test.Main where

import Prelude

import Control.Comonad.Cofree (Cofree, buildCofree)
import Control.Comonad.Store (StoreT, store)
import Control.Comonad.Traced (TracedT, traced)
import Control.Monad.Free.Class (wrapFree)
import Control.Monad.State (modify)
import Control.Monad.Writer (tell)
import Data.Functor.Pairing.Co (Co, co)
import Data.Identity (Identity)
import Data.Lazy (Lazy, defer, force)
import Data.Maybe (fromJust)
import Data.Monoid.Additive (Additive(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import React (ReactElement, unsafeCreateLeafElement)
import React.DOM as D
import React.DOM.Props as P
import React.Explore (Component, UI, explore)
import React.Explore.List as List
import React.Explore.Sum as Sum
import ReactDOM (render)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.Window (document)

-- | A counter component implemented using the `Store` comonad.
storeExample :: Component (StoreT Int Identity)
storeExample = store render 0 where
  render :: Int -> UI (Co (StoreT Int Identity) Unit)
  render count send =
    D.div' [ D.p' [ D.text ("State: " <> show count) ]
           , D.button [ P.onClick \_ ->
                         send (void (modify (add 1)))
                      ]
                      [ D.text "Increment"
                      ]
           , D.button [ P.onClick \_ ->
                         send (void (modify (_ `sub` 1)))
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
cofreeExample = buildCofree step 0 where
  moveRight :: Co Lazy Unit
  moveRight = co \a -> force a unit

  step :: Int -> Tuple (UI (Co (Cofree Lazy) Unit)) (Lazy Int)
  step count = Tuple ui (defer \_ -> add count 1) where
    ui :: UI (Co (Cofree Lazy) Unit)
    ui send =
      D.div' [ D.p' [ D.text ("State: " <> show count) ]
             , D.button [ P.onClick \_ ->
                            send (wrapFree (moveRight $> pure unit))
                        ]
                        [ D.text "Increment"
                        ]
             ]

main :: Effect Unit
main = void (elm' >>= render ui) where
  together = map addControls
    (stores `Sum.combine` tracedExample `Sum.combine` cofreeExample)

  addControls render send =
    D.div' [ D.p' [ D.a [ P.onClick \_ -> send (Sum.moveLeft *> Sum.liftLeft Sum.moveLeft)
                        , P.href "#"
                        ]
                        [ D.text "Stores example"
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

  stores = map withButton (List.listOf D.div' storeExample) where
    withButton render send =
      D.div' [ D.p' [ D.a [ P.onClick \_ -> send List.push
                          , P.href "#"
                          ]
                          [ D.text "Add Store"
                          ]
                    ]
             , render send
             ]

  ui :: ReactElement
  ui = D.div' [ unsafeCreateLeafElement (explore together) {} ]

  elm' = do
    win <- window
    doc <- document win
    elm <- getElementById "example" (toNonElementParentNode doc)
    pure $ unsafePartial fromJust elm
