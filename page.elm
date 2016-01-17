module Page  where

import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Bootstrap.Html as B exposing (..)
import List exposing (..)
import Char exposing (toUpper)
import String

(=>) = (,)

main =
  let buttonList = ["default", "primary", "success", "info", "warning", "danger", "link"]
      lg = List.map (makeBtns "btn-lg") buttonList
      de = List.map (makeBtns "") buttonList
      sm = List.map (makeBtns "btn-sm") buttonList
      xs = List.map (makeBtns "btn-xs") buttonList
  in div [] [
        navbarMain
       , div [class "container theme-showcase"
             , attribute "role" "main"] [
            jumbotron
           , buttons
           , p [] lg
           , p [] de
           , p [] sm
           , p [] xs
           ]
       ]

navbarMain : Html
navbarMain =
  node "nav" [class "navbar navbar-inverse navbar-fixed-top"]
         [ div [class "container"]
             [ div [class "navbar-header"]
                 [ button [type' "button"
                          , class "navbar-toggle collapsed"
                          , attribute "data-toggle" "collapse"
                          , attribute "data-target" "#navbar"
                          , attribute "aria-expanded" "false"
                          , attribute "aria-controls" "navbar"
                          ] [span [class "sr-only"] [text "Toggle navigation"]
                            , span [class "icon-bar"] []
                            , span [class "icon-bar"] []
                            , span [class "icon-bar"] []
                            ]
                 , a [class "navbar-brand", href "#"] [text "Bootstrap theme"]
                 ]
             , div [id "navbar", class "navbar-collapse collapse"]
                 [ ul [class "nav navbar-nav"]
                     [ li [class "active"] [a [href "#"] [text "Home"]]
                     , li [] [a [href "#about"] [text "About"]]
                     , li [] [a [href "#contact"] [text "Contact"]]
                     , li [ class "dropdown" ]
                         [ a [class "dropdown-toggle"
                             , href "#"
                             , attribute "data-toggle" "dropdown"
                             , attribute "role" "button"
                             , attribute "aria-haspopup" "true"
                             , attribute "aria-expanded" "false"
                             ]
                             [ text "Dropdown "
                             , span [ class "caret" ] []
                             ]
                         , ul [class "dropdown-menu"] [
                                 li [] [a [href "#"] [text "Action"]]
                                , li [] [a [href "#"] [text "Another action"]]
                                , li [] [a [href "#"] [text "Something else here"]]
                                , li [class "divider", attribute "role" "seperator"] []
                                , li [class "dropdown-header"] [text "Nav header"]
                                , li [] [a [href "#"] [text "Seperated link"]]
                                , li [] [a [href "#"] [text "One more seperated link"]]
                                ]
                         ]
                     ]
                 ]
             ]
         ]



jumbotron : Html
jumbotron =
      div [class "jumbotron"] [
             h1 [] [text "Theme Example"]
        , p [] [text "This is a template showcasing the optional theme stylesheet included in Bootstrap. Use it as a starting point to create something more unique by building on or modifying it."]
        ]

buttons : Html
buttons =
  div [class "page-header"] [h1 [] [text "Buttons"]]

makeBtns : String -> String -> Html
makeBtns prop name =
  button [type' "button"
         , class ("btn " ++ prop ++ " btn-" ++ name)
         , style [("margin-left" => "5px")]]
    [text <| getLinkName name]

getLinkName : String -> String
getLinkName name = let f = String.left 1 name
                       l = String.dropLeft 1 name
                       g = String.map toUpper f
                   in g ++ l
