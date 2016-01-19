module Page  where

import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Bootstrap.Html as B exposing (..)
import List exposing (..)
import Char exposing (toUpper)
import Maybe exposing (withDefault)
import Array as Array exposing (initialize, toList)
import String

(=>) = (,)

main =
  let buttonList = ["default", "primary", "success", "info", "warning", "danger", "link"]
      lg = List.map (makeBtns "btn-lg") buttonList
      de = List.map (makeBtns "") buttonList
      sm = List.map (makeBtns "btn-sm") buttonList
      xs = List.map (makeBtns "btn-xs") buttonList
  in div [] [
        makeNavbar "navbar navbar-inverse navbar-fixed-top"
       , div [class "container theme-showcase"
             , attribute "role" "main"] [
            jumbotron
           , buttons
           , p [] lg
           , p [] de
           , p [] sm
           , p [] xs
           , div [class "page-header"] [h1 [] [text "Tables"]]
           , makeTables "table"
           , makeTables "table table-striped"
           , makeTables "table table-bordered"
           , makeTables "table table-condensed"
           , makeThumbnails
           , div [class "page-header"] [h1 [] [text "Labels"]]
           , makeLabels [h1, h2, h3, h4, h5, h6]
           , div [class "page-header"] [h1 [] [text "Badges"]]
           , makeBadges
           , div [class "page-header"] [h1 [] [text "Dropdown menus"]]
           , makeDropDown
           , div [class "page-header"] [h1 [] [text "Navs"]]
           , makeNavs
           , div [class "page-header"] [h1 [] [text "Navbars"]]
           , makeNavbar "navbar navbar-default"
           , makeNavbar "navbar navbar-inverse"
           , makeAlerts
           , makeProgressBars
           , makeListGroups
           , makePanels
           , makeWells
           , makeCarousel
           ]
       ]

makeNavbar : String -> Html
makeNavbar cl =
  node "nav" [class cl]
         [ div [class "container"]
             [ div [class "navbar-header"]
                 [a [class "navbar-brand", href "#"]
                    [text "Bootstrap theme"]
                 ]
             , div [id "navbar", class "navbar-collapse collapse"]
                 [ ul [class "nav navbar-nav"]
                     [ li [class "active"] [a [href "#"] [text "Home"]]
                     , li [] [a [href "#about"] [text "About"]]
                     , li [] [a [href "#contact"] [text "Contact"]]
                     , li [ class "dropdown" ]
                         [ a ([class "dropdown-toggle"
                             , href "#"
                             ] ++ baseDropDownAttributes)
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

{-|
Tables
-}
-- Tables
makeTables : String -> Html
makeTables tableType =
  div [class "row"]
        [ div [class "col-md-6"]
            [ table [class tableType]
                [thead []
                   [tr []
                      [th [] [text "#"]
                      , th [] [text "First Name"]
                      , th [] [text "Last Name"]
                      , th [] [text "Username"]
                      ]
                   ]
                , makeRow row1
                , makeRow row2
                , makeRow row3
                ]
            ]
        ]

makeRow xs = tbody [] [tr [] (List.map (\x -> th [] [text x]) xs)]

row0 = ["#", "First Name", "Last Name", "Username"]
row1 = ["1", "Mark", "Otto", "@mdo"]
row2 = ["2", "Jacob", "Thornton", "@fat"]
row3 = ["3", "Larry", "Bird", "@twitter"]

{-|
Labels
-}
makeLabels hs =
  let lbls = ["default", "primary", "success", "info", "warning", "danger"]
  in div [] (List.map (\x -> x [] (makeSpans lbls)) hs)

makeSpans : List String -> List Html
makeSpans ls =
  List.map (\x -> span [class <| "label label-" ++ x, style [("margin-left" => "5px")]]
              [text <| getLinkName x]) ls


{-|
Buttons
-}
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

{-|
Badges
-}
makeBadges : Html
makeBadges =
  div [] [p [] [a [href "#"] [text "Inbox"]
               , span [class "badge"] [text "42"]
               ]
         , ul [class "nav nav-pills", attribute "role" "tablist"]
            [ li [attribute "role" "presentation"
                 , class "active"
                 ] [ a [href "#"] [
                          text "Home"
                         , span [class "badge"] [text "42"]
                         ]
                   ]
            , li [attribute "role" "presentation"]
                [ a [href "#"] [
                       text "Profile"
                      ]
                ]
            , li [attribute "role" "presentation"]
                [ a [href "#"]
                    [text "Messages"
                    , span [class "badge"] [text "3"]
                    ]
                ]
            ]
         ]


{-|
DropDown
-}
makeDropDown : Html
makeDropDown =
  div [class "dropdown theme-dropdown clearfix"]
      [ a ([id "dropdownMenu1"
          , href "#"
          , class "sr-only dropdown-toggle"
          ] ++ baseDropDownAttributes)
          [text "Dropdown"
          , span [class "caret"] []
          ]
      , ul [class "dropdown-menu"
           , attribute "aria-labelledby" "dropdownMenu1"
           ]
          [ li [class "active"] [a [href "#"] [text "Action"]]
          , li [] [a [href "#"] [text "Another action"]]
          , li [] [a [href "#"] [text "Something else here"]]
          , li [attribute "role" "seperator"
               , class "divider"] []
          , li [] [a [href "#"] [text "Seperated link"]]
          ]
      ]

baseDropDownAttributes : List Attribute
baseDropDownAttributes =
  [ attribute "data-toggle" "dropdown"
  , attribute "role" "button"
  , attribute "aria-haspopup" "true"
  , attribute "aria-expanded" "false"
  ]

{-|
Navs
-}
makeNavs : Html
makeNavs =
  let c =  [ li [attribute "role" "presentation", class "active"] [a [href "#"] [text "Home"]]
           , li [attribute "role" "presentation"] [a [href "#"] [text "Profile"]]
           , li [attribute "role" "presentation"] [a [href "#"] [text "Messages"]]
           ]
  in div [] [
        ul [class "nav nav-tabs"
           , attribute "role" "tablist"
           ]
          c
       , ul [class "nav nav-pills"
            , attribute "role" "tablist"
            ]
          c
       ]

{-|
Alerts
-}
makeAlerts : Html
makeAlerts =
  let d ty (s,o) = div [class <| "alert alert-" ++ ty, attribute "role" "alert"] [strong [] [text s], text o]
      words = ["Well done!" => " You successfully read this important alert message."
              , "Heads up!" => " This alert needs your attention, but it's not super important."
              , "Warning!" => " Best check yo self, you're not looking too good."
              , "Oh snap!" => " Change a few things up and try submitting again."
              ]
      typs = ["success", "info", "warning", "danger"]
  in div [] [
        div [class "page-header"] [h1 [] [text "Navbars"]]
       , div [] (List.map2 d typs words)
       ]

{-|
Progress bars
-}
makeProgressBars : Html
makeProgressBars =
  let sd c cs p = div [class "progress"]
                  [div (List.concat [[class <| "progress-bar progress-bar-" ++ c], cs])
                     [span [class "sr-only"] [text <| p ++ "% Complete"]]
                  ]
      cls p = [
       attribute "role" "progressbar"
      , attribute "aria-valuenow" p
      , attribute "aria-valuemin" "0"
      , attribute "aria-valuemax" "100"
      , style [("width" => (p ++ "%"))]
      ]
      c' = ["success", "info", "warning", "danger", "striped"]
      p' = ["60", "40", "20", "80", "60", "80", "60"]
  in div [] [
        div [class "page-header"] [h1 [] [text "Progress bars"]]
       , div [] (List.map3 sd c' (List.map cls p') p')
       ]

{-|
List groups
-}
makeListGroups : Html
makeListGroups =
  let gps = Array.toList <| Array.initialize 8 (\x -> "Group " ++ toString (x + 1))
      lis g = li [class "list-group-item"] [text g]
      ais g = a [href "#"
                , class "list-group-item"] [text g]
      ais' = List.map ais (withDefault [] <| tail gps)
      ais'' = a [href "#", class "list-group-item active"] [text (withDefault "" <| head gps)]
      go c = div [class "col-sm-4"] [ ul [ class "list-group"] c]
  in div [] [
        div [class "page-header"] [h1 [] [text "List groups"]]
       , div [class "row"] [ go (List.map lis gps)
                           , go (ais'' :: ais')
                           , go makeListGroup'
                           ]
       ]

makeListGroup' = [a [href "#", class "list-group-item active"]
                    [ h4 [class "list-group-item-heading"] [text "List group item heading"]
                    , p [class "list-group-item-text"] [text "Donec id elit non mi porta gravida at eget metus. Maecenas sed diam eget risus varius blandit."]]
                 , a [href "#", class "list-group-item"]
                    [ h4 [class "list-group-item-heading"] [text "List group item heading"]
                    , p [class "list-group-item-text"] [text "Donec id elit non mi porta gravida at eget metus. Maecenas sed diam eget risus varius blandit."]]
                 , a [href "#", class "list-group-item"]
                    [ h4 [class "list-group-item-heading"] [text "List group item heading"]
                    , p [class "list-group-item-text"] [text "Donec id elit non mi porta gravida at eget metus. Maecenas sed diam eget risus varius blandit."]]
                 ]

{-|
Panels
-}
makePanels : Html
makePanels =
  let base s = div [class <| "panel panel-" ++ s]
                   [ div [class "panel-heading"]
                       [h3 [class "panel-title"] [text "Panel title"]
                       ]
                   , div [class "panel-body"] [text "Panel content"]
                   ]
      two1 = div [class "col-sm-4"] <| List.map base ["default", "primary"]
      two2 = div [class "col-sm-4"] <| List.map base ["success", "info"]
      two3 = div [class "col-sm-4"] <| List.map base ["warning", "danger"]
      res = div [class "row"] [two1 , two2 , two3]
  in h' "Panels" res

{-|
Wells
-}
makeWells : Html
makeWells =
  let blah = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Maecenas sed diam eget risus varius blandit sit amet non magna. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Praesent commodo cursus magna, vel scelerisque nisl consectetur et. Cras mattis consectetur purus sit amet fermentum. Duis mollis, est non commodo luctus, nisi erat porttitor ligula, eget lacinia odio sem nec elit. Aenean lacinia bibendum nulla sed consectetur."
  in h' "Wells" <| div [class "well"] [text blah]

{-|
Carousel
-}
makeCarousel : Html
makeCarousel =
  let ols = div [id "carousel-example-generic", class "carousel slide", attribute "data-ride" "carousel"]
            [ol [class "carousel-indicators"]
               [li [class "active",
                      attribute "data-target" "#carousel-example-generic"
                   ,  attribute "data-slide-to" "0"] []
               , li [attribute "data-target" "#carousel-example-generic"
                   ,  attribute "data-slide-to" "1"] []
               , li [attribute "data-target" "#carousel-example-generic"
                   ,  attribute "data-slide-to" "2"] []
               ]
            , div [class "carousel-inner", attribute "role" "listbox"]
               [div [class "item active"]
                  [img [attribute "data-src" "holder.js/1140x500/auto/#777:#555/text:First slide"
                       , alt "First slide"] []]
               , div [class "item"]
                  [img [attribute "data-src" "holder.js/1140x500/auto/#666:#444/text:Second slide"
                       , alt "Second slide"] []]
               , div [class "item"]
                  [img [attribute "data-src" "holder.js/1140x500/auto/#666:#444/text:Third slide"
                       , alt "Third slide"] []]
               ]
            , a [class "left carousel-control"
                , href "#carousel-example-generic"
                , attribute "role" "button"
                , attribute "data-slide" "prev"]
               [span [class "glyphicon glyphicon-chevron-left"
                     , attribute "aria-hidden" "true"] []
               , span [class "sr-only"] [text "Previous"]
               ]
            , a [class "right carousel-control"
                , href "#carousel-example-generic"
                , attribute "role" "button"
                , attribute "data-slide" "next"]
               [span [class "glyphicon glyphicon-chevron-right"
                     , attribute "aria-hidden" "true"] []
               , span [class "sr-only"] [text "Next"]
               ]
            ]
  in h' "Carousel" ols

{-|
Thumbnails
-}
makeThumbnails : Html
makeThumbnails =
  let alt' = "A generic square placeholder image with a white border around it, making it resemble a photograph taken with an old instant camera"
  in h' "Thumbnails" <| img [class "img-thumbnail"
                          , attribute "data-src" "holder.js/200x200"
                          , alt alt'] []

{-|
Page header helper
-}
-- h' : List Html -> Html
h' n c = div [] [
          div [class "page-header"] [h1 [] [text n]]
         , div [class "row"] [c]
         ]
