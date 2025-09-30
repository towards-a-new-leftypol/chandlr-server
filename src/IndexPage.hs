{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module IndexPage where

import Miso (ToView (..))
import Miso.Html.Property
    ( charset_
    , name_
    , content_
    , rel_
    , href_
    , type_
    , class_
    , src_
    , language_
    , defer_
    )
import Miso.Html
    ( ToHtml (..)
    , doctype_
    , html_
    , head_
    , meta_
    , link_
    , body_
    , script_
    )
import Miso.Html.Element (title_)
import Miso.String (toMisoString)
import Data.Aeson (ToJSON, encode)
import Data.ByteString (toStrict)
import qualified Data.ByteString.Base64 as B64

import Common.FrontEnd.JSONSettings
import qualified Common.FrontEnd.Model  as FE

data IndexPage a = forall b. (ToJSON b, ToView FE.Model a) => IndexPage (JSONSettings, b, a)

instance ToHtml (IndexPage a) where
    toHtml (IndexPage (settings, initial_data, x)) = toHtml
        [ doctype_
        , html_
            []
            [ head_
                []
                (
                    [ meta_ [ charset_ "utf-8" ]
                    , meta_
                        [ name_ "viewport"
                        , content_ "width=device-width, initial-scale=1.0"
                        ]
                    ] 
                    ++
                    (asHtml settings)
                    ++
                    [ script_
                        [ class_ "initial-data"
                        , type_ "text/plain"
                        ]
                        (toMisoString $ B64.encode $ toStrict $ encode initial_data)

                    , title_ [] [ "Chandlr" ]

                    , js_wasm $ static_root <> "/init.js"
                    --, js_js $ static_root <> "/all.js"
                    , css $ static_root <> "/style.css"
                    ]
                )
            , body_ [] [ toView @FE.Model x ]
            ]
        ]

        where
            static_root = static_serve_url_root settings

            css href =
                link_
                    [ rel_ "stylesheet"
                    , type_ "text/css"
                    , href_ $ toMisoString href
                    ]

            js_wasm href =
                script_
                    [ type_ "module"
                    , src_ $ toMisoString href
                    ]
                    ""

            js_js href =
                script_
                    [ language_ "javascript"
                    , src_ $ toMisoString href
                    , defer_ "true"
                    ]
                    ""


