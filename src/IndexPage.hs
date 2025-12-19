{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module IndexPage where

import Miso (mount)
import Miso.Html.Property
    ( charset_
    , name_
    , content_
    , rel_
    , href_
    , type_
    , class_
    , src_
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
import Common.FrontEnd.MainComponent (MainComponent)

data IndexPage = forall b. (ToJSON b)
    => IndexPage (JSONSettings, b, MainComponent)

instance ToHtml IndexPage where
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

                    , jsm $ static_root <> "/profile.js"
                    , jsm $ static_root <> "/init.js"
                    , css $ static_root <> "/style.css"
                    ] ++ adminCss
                )
            , body_ [] [ mount (x :: MainComponent) ]
            ]
        ]

        where
            static_root = static_serve_url_root settings

            adminCss
                | admin settings =
                    [ css $ static_root <> "/newstyle.css"
                    , css $ static_root <> "/newstyle-admin.css"
                    ]
                | otherwise = []

            css href =
                link_
                    [ rel_ "stylesheet"
                    , type_ "text/css"
                    , href_ $ toMisoString href
                    ]

            jsm href =
                script_
                    [ type_ "module"
                    , src_ $ toMisoString href
                    ]
                    ""
