{-# LANGUAGE OverloadedStrings #-}

-- Dummy module, see the front-end implementation for the real client.
module Network.Client where

import Miso (Component, component, text)

import Common.Network.ClientTypes (Model (Uninitialized), Action)

app :: Component Model Action
app = component Uninitialized (const $ return ()) (const $ text "")
