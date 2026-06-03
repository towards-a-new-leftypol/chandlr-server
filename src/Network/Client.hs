{-# LANGUAGE OverloadedStrings #-}

-- Dummy module, see the front-end implementation for the real client.
module Network.Client where

import Miso (Component, component, vfrag)

import Common.Network.ClientTypes (Model (Uninitialized), Action)

app :: Component parent Model Action
app = component Uninitialized (const $ return ()) (const $ vfrag [])
