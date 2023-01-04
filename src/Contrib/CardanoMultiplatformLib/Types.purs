module CardanoMultiplatformLib.Types where

-- We load the lib dynamically (nodejs vs browser version) so we represent
-- the lib module by opaque value.
-- FIXME: in `js-object` we should introduce the concept of `TsClass` so we
-- can actually handle these scenarios generically.
foreign import data Lib :: Type

