-- The suite is assembled by hspec-discover: every @*Spec.hs@ under @test/@ that
-- exports @spec@ is collected at build time. Nothing is registered by hand, so a
-- new spec file cannot be written and then forgotten.
{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
