---------------- Exercise 1.1 -------------------------

-- Check the type of the following:
:type 5 + 8
-- 5 + 8 :: Num a => a
:type 3 * 5 + 8
-- 3 * 5 + 8 :: Num a => a
:type 2 + 4
-- 2 + 4 :: Num a => a
:type (+) 2 4
-- (+) 2 4 :: Num a => a
:type sqrt 16
-- sqrt 16 :: Floating a => a
:type succ 6
-- succ 6 :: (Num a, Enum a) => a
:type succ 7
-- increments? Successor?
-- succ 7 :: (Num a, Enum a) => a
:type pred 9
-- decrements. predecessor
-- pred 9 :: (Num a, Enum a) => a
:type pred 8
-- pred 8 :: (Num a, Enum a) => a
:type sin (pi / 2)
-- sin (pi / 2) :: Floating a => a
-- Sine
:type truncate pi
-- truncate pi :: Integral b => b
-- truncates to full Integer. No rounding.
:type round 3.5
-- round 3.5 :: Integral b => b
-- round up or down based on value
:type round 3.4
-- round 3.4 :: Integral b => bs
:type floor 3.7
-- floor 3.7 :: Integral b => b
-- round down.
:type ceiling 3.3
-- ceiling 3.3 :: Integral b => b
-- round up.

---------------- Exercise 1.2 -------------------------
-- I declared let z = 2, and ran the :show command to return the following:
:show bindings
-- z :: Num t => t = _
-- it :: Num t => t = _

-- added additional variable declarations and received:
-- let x = 2, let z = 5, let dog = "dog", let c = 'c'
:show bindings
-- $trModule :: GHC.Types.Module = _
-- x :: Num t => t = _
-- it :: Num t => t = _
-- z :: Num t => t = _
-- dog :: [Char] = _
-- c :: Char = 'c'

---------------- Exercise 1.3 -------------------------
-- Updated WC.hs file as directed.
