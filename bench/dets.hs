-- dets.hs -- bugs from the Erlang's dets library
--
-- Copyright (c) 2017-2020 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
--
--
-- In 2016, John Hughes wrote a paper titled:
--
-- "Experiences with QuickCheck: Testing the Hard Stuff And Staying Sane"
--
-- http://publications.lib.chalmers.se/records/fulltext/232550/local_232550.pdf
--
-- In it, among other things, he describes how he used QuviQ QuickCheck to find
-- 5 bugs in Erlang's dets library which were "open problems" at the time.
--
--
-- Fastforward to my PhD exam in 2017 (Rudy), John Hughes, one of my examiners
-- challenged me to use LeanCheck to try to find one of the bugs of Erlang's
-- dets library which he thought would be unreachable by LeanCheck.
--
-- Unreachable above means appearing too late in the enumeration to be able
-- to practially reach it.
--
-- Turns out Hughes was right, LeanCheck runs out of memory before reaching the
-- bug.
--
--
-- This is a reconstruction of the program written during my PhD exam
-- (2017-11-24).
--
-- This program does not really test the dets library directly, but merely
-- pattern matches the bug.  So the dummy property under test returns False for
-- the specific bug we're looking for.
--
--
-- This version seaches for the 5 bugs found by QuickCheck described in Hughes'
-- paper.  It is able to find 2 of these 5.  We run out of memory before being
-- able to reach the other 3.
--
-- By cheating a bit, we can increase bugs found to 3,
-- but 2 are then still out of reach.
--
--
-- Dets library online manual: http://erlang.org/doc/man/dets.html
import Test.LeanCheck


-- For simplicity, we only stick ourselves with 3 different possible database
-- names
data Name  =  A | B | C
  deriving (Eq, Ord, Show, Read)

type Key  =  Int

type Value  =  Int

type Object  =  (Key, Value)

-- The most important operations in Erlang's dets.
data Op  =  All                       -- all() -> [tab_name()]
         |  Close Name                -- close(Name)
         |  Delete Name Key           -- delete(Name, Key)
         |  DeleteAllObjects Name     -- delete_all_objects(Name)
--       |  First Name                -- first(Name) -> Key
         |  Insert Name [Object]      -- insert(Name, Objects)
         |  InsertNew Name [Object]   -- insert_new(Name, Objects) -> boolean()
         |  Lookup Name Key           -- lookup(Name, Key) -> Objects
         |  Member Name Key           -- member(Name, Key) -> boolean()
--       |  Next Name Key             -- next(Name, Key) -> boolean()
         |  Open Name                 -- open_file(...)
         |  GetContents Name          -- get_contents(Name) -> Objects
  deriving (Eq, Ord, Show, Read)

-- NOTE: I couldn't find get_contents on the dets documentation, but it is listed on Hughes paper
--       as one of the operations.  Perhaps he implemented it using first and next.

-- NOTE (2): I am admittedly cheating a bit
--           by ommiting First and Next in
--           place of GetContents.

-- A Program has a prefix and a parallel section that follows immediately
data Program  =  Program [Op] [[Op]]
  deriving (Eq, Ord, Show, Read)

instance Listable Name where list = [A, B, C]

-- This is an unoptimized generator as it will generate invalid programs like:
--
-- Program [Close A, Lookup B 0] []
instance Listable Op where
  tiers  =  cons0 All
         \/ cons1 Close
         \/ cons2 Delete
         \/ cons1 DeleteAllObjects
--       \/ cons1 First
         \/ cons2 Insert
         \/ cons2 InsertNew
         \/ cons2 Lookup
         \/ cons2 Member
--       \/ cons2 Next
         \/ cons1 Open
         \/ cons1 GetContents

instance Listable Program where
  tiers  =  cons2 Program

--            open(a)
-- -------------|-----------------
-- insert(a,[]) | insert_new(a,[])
bug1 :: Program
bug1 = Program
  [Open A]           -- initialization
  [ [Insert A []]    -- thread 1
  , [InsertNew A []] -- thread 2
  ]

--             open(a)
-- ----------------|---------------------
-- insert(a,{0,0}) | insert_new(a,{0,0})
bug2 :: Program
bug2 = Program
  [Open A]
  [ [Insert A [(0,0)]]
  , [InsertNew A [(0,0)]]
  ]

--       open(a)
-- --------|----------------
-- open(a) | insert(a,{0,0})
--         | get_contents(a)
bug3 :: Program
bug3 = Program
  [Open A]
  [ [Open A]
  , [ Insert A [(0,0)]
    , GetContents A
    ]
  ]

-- This was the one John Hughes originally
-- challenged me to find using LeanCheck
-- after my PhD examination.  (Rudy, 2017)
bug4 :: Program
bug4 = Program
  [ Open A
  , Close A
  , Open A
  ]
  [ [Lookup A 0]
  , [Insert A [(0,0)]]
  , [Insert A [(0,0)]]
  ]

bug5 :: Program
bug5 = Program
  [ Open A
  , Insert A [(1,0)]
  ]
  [ [ Lookup A 0
    , Delete A 1
    ]
  , [Open A]
  ]


prop1, prop2, prop3, prop4, prop5 :: Program -> Bool
prop1 p = p /= bug1
prop2 p = p /= bug2
prop3 p = p /= bug3
prop4 p = p /= bug4
prop5 p = p /= bug5

main :: IO ()
main = do
  checkFor   200000  prop1  -- bug found after    88 410 tests
  checkFor  4000000  prop2  -- bug found after 2 044 950 tests
  checkFor  2000000  prop3  -- bug not found, more tests and we go out of memory...
  checkFor  2000000  prop4  -- bug not found, more tests and we go out of memory...
  checkFor  2000000  prop5  -- bug not found, more tests and we go out of memory...

-- If I recall correcly, John Hughes mentioned that each test took 3 seconds to
-- run.  That means LeanCheck would find the first bug after 3 days, and the
-- second bug after 2 months.  This could be improved if we cheat by removing
-- some of the uneeded operations in our Program datatype.
--
-- 1. Supposing we drop the "All" and "DeleteAllObjects" operation:
--
--    * bug 1 is found after    18 580 tests
--    * bug 3 is found after   300 104 tests
--    * bug 3 is found after 1 204 421 tests
--    * bugs 4 and 5 are not found
--
-- 2. Supposing we just allow a single database name "A"
--
--    * bug 1 is found after    10 651 tests
--    * bug 3 is found after   144 852 tests
--    * bug 3 is found after   534 550 tests
--    * bugs 4 and 5 are not found
--
-- Other improvements are possible:
--
-- * generating a set of parallel programs instead of a list as the order does
--   not matter (we would have to change the Program comparison function
--   accordingly)
--
-- * only generating valid programs (we only operate on A after open'ing it)
--
-- * generating a set of operations, then from that generate a set of programs
--   with these operations
--
-- But I conjecture bugs 4 and 5 will be simply out of reach anyway.
