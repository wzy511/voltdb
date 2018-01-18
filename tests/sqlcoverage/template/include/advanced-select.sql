-- This file holds the SELECT statement patterns formerly located in
-- advanced-template.sql, but without its DELETE and INSERT statements,
-- so that these can be called separately, when desired

-- Required preprocessor macros (with example values):
-- {@insertvals = "_id, _value[decimal], _value[decimal], _value[float]"}
-- {@aftermath = " _math _value[int:1,3]"}
-- {@agg = "_numagg"}
-- {@distinctableagg = "_distinctableagg"}
-- {@cmp = "_cmp"} -- all comparison operators (=, <>, !=, <, >, <=, >=)
-- {@somecmp = "_somecmp"} -- a smaller list of comparison operators (=, <, >=)
-- {@columntype = "decimal"}
-- {@columnpredicate = "_numericcolumnpredicate"}
-- {@comparableconstant = "42.42"}
-- {@comparabletype = "numeric"}
-- {@dmltable = "_table"}
-- {@fromtables = "_table"}
-- {@optionalfn = "_numfun"}
-- {@plus10 = " + 10"}
-- {@star = "*"}
-- {@updatecolumn = "NUM"}
-- {@updatesource = "ID"}
-- {@updatevalue = "_value[int:0,100]"}
-- {@updatecolumn2 = "RATIO"} -- rarely used; so far, only in CTE tests
-- {@maxdepth = "6"} -- maximum depth, in Recursive CTE tests

--- Define "place-holders" used in some of the queries below
{_optionalorderbyidlimitoffset |= ""}
{_optionalorderbyidlimitoffset |= "LIMIT 1000"}
{_optionalorderbyidlimitoffset |= "ORDER BY @idcol _sortorder _optionallimit _optionaloffset"}

-- alias fun
-- ticket 231
SELECT -8, A._variable[#arg numeric] FROM @fromtables A WHERE @optionalfn(A.__[#arg] + 5   )        > @comparableconstant
SELECT -7, A._variable[#arg numeric] FROM @fromtables A WHERE @optionalfn(A.__[#arg]       ) + 5    > @comparableconstant
SELECT -6, @optionalfn(A._variable[#arg numeric] + 5   )        NUMSUM FROM @fromtables A ORDER BY NUMSUM
SELECT -5, @optionalfn(A._variable[#arg numeric]       ) + 5    NUMSUM FROM @fromtables A ORDER BY NUMSUM
SELECT -4, A._variable[#arg numeric] FROM @fromtables A WHERE @optionalfn(A.__[#arg] + 5.25)        > @comparableconstant
SELECT -3, A._variable[#arg numeric] FROM @fromtables A WHERE @optionalfn(A.__[#arg]       ) + 5.25 > @comparableconstant
SELECT -2, @optionalfn(A._variable[#arg numeric] + 5.25)        NUMSUM FROM @fromtables A ORDER BY NUMSUM
SELECT -1, @optionalfn(A._variable[#arg numeric]       ) + 5.25 NUMSUM FROM @fromtables A ORDER BY NUMSUM

-- cover some select WHERE expressions not covered by the basic templates
-- (use of @somecmp rather than @cmp reduces the explosion of generated queries)

SELECT 1, @star FROM @fromtables A WHERE @columnpredicate
SELECT 2, @star FROM @fromtables A WHERE @optionalfn(A._variable[#some numeric]) @somecmp (            A._variable[#other numeric]  _math 2)
SELECT 3, @star FROM @fromtables A WHERE             A._variable[#some numeric]  @somecmp (@optionalfn(A._variable[#other numeric]) _math 3)

SELECT @optionalfn(A._variable[#picked @columntype]) AS Q4 FROM @fromtables A ORDER BY @optionalfn(A.__[#picked]), 1 LIMIT _value[int:0,10]

-- Found eng-3191 (or similar, anyway) crashed (fixed, since?) with these statements:
-- -- combine where and limit
-- SELECT @optionalfn(A._variable[#picked @columntype]) AS Q5 FROM @fromtables A WHERE  @optionalfn(A._variable[@comparabletype]) @somecmp @optionalfn(A._variable[@comparabletype]) ORDER BY @optionalfn(A.__[#picked]) LIMIT _value[int:0,100]
-- -- combine where and offset
-- SELECT @optionalfn(A._variable[#picked @columntype]) AS Q6 FROM @fromtables A WHERE  @optionalfn(A._variable[@comparabletype]) @somecmp @optionalfn(A._variable[@comparabletype]) ORDER BY @optionalfn(A.__[#picked]) LIMIT _value[int:0,100] OFFSET _value[int:0,100]
-- -- compare more columns
-- SELECT @optionalfn(A._variable[@comparabletype]    ) AS Q7 FROM @fromtables A WHERE (@optionalfn(A._variable[@comparabletype]) @somecmp @optionalfn(A._variable[@comparabletype])) _logicop (@optionalfn(A._variable[@comparabletype]) @somecmp @optionalfn(A._variable[@comparabletype]))
-- Now that eng-3191 is fixed, we keep them watered down to reduce the number of generated combinations:
-- Even simplified like this, it crashes (or DID, anyway):
-- SELECT @optionalfn(A._variable[#picked            ]) AS Q5 FROM @fromtables A WHERE             (A._variable[@comparabletype]) @somecmp @optionalfn(A._variable[@comparabletype]) ORDER BY @optionalfn(A.__[#picked]) LIMIT _value[int:0,100]
-- so, it was simplified even further
-- combine where and limit
   SELECT @optionalfn(A._variable[#picked @columntype]) AS Q5 FROM @fromtables A WHERE             (A.__[#picked]               ) @somecmp            (A._variable[@comparabletype]) ORDER BY 1                        LIMIT _value[int:0,100]
-- combine where and offset
   SELECT @optionalfn(A._variable[#picked @columntype]) AS Q6 FROM @fromtables A WHERE             (A._variable[@comparabletype]) @somecmp            (A.__[#picked]               ) ORDER BY 1                        LIMIT _value[int:0,100] OFFSET _value[int:0,100]
-- compare more columns
   SELECT @optionalfn(A._variable[#picked @columntype]) AS Q7 FROM @fromtables A WHERE (           (A.__[#picked]               ) @somecmp            (A._variable[@comparabletype])) _logicop ( @columnpredicate )

-- order by with projection
SELECT 8, @optionalfn(A._variable[@columntype]), ID FROM @fromtables A ORDER BY ID _sortorder
-- order by on two columns
-- eng-631 With multiple columns named the same thing and multiple order by columns using the same
-- column and different sort directions, this statement failed.
-- First explicitly isolate a test for eng-631 where DESC and ASC order are non-sensically combined on
-- the same column.
-- TO avoid too much explosion, separate out SELECT function options from sort order options.
SELECT            (A._variable[#order1 @columntype]) AS Q12,            (A._variable[#order2 @columntype]), A._variable FROM @fromtables A ORDER BY @optionalfn(A.__[#order1]) _sortorder, @optionalfn(A.__[#order2])
SELECT @optionalfn(A._variable[#order1 @columntype]) AS Q13, @optionalfn(A._variable[#order2 @columntype]), A._variable FROM @fromtables A ORDER BY 1 DESC, 2 DESC

SELECT @optionalfn(A._variable[@columntype]) AS Q14, @optionalfn(A._variable[@columntype]), @star FROM @fromtables A ORDER BY 1 _sortorder, 2 _sortorder

-- additional aggregation fun
SELECT     @distinctableagg(DISTINCT @optionalfn(        A._variable[@columntype]     ))                                              AS Q15 FROM @fromtables A
SELECT     @agg(                     @optionalfn(        A._variable[@columntype]     ))                                              AS Q16 FROM @fromtables A WHERE @columnpredicate
-- These test that the fixed issue eng-909 -- combining DISTINCT and non-DISTINCT aggs has not regressed.
SELECT     @distinctableagg(DISTINCT @optionalfn(        A._variable[@columntype]     )), @agg(            A._variable[@columntype] ) AS Q18 FROM @fromtables A
SELECT     @distinctableagg(DISTINCT                     A._variable[@columntype]      ), @agg(@optionalfn(A._variable[@columntype])) AS Q19 FROM @fromtables A
SELECT 20,                                               A._variable[#GB @columntype]   , @agg(@optionalfn(A._variable[@columntype]))        FROM @fromtables A GROUP BY         A.__[#GB]
SELECT 21,                           @optionalfn(        A._variable[#GB @columntype] ) , @agg(            A._variable[@columntype] )        FROM @fromtables A GROUP BY         A.__[#GB]
SELECT 22,                           @optionalfn(@onefun(A._variable[#GB @columntype])) , @agg(            A._variable[@columntype] )        FROM @fromtables A GROUP BY @onefun(A.__[#GB])
-- multiple column group by
SELECT 23,            A._variable[#GB1 @columntype],   A._variable[#GB2 @comparabletype], @agg(            A._variable[@comparabletype])     FROM @fromtables A GROUP BY         A.__[#GB1], A.__[#GB2]

SELECT     @agg(                     @optionalfn(        A._variable[@columntype]     )), @agg(@optionalfn(A._variable[@columntype])) AS Q24 FROM @fromtables A
SELECT     @agg(                     @optionalfn(        A._variable[@columntype]     )), COUNT(*)                                    AS Q25 FROM @fromtables A

-- group by alias (50 - 55)
SELECT 50,                           @optionalfn(@onefun(A._variable[#GB @columntype])) as tag , @agg(            A._variable[@columntype] )        FROM @fromtables A GROUP BY tag
SELECT 51,                           A._variable[#GB @columntype] as tag ,                       @agg(            A._variable[@columntype] )        FROM @fromtables A GROUP BY tag
SELECT 52, @optionalfn(@onefun(A._variable[#GB @columntype])) as tag1, A._variable[@comparabletype] as tag2, @agg(A._variable[@comparabletype])     FROM @fromtables A GROUP BY tag2, tag1

-- DISTINCT expression (56 - 65)
-- basic select template has covered multiple columns distinct
SELECT DISTINCT @optionalfn(A._variable[@columntype]) AS C56, A._variable FROM @fromtables A 
SELECT DISTINCT @onefun(A._variable[@columntype]) AS C56, A._variable FROM @fromtables A ORDER BY 1, 2 LIMIT 10

-- Edge case: table aggregate with DISTINCT
SELECT DISTINCT COUNT(*) FROM @fromtables A 
SELECT DISTINCT @agg( A._variable[@columntype] ), COUNT(*)  FROM   @fromtables A

-- DISTINCT on GROUP BY
SELECT DISTINCT   @agg(@optionalfn(A._variable[@columntype]))                                 FROM @fromtables A GROUP BY         A._variable[@comparabletype]
SELECT DISTINCT   A._variable[#GB1 @columntype], A._variable[#GB2 @comparabletype],  @agg(     A._variable)         FROM @fromtables A GROUP BY         A.__[#GB1], A.__[#GB2]
SELECT DISTINCT   A._variable[#GB1 @columntype], A._variable[#GB2 @comparabletype],  @agg(     A._variable)         FROM @fromtables A GROUP BY         A.__[#GB1], A.__[#GB2] ORDER BY 1, 2 LIMIT 5

-- AGG DISTINCT
SELECT   A._variable[#GB @columntype],  @distinctableagg(DISTINCT  A._variable[@comparabletype]  ) AS Q60 FROM @fromtables A  GROUP BY         A.__[#GB]
SELECT   A._variable[#GB1 @columntype],  A._variable[#GB2 @comparabletype]   , @distinctableagg( DISTINCT A._variable)  AS Q61       FROM @fromtables A GROUP BY         A.__[#GB1], A.__[#GB2]
SELECT   A._variable[#GB1 @columntype],  @optionalfn(        A._variable[@comparabletype]     ) as GB2_alias   , @distinctableagg( DISTINCT A._variable)  AS Q62         FROM @fromtables A GROUP BY         A.__[#GB1],  GB2_alias

-- update
-- compare two cols
-- UPDATE @fromtables A SET @updatecolumn = @updatevalue WHERE @optionalfn(A._variable[@columntype]) @somecmp @optionalfn(A._variable[@columntype])
-- comparison with set expression
UPDATE @dmltable A SET @updatecolumn = @updatesource @aftermath WHERE @optionalfn(_variable[@columntype]) @somecmp _variable[@comparabletype]

-- Save more exhaustive LIKE testing for advanced-strings.sql.
-- This is mostly just to catch the error of applying different forms of LIKE to non-strings.
-- TODO: migrate likely-to-error-out cases like this to their own template/suite
SELECT @star FROM @fromtables Q26 WHERE Q26._variable[@columntype] _maybe LIKE 'abc%'
SELECT @star FROM @fromtables Q27 WHERE Q27._variable[@columntype]        LIKE '%'
SELECT @star FROM @fromtables Q28 WHERE Q28._variable[@columntype]        LIKE '%' ESCAPE '!'
-- Uncomment after ENG-9449 is fixed; and delete the two above (??):
--SELECT @star FROM @fromtables Q27 WHERE Q27._variable[@columntype] _maybe LIKE '%'
--SELECT @star FROM @fromtables Q28 WHERE Q28._variable[@columntype] _maybe LIKE '%' ESCAPE '!'
SELECT @star FROM @fromtables Q29 WHERE Q29._variable[@columntype] _maybe LIKE '!%' ESCAPE '!'

----SELECT @star FROM @fromtables A WHERE _inoneint
----SELECT @star FROM @fromtables A WHERE _inpairofints
------just too slow for now: SELECT @star FROM @fromtables A WHERE _insomeints
----SELECT @star FROM @fromtables A WHERE _inonestring
----SELECT @star FROM @fromtables A WHERE _inpairofstrings
------just too slow for now: SELECT @star FROM @fromtables A WHERE _insomestrings

--- Test CASE WHEN
--- CASE WHEN with expression
--- Note: the parens are needed here (in Q34, Q35, Q38, Q39) because without them PostgreSQL does
--- not parse certain queries correctly: when used with strings (so + is concatenation - translated
--- to || for PostgreSQL) and with certain comparison operators (<>, <=, >=, !=), an error results:
--- "operator does not exist: boolean || integer. Hint: No operator matches the given name and
--- argument type(s). You might need to add explicit type casts", which suggests that the comparison
--- operators are given higher precedence than the concatenation operator. In contrast, VoltDB and
--- HSQL behave the same, with or without the parens.
SELECT @star FROM @fromtables Q34 WHERE CASE WHEN Q34._variable[#arg @columntype] @cmp @comparableconstant THEN Q34._variable[#numone @columntype]            ELSE Q34.__[#arg] @aftermath END @cmp (@comparableconstant@plus10)
SELECT @star FROM @fromtables Q35 WHERE CASE WHEN Q35._variable[#arg @columntype] @cmp @comparableconstant THEN Q35._variable[#numone @columntype]                                         END @cmp (@comparableconstant@plus10)
SELECT __[#numone]            Q36,      CASE WHEN   A._variable[#arg @columntype] @cmp @comparableconstant THEN   A._variable[#numone @columntype]            ELSE   A.__[#arg] @aftermath END FROM @fromtables A WHERE @columnpredicate
SELECT __[#arg]               Q37,      CASE WHEN   A._variable[#arg @columntype] @cmp @comparableconstant THEN   A.__[#arg]                                                               END FROM @fromtables A WHERE @columnpredicate
--- CASE WHEN like DECODE
SELECT @star FROM @fromtables Q38 WHERE CASE      Q38._variable[#arg @columntype] WHEN @comparableconstant THEN Q38._variable[#numone @columntype] @aftermath ELSE Q38.__[#arg] @aftermath END @cmp (@comparableconstant@plus10)
SELECT @star FROM @fromtables Q39 WHERE CASE      Q39._variable[#arg @columntype] WHEN @comparableconstant THEN Q39._variable[#numone @columntype] @aftermath                              END @cmp (@comparableconstant@plus10)
SELECT __[#numone]            Q40,      CASE        A._variable[#arg @columntype] WHEN @comparableconstant THEN   A._variable[#numone @columntype] @aftermath ELSE   A.__[#arg] @aftermath END FROM @fromtables A WHERE @columnpredicate
SELECT __[#arg]               Q41,      CASE        A._variable[#arg @columntype] WHEN @comparableconstant THEN   A._variable[#numone @columntype] @aftermath                              END FROM @fromtables A WHERE @columnpredicate



-- Prepare tables for (Recursive) CTE query tests: note that just two columns
-- are updated to reference one other column, using a "standard tree" structure
-- typical of Recursive CTE queries (similar to our Employee/Manager example),
-- with one NULL value at the "top" of the tree; other columns remain random
UPDATE @dmltable[#tab] Q67 SET @updatecolumn = (SELECT @updatesource FROM __[#tab] WHERE @idcol =  \
    FLOOR( (SELECT MIN(@idcol) FROM __[#tab]) + (Q67.@idcol - (SELECT MIN(@idcol) FROM __[#tab]))/3 ) LIMIT 1)

UPDATE @dmltable[#tab] Q68 SET @updatecolumn = NULL WHERE @idcol = (SELECT MIN(@idcol) FROM __[#tab])

-- TODO: decide whether to keep this or not, which updates a second column,
-- similar to the first but in a different order, so the "top" of the tree
-- is no longer the "first" row (with the lowest ID), but in the middle:
UPDATE @dmltable[#tab] Q69 SET @updatecolumn2 =  \
    ( SELECT @updatecolumn FROM __[#tab] WHERE @idcol =  \
    ( SELECT @idcol FROM __[#tab],  \
    ( SELECT MAX(@idcol)+1 - MIN(@idcol) AS BASE, (MAX(@idcol)+1 - MIN(@idcol))/2 AS BD2 FROM __[#tab] ) AS B  \
        WHERE MOD(@idcol + CAST(B.BD2 AS INTEGER), B.BASE) = MOD(Q69.@idcol, B.BASE) ) )


-- TODO: temp, to check the data in both/all tables:
SELECT * FROM @fromtables ORDER BY ID


-- Basic (non-Recursive) CTE (Common Table Expression) query tests (i.e., using WITH):

-- ... using a simple join and a simplified version of our standard Employee/Manager
-- example (at least, when the @updatesource and @updatecolumn are used as the first
-- two columns, it's kind of similar to that example)
WITH CTE AS (  \
    SELECT _variable[#ctec1 @columntype], _variable[#ctec2 @columntype], 1 AS DEPTH,  \
        CAST(__[#ctec1] AS VARCHAR) AS PATH, _variable[#ctec5 numeric] AS TOTAL, _variable[#ctec6]  \
        FROM @fromtables[#tab]  \
)   SELECT Q70.__[#ctec1], Q70.__[#ctec2], COALESCE(CTE.DEPTH, 0) + 1 AS DEPTH,  \
        COALESCE(CTE.PATH || '/', '') || Q70.__[#ctec1] AS PATH, CTE.TOTAL + Q70.__[#ctec5] AS TOTAL, Q70.__[#ctec6]  \
        FROM __[#tab] Q70 _jointype JOIN CTE ON Q70.__[#ctec2] = CTE.__[#ctec1]
        -- _optionalorderbyidlimitoffset

-- ... with aggregate functions (implicit GROUP BY), and an implicit JOIN (but no explicit one)
WITH CTE AS (  \
    SELECT @agg(_variable[#ctec1 @columntype]) AG1, MIN(_variable[#ctec2 @columntype]) MN2, MAX(_variable[numeric]) MX3, COUNT(_variable) CT4 FROM @fromtables[#tab]  \
)   SELECT @star FROM __[#tab] Q71, CTE
-- _optionalorderbyidlimitoffset

-- ... with aggregate functions (implicit GROUP BY), and an explicit JOIN
WITH CTE AS (  \
    SELECT @agg(_variable[#ctec1 @columntype]) AG1, MIN(_variable[#ctec2 @columntype]) MN2, MAX(_variable[numeric]) MX3, COUNT(_variable) CT4 FROM @fromtables[#tab]  \
)   SELECT @star FROM __[#tab] Q72 _jointype JOIN CTE ON Q72.__[#ctec2] <= CTE.AG1
-- _optionalorderbyidlimitoffset

-- ... with an actual GROUP BY, in the CTE
WITH CTE AS (  \
    SELECT _variable[#ctec1 @columntype], @agg(_variable[#ctec2 @columntype]) AG1  \
        FROM @fromtables[#tab] GROUP BY __[#ctec1]  \
)   SELECT @star FROM __[#tab] Q73 _jointype JOIN CTE ON Q73.__[#ctec1] = CTE.__[#ctec1]
-- _optionalorderbyidlimitoffset

-- ... with an actual GROUP BY, in the CTE and in the main (final) query
WITH CTE AS (  \
    SELECT _variable[#ctec1 @columntype], _symbol[#agg1 @agg](_variable[#ctec2 @columntype]) AG1  \
        FROM @fromtables[#tab] GROUP BY __[#ctec1]  \
)   SELECT _variable[#grp], __[#agg1](Q74.__[#ctec2]) AG1, __[#agg1](AG1) AG2  \
        FROM __[#tab] Q74 _jointype JOIN CTE ON Q74.__[#ctec1] = CTE.__[#ctec1]  \
        GROUP BY __[#grp]
-- _optionalorderbyidlimitoffset

-- TODO: uncomment if we ever support more than one CTE (ENG-13575):
--WITH CTE1 AS (  \
--    SELECT _variable[#ctec1 @columntype], _symbol[#agg1 @agg](_variable[#ctec2 @columntype]) AG1  \
--        FROM @fromtables[#tab] GROUP BY __[#ctec1]  \
--    ), CTE2 AS (  \
--        SELECT __[#ctec1] FROM CTE1  \
--            WHERE AG1 > (SELECT MIN(AG1) FROM CTE1)  \
--    )  \
--SELECT __[#ctec1], _variable[#ctec3], __[#agg1](__[#ctec2]) AS AG1, _numagg(_variable[numeric]) AS AG2  \
--    FROM __[#tab] WHERE __[#ctec1] IN (SELECT __[#ctec1] FROM CTE2)  \
--    GROUP BY __[#ctec1], __[#ctec3] 
--    -- _optionalorderbyidlimitoffset


-- Recursive CTE query tests (i.e., using WITH RECURSIVE):

-- ... using a version of our standard Employee/Manager example (at least, when
-- the @updatesource and @updatecolumn are used as the first two columns, it's
-- very similar to that example)

WITH RECURSIVE RCTE (_variable[#rctec1 @columntype], _variable[#rctec2 @columntype], DEPTH, PATH, TOTAL, _variable[#rctec6]) AS (  \
    SELECT __[#rctec1], __[#rctec2], 1, CAST(__[#rctec1] AS VARCHAR), _variable[#rctec5 numeric], __[#rctec6]  \
        FROM @fromtables[#tab] WHERE __[#rctec2] IS NULL  \
    UNION ALL  \
    SELECT Q80.__[#rctec1], Q80.__[#rctec2], RCTE.DEPTH + 1, RCTE.PATH || '/' || Q80.__[#rctec1], RCTE.TOTAL + Q80.__[#rctec5] AS TOTAL, Q80.__[#rctec6]  \
        FROM __[#tab] Q80 JOIN RCTE ON Q80.__[#rctec2] = RCTE.__[#rctec1]  \
        WHERE DEPTH < @maxdepth  \
)   SELECT * FROM RCTE
-- _optionalorderbyidlimitoffset


-- ... with aggregate functions (implicit GROUP BY)
-- TODO: write one of these

-- ... with an actual GROUP BY (or two??)
-- TODO: write one of these
