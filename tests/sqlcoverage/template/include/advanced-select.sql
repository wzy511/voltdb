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

-- For some queries, there is no ID, so we order by one of the columns
{_optionalorderbyordlimitoffset |= ""}
{_optionalorderbyordlimitoffset |= "LIMIT 1000"}
{_optionalorderbyordlimitoffset |= "ORDER BY __[#ord] _sortorder _optionallimit _optionaloffset"}

-- For some queries, ordering by ID is insufficient to ensure a unique order
{_optionalorderbyidtotallimitoffset |= ""}
{_optionalorderbyidtotallimitoffset |= "LIMIT 1000"}
{_optionalorderbyidtotallimitoffset |= "ORDER BY @idcol _sortorder, PATH, TOTAL _optionallimit _optionaloffset"}

-- TODO: probably will change these back (to _jointype, _nonfulljointype) later:
{@jointype = "_jointype"}
{@nonfulljointype = "_nonfulljointype"}

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



-- Prepare tables for (Basic & Recursive) CTE query tests: first, double
-- the number of rows
INSERT INTO @dmltable[#tab] SELECT @insertselectcols FROM __[#tab]

-- Next, one column is updated to reference another column, using a "tree"
-- structure (a binary tree, in this case) typical of Recursive CTE queries
-- (similar to our Employee/Manager example, from ENG-13286); other columns
-- remain random
UPDATE @dmltable[#tab] Q70 SET @updatecolumn =   \
    ( SELECT @updatesource FROM __[#tab] WHERE @idcol =  \
        FLOOR( (SELECT MIN(@idcol) FROM __[#tab]) + (Q70.@idcol  \
             - (SELECT MIN(@idcol) FROM __[#tab]))/2 ) LIMIT 1 )

-- Change the "last" 4 rows, to make the "tree" deeper
UPDATE @dmltable[#tab] Q71 SET @updatecolumn =  \
    ( SELECT @updatesource FROM __[#tab] WHERE @idcol = Q71.@idcol - 1 )  \
    WHERE @idcol > (SELECT MAX(@idcol) FROM __[#tab]) - 4

-- Add a NULL value at the "top" & "middle" of the "tree"
-- [Note: for this "middle", we round down, unlike below ??]
UPDATE @dmltable[#tab] Q72 SET @updatecolumn = NULL  \
    WHERE @idcol = (SELECT                  MIN(@idcol)    FROM __[#tab])  \
       OR @idcol = (SELECT (MAX(@idcol)+1 + MIN(@idcol))/2 FROM __[#tab])

-- Then, update a second column, similar to the first but in a different order,
-- so the "top" of this "tree" is no longer the "first" row (with the lowest ID),
-- but in the "middle"; and this one is not a binary tree
-- (Note: if @updatecolumn2 is set to NONEXISTENT, then this statement will
-- return an error, and have no effect)
UPDATE @dmltable[#tab] Q73 SET @updatecolumn2 =  \
    ( SELECT @updatesource FROM __[#tab],  \
        ( SELECT MIN(@idcol) AS MINID, MAX(@idcol)+1 - MIN(@idcol)                AS DIFF,  \
                                CAST( (MAX(@idcol)+1 - MIN(@idcol))/2 AS INTEGER) AS HALF   \
        FROM __[#tab] ) AS B  \
        WHERE MOD(    @idcol - B.MINID + B.HALF, B.DIFF) =   \
              MOD(Q73.@idcol - B.MINID + B.HALF, B.DIFF)/3 LIMIT 1 )

-- Add a NULL value at the "top" of the "tree", but in the "middle", as far as
-- ID column values go
UPDATE @dmltable[#tab] Q74 SET @updatecolumn2 = NULL WHERE @idcol =  \
    ( SELECT (MAX(@idcol) + 1 + MIN(@idcol)) / 2 FROM __[#tab] )


-- TODO: temp, to check the data in both/all tables:
SELECT @star FROM @fromtables ORDER BY ID


-- Basic (non-Recursive) CTE (Common Table Expression) query tests (i.e., using WITH):

-- ... using a simple join and a simplified version of our standard Employee/Manager
-- example (at least, when the @updatesource and @updatecolumn are used as the first
-- two columns, it's kind of similar to that example - see ENG-13286)
WITH CTE AS (  \
    SELECT _variable[#ctec1 @columntype], _variable[#ctec2 @columntype], _variable[#ctec3], 1 AS DEPTH,  \
        COALESCE(CAST(__[#ctec1] AS VARCHAR), '') AS PATH, COALESCE(_variable[#ctec5 numeric], 0) AS TOTAL  \
        FROM @fromtables  \
)   SELECT Q80.__[#ctec1], Q80.__[#ctec2], Q80.__[#ctec3],  \
        CTE.DEPTH + 1 AS DEPTH,  \
        CTE.PATH || '/' || COALESCE(CAST(Q80.__[#ctec1] AS VARCHAR), '') AS PATH,   \
        CTE.TOTAL        +  COALESCE(     Q80.__[#ctec5], 0)  AS TOTAL  \
        FROM @fromtables Q80 @jointype JOIN CTE ON Q80.__[#ctec2] = CTE.__[#ctec1]  \
        _optionalorderbyidtotallimitoffset

-- ... with aggregate functions (implicit GROUP BY), and an implicit JOIN (but no explicit one)
WITH CTE AS (  \
    SELECT @agg(_variable[#ctec1 @columntype]) AG1, MIN(_variable[#ctec2 @columntype]) MN2,  \
            MAX(_variable[numeric]) MX3,          COUNT(_variable) CT4 FROM @fromtables[#tab]  \
)   SELECT @star FROM __[#tab] Q81, CTE _optionalorderbyidlimitoffset

-- ... with aggregate functions (implicit GROUP BY), and an explicit JOIN
-- Note: we use @nonfulljointype because PostgreSQL does not support FULL JOIN here
WITH CTE AS (  \
    SELECT @agg(_variable[#ctec1 @columntype]) AG1, MIN(_variable[#ctec2 @columntype]) MN2,  \
            MAX(_variable[numeric]) MX3,          COUNT(_variable) CT4 FROM @fromtables[#tab]  \
)   SELECT @star FROM __[#tab] Q82 @nonfulljointype JOIN CTE ON Q82.__[#ctec2] <= CTE.AG1  \
        _optionalorderbyidlimitoffset

-- ... with an actual GROUP BY, in the CTE (and an explicit JOIN)
WITH CTE AS (  \
    SELECT _variable[#ctec1 @columntype], @agg(_variable[#ctec2 @columntype]) AG1  \
        FROM @fromtables[#tab] GROUP BY __[#ctec1]  \
)   SELECT @star FROM __[#tab] Q83 @jointype JOIN CTE ON Q83.__[#ctec1] = CTE.__[#ctec1]  \
        _optionalorderbyidlimitoffset

-- ... with an actual GROUP BY, in the CTE and in the main (final) query (and an explicit JOIN)
WITH CTE AS (  \
    SELECT _variable[#ctec1 @columntype], _symbol[#agg1 @agg](_variable[#ctec2 @columntype]) __[#ctec2]  \
        FROM @fromtables[#tab] GROUP BY __[#ctec1]  \
)   SELECT _variable[#grp], __[#agg1](Q84.__[#ctec2]) AG1, __[#agg1](CTE.__[#ctec2]) AG2  \
        FROM __[#tab] Q84 @jointype JOIN CTE ON Q84.__[#ctec1] = CTE.__[#ctec1]  \
        GROUP BY __[#grp] _optionalorderbyidlimitoffset

-- TODO: uncomment/fix this if we ever support more than one CTE (ENG-13575):
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

-- ... a very simple example (almost like using VALUES, which we do not
-- support, currently - see ENG-13576)
-- TODO: if/when ENG-13613 is fixed, could replace CAST('A' AS VARCHAR) with
-- just 'A', in the 2 examples below:
WITH RECURSIVE RCTE (CT1, STR2) AS (  \
    SELECT 1, CAST('A' AS VARCHAR) FROM @fromtables Q90  \
    UNION ALL  \
    SELECT CT1+1, STR2 || 'b' FROM RCTE WHERE CT1 < 10  \
) SELECT @star FROM RCTE

-- ... a very similar simple example, but with aggregates
-- (in the "final" query)
WITH RECURSIVE RCTE (CT1, STR2) AS (  \
    SELECT 1, CAST('A' AS VARCHAR) FROM @fromtables Q91  \
    UNION ALL  \
    SELECT CT1+1, STR2 || 'b' FROM RCTE WHERE CT1 < 10  \
) SELECT SUM(CT1), MIN(STR2), MAX(STR2), COUNT(STR2) FROM RCTE

-- ... using a version of our standard Employee/Manager example (at least, when
-- the @updatesource and @updatecolumn are used as the first two columns, it's
-- very similar to that example - see ENG-13286)
-- Note: we use only inner joins because PostgreSQL does not support outer ones here
WITH RECURSIVE RCTE (_variable[#rctec1 @columntype], _variable[#rctec2 @columntype],  \
        _variable[#rctec3], DEPTH, PATH, TOTAL) AS (  \
    SELECT __[#rctec1], __[#rctec2], __[#rctec3], 1,  \
            CAST(__[#rctec1] AS VARCHAR),  \
            CAST(_variable[#rctec6 numeric] AS FLOAT)  \
        FROM @fromtables WHERE __[#rctec2] IS NULL  \
    UNION ALL  \
    SELECT Q92.__[#rctec1], Q92.__[#rctec2], Q92.__[#rctec3], RCTE.DEPTH + 1,  \
            COALESCE(RCTE.PATH, 'null') || '/' || COALESCE(CAST(Q92.__[#rctec1] AS VARCHAR), 'null'),  \
            COALESCE(RCTE.TOTAL, 0)         +     COALESCE(     Q92.__[#rctec6], 0)  \
        FROM @fromtables Q92 JOIN RCTE ON Q92.__[#rctec2] = RCTE.__[#rctec1]  \
        WHERE DEPTH < @maxdepth  \
)   SELECT @star FROM RCTE _optionalorderbyidtotallimitoffset

-- ... a very similar example, but with aggregate functions and GROUP BY
-- (in the "final" query)
-- Note: we use only inner joins because PostgreSQL does not support outer ones here
WITH RECURSIVE RCTE (_variable[#rctec1 @columntype], _variable[#ord @columntype],  \
        _variable[#rctec3], DEPTH, PATH, TOTAL) AS (  \
    SELECT __[#rctec1], __[#ord], __[#rctec3], 1,  \
            CAST(__[#rctec1] AS VARCHAR),  \
            CAST(_variable[#rctec6 numeric] AS FLOAT)  \
        FROM @fromtables WHERE __[#ord] IS NULL  \
    UNION ALL  \
    SELECT Q93.__[#rctec1], Q93.__[#ord], Q93.__[#rctec3], RCTE.DEPTH + 1,  \
            COALESCE(RCTE.PATH, 'null') || '/' || COALESCE(CAST(Q93.__[#rctec1] AS VARCHAR), 'null'),  \
            COALESCE(RCTE.TOTAL, 0)         +     COALESCE(     Q93.__[#rctec6], 0)  \
        FROM @fromtables Q93 JOIN RCTE ON Q93.__[#ord] = RCTE.__[#rctec1]  \
        WHERE DEPTH < @maxdepth  \
)   SELECT __[#ord], _symbol[#agg1 @agg](__[#rctec1]) AG1, __[#agg1](DEPTH) AGD,  \
            __[#agg1](TOTAL) AGT, MIN(PATH) MINP, MAX(PATH) MAXP, COUNT(*) CT FROM RCTE   \
        GROUP BY __[#ord] _optionalorderbyordlimitoffset


-- TODO: need to fix the rest of these:


-- ... with aggregate functions (implicit GROUP BY) in the "base" query;
-- and an implicit JOIN (but no explicit one)
--WITH RECURSIVE RCTE (AG1, MN2, MX3, CT4, DEPTH) AS (  \
--    SELECT _symbol[#agg1 @agg](_variable[#rctec1 @columntype]), MIN(_variable[#rctec2 @columntype]),  \
--                           MAX(_variable[#rctec3]),           COUNT(_variable[#rctec4]), 1  \
--        FROM @fromtables  \
--    UNION ALL  \
--    SELECT __[#rctec1], __[#rctec2], __[#rctec3], COUNT(__[#rctec4]), RCTE.DEPTH + 1  \
--        FROM @fromtables Q94, RCTE  \
--        WHERE Q94.__[#rctec1] < RCTE.AG1 AND DEPTH < @maxdepth  \
--)   SELECT @star FROM RCTE ORDER BY DEPTH



-- ... with aggregate functions (implicit GROUP BY), and an explicit JOIN
-- Note: we use _nonfulljointype because PostgreSQL does not support FULL JOIN here
--WITH RECURSIVE RCTE (AG1, MN2, MX3, CT4, DEPTH, PATH, TOTAL) AS (  \
--    SELECT _symbol[#agg1 @agg](_variable[#rctec1 @columntype]),   MIN(_variable[#rctec2 @columntype]),  \
--                           MAX(_variable[#rctec3 numeric]),     COUNT(_variable[#rctec4]), 1,  \
--                       CAST(__[#agg1](__[#rctec1]) AS VARCHAR), COALESCE(MAX(__[#rctec3]), 0)  \
--        FROM @fromtables WHERE __[#rctec2] IS NULL  \
--    UNION ALL  \
--    SELECT __[#agg1](Q95.__[#rctec1]),   MIN(Q95.__[#rctec2]),  \
--                 MAX(Q95.__[#rctec3]), COUNT(Q95.__[#rctec4]), RCTE.DEPTH + 1,  \
--            COALESCE(RCTE.PATH, '') || '/' || COALESCE(CAST(Q95.__[#rctec1] AS VARCHAR), ''),  \
--            COALESCE(RCTE.TOTAL, 0)        +  COALESCE(     Q95.__[#rctec3], 0)  \
--        FROM @fromtables Q95 @nonfulljointype JOIN RCTE ON  Q95.__[#rctec2] <= RCTE.MN2  \
--        WHERE DEPTH < @maxdepth  \
--)   SELECT @star FROM RCTE _optionalorderbyidlimitoffset

-- ... with an actual GROUP BY, in the RCTE
--WITH RECURSIVE RCTE (AG1, MN2, MX3, CT4, DEPTH, PATH, TOTAL) AS (  \
--    SELECT _symbol[#agg1 @agg](_variable[#rctec1 @columntype]),   MIN(_variable[#rctec2 @columntype]),  \
--                           MAX(_variable[#rctec3 numeric]),     COUNT(_variable[#rctec4]), 1,  \
--                       CAST(__[#agg1](__[#rctec1]) AS VARCHAR), COALESCE(MAX(__[#rctec3]), 0)  \
--        FROM @fromtables WHERE __[#rctec2] IS NULL GROUP BY DEPTH  \
--    UNION ALL  \
--    SELECT __[#agg1](Q96.__[#rctec1]),   MIN(Q96.__[#rctec2]),  \
--                 MAX(Q96.__[#rctec3]), COUNT(Q96.__[#rctec4]), RCTE.DEPTH + 1,  \
--            COALESCE(RCTE.PATH, '') || '/' || COALESCE(CAST(Q96.__[#rctec1] AS VARCHAR), ''),  \
--            COALESCE(RCTE.TOTAL, 0)        +  COALESCE(     Q96.__[#rctec3], 0)  \
--        FROM @fromtables Q96 @nonfulljointype JOIN RCTE ON  Q96.__[#rctec2] <= RCTE.MN2  \
--        WHERE RCTE.DEPTH < @maxdepth GROUP BY RCTE.DEPTH  \
--)   SELECT @star FROM RCTE _optionalorderbyidlimitoffset
----WITH CTE AS (  \
----    SELECT _variable[#ctec1 @columntype], @agg(_variable[#ctec2 @columntype]) AG1  \
----        FROM @fromtables[#tab] GROUP BY __[#ctec1]  \
----)   SELECT @star FROM __[#tab] Q83 @jointype JOIN CTE ON Q83.__[#ctec1] = CTE.__[#ctec1]  \
----        _optionalorderbyidlimitoffset

---- ... with an actual GROUP BY, in the RCTE and in the main (final) query
----WITH CTE AS (  \
----    SELECT _variable[#ctec1 @columntype], _symbol[#agg1 @agg](_variable[#ctec2 @columntype]) __[#ctec2]  \
----        FROM @fromtables[#tab] GROUP BY __[#ctec1]  \
----)   SELECT _variable[#grp], __[#agg1](Q84.__[#ctec2]) AG1, __[#agg1](CTE.__[#ctec2]) AG2  \
----        FROM __[#tab] Q84 @jointype JOIN CTE ON Q84.__[#ctec1] = CTE.__[#ctec1]  \
----        GROUP BY __[#grp] _optionalorderbyidlimitoffset


-- TODO: may need to fix this, though it partly works:


WITH RECURSIVE RCTE (_variable[#rctec1 @columntype], _variable[#rctec2 @columntype],  \
        _variable[#rctec3], DEPTH, PATH, TOTAL) AS (  \
    SELECT __[#rctec1], __[#rctec2], __[#rctec3], 1, CAST(__[#rctec1] AS VARCHAR),  \
            COALESCE(_variable[#rctec6 numeric], 0)  \
        FROM @fromtables[#tab1]  \
        WHERE __[#rctec2] = (SELECT MIN(__[#rctec2]) FROM __[#tab1])  \
    UNION ALL  \
    SELECT Q97.__[#rctec1], Q97.__[#rctec2], Q97.__[#rctec3], RCTE.DEPTH + 1,  \
            COALESCE(RCTE.PATH, '') || '/' || COALESCE(CAST(Q97.__[#rctec1] AS VARCHAR), ''),  \
            COALESCE(RCTE.TOTAL, 0)        +  COALESCE(     Q97.__[#rctec6], 0)  \
        FROM RCTE, @fromtables[#tab2] Q97  \
        WHERE Q97.__[#rctec2] = COALESCE(RCTE.__[#rctec1], (SELECT __[#rctec2] FROM __[#tab2] WHERE @idcol = Q97.@idcol + 1))  \
          AND DEPTH < @maxdepth  \
)   SELECT __[#rctec1], @agg(__[#rctec2]) AG1, MIN(DEPTH) MIND, MAX(DEPTH) MAXD,  \
             MIN(TOTAL) MINT, MAX(TOTAL) MAXT, MIN(PATH)  MINP, MAX(PATH)  MAXP  \
        FROM RCTE GROUP BY __[#rctec1]
--_optionalorderbyidlimitoffset
