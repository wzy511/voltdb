#!/usr/bin/env python

# This file is part of VoltDB.
# Copyright (C) 2008-2010 VoltDB L.L.C.
#
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:
#
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
# OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
# ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
# OTHER DEALINGS IN THE SOFTWARE.

{
    "basic": {"schema": "schema.py",
              "ddl": "DDL.sql",
              "template": "regression-basic.sql",
              "normalizer": "normalizer.py"},
    "basic-index": {"schema": "schema.py",
                    "ddl": "index-DDL.sql",
                    "template": "regression-basic-index.sql",
                    "normalizer": "normalizer.py"},
    "basic-strings": {"schema": "strings-schema.py",
                      "ddl": "strings-DDL.sql",
                      "template": "regression-basic-strings.sql",
                      "normalizer": "normalizer.py"},
    "basic-ints": {"schema": "int-schema.py",
                   "ddl": "int-DDL.sql",
                   "template": "regression-basic-ints.sql",
                   "normalizer": "normalizer.py"},
    "basic-decimal": {"schema": "decimal-schema.py",
                      "ddl": "decimal-DDL.sql",
                      "template": "regression-basic-decimal.sql",
                      "normalizer": "normalizer.py"},
    "basic-timestamp": {"schema": "timestamp-schema.py",
                        "ddl": "timestamp-DDL.sql",
                        "template": "regression-basic-timestamp.sql",
                        "template-hsqldb": "regression-basic-timestamp-hsql.sql",
                        "normalizer": "normalizer.py"},
    "basic-matview": {"schema": "matview-schema.py",
                      "ddl": "matview-DDL.sql",
                      "template": "regression-basic-matview.sql",
                      "normalizer": "normalizer.py"},
}
