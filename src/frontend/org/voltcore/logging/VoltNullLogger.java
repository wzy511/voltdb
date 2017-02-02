/* This file is part of VoltDB.
 * Copyright (C) 2008-2016 VoltDB Inc.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with VoltDB.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.voltcore.logging;

/**
 * Implements the core logging functionality for VoltLogger as a null operation
 *
 */
public class VoltNullLogger implements VoltLogger.CoreVoltLogger {

    @Override
    public boolean isEnabledFor(Level level) {
        return false;
    }

    @Override
    public void l7dlog(Level level, String key, Object[] params, Throwable t) {}

    @Override
    public void log(Level level, Object message, Throwable t) {}

    @Override
    public long getLogLevels(VoltLogger[] loggers) {
        System.err.printf("This logger doesn't support getting log levels. You need Log4j.\n");
        return 0;
    }

    @Override
    public void setLevel(Level level) {}

}
