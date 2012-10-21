/*
 * Copyright (c) 2010 Radomír Černoch (radomir.cernoch at gmail.com)
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"), 
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

package cernoch.scalogic.exceptions;

/**
 * 
 *
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
public class SemanticsError extends RuntimeException {

	private static final long serialVersionUID = 753151452239874150L;

	/**
     * Creates a plain instance of <code>SyntaxError</code>.
     */
    public SemanticsError() {
    }


    /**
     * Constructor with the specified detail message.
     *
     * @param message the detail message.
     */
    public SemanticsError(String message) {
        super(message);
    }


    /**
     * Constructor of the exception, in case reason is known.
     *
     * @param cause the reason to throw the exception.
     */
    public SemanticsError(Throwable cause) {
        super(cause);
    }


    /**
     * Constructor with the detail message and the reason to throw {@code this}.
     *
     * @param message the detail message.
     * @param cause the reason to throw the exception.
     */
    public SemanticsError(String message, Throwable cause) {
        super(message, cause);
    }
}
