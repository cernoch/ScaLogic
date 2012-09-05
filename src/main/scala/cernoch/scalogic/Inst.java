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

package cernoch.scalogic;

/**
 * Instantiation of an argument.
 *
 * <p>There are 3 types of Occurence
 * <ul>
 * <li><b>+</b>, <i>input</i>:
 *     occurence <u>must</u> be unified with an <u>output</u> occurence.</li>
 * <li><b>-</b>, <i>output</i>:
 *     occurence <u>can</u> be unified with another occurence.</li>
 * <li><b>#</b>, <i>const</i>:
 *     occurence <u>must not</u> be unified with another occurence.</li>
 * </ul>
 * </p>
 *
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
public enum Inst {
    
	IN('+'), OUT('-'), CONST('#');

	public final char symbol;
    Inst(char symbol) {
    	this.symbol = symbol;
    }

    public Inst resolve(char symbol) {
    	for (Inst i : values())
    		if (i.symbol == symbol)
    			return i;
    	return null;
    }
    
    @Override
    public String toString() {
        return Character.toString(symbol);
    }
}
