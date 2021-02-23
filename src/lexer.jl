#=

Lexer interface and implementation.
Basic building blocks for parsers using Token and IOShared types.

=#

"""
An Iterator for tokens from a stream.

Concrete types implement the iterator interface with
a token type as its element type, and the following functions with 
AbstractLexer parameter, which characterize the AbstractLexer API.


"""
abstract type AbstractLexer end


"""
ByteLexer is thought to work on byte streams with mostly ASCII based
source text in mind.

See [`next`](@ref) for its central function.


methodreads a sequence of bytes which form a raw token,
and returns its "token class".


which is defined as follows:

0 bytes if end-of data is reached, else:
a sequence b0, b1, ..., bn bytes is .returns 
syntax definition is given using "byte classes":
syntax[b+1] is a bitfield defining the byte class of byte b.

A (raw) token returned by a ByteLexer is a sequence of bytes withthe following property:

Bits 27..31: byte class, values 0..26. Any token with byte classes
for tokens

Bits 0..26: bit i set === byte is allowed as following byte in byte class i
"""
struct ByteLexer <: AbstractLexer
    source :: IOShared # lexer sets mark always to the begin of the current token.
    syntax::Vector{UInt32} # Bits: character class of 
end


function next(bl:ByteLexer) :: Nibble 
    mark(bl.source)
    fillup(bl.source,1)
    bl.source.writeofs==bl.source.readofs && return Nibble(0) # EOD
    class = syntax[]
        < count && io.ioread

end


#"Lexer elements are tokens"
#Base.eltype(::Type{AbstractLexer}) = Token

"type annotation for different Lexer flavours"
abstract type LexerVariant end


"marker type: there are alias encodings for tokens in a  [`LexerTable`](@ref) which have to be resolved"
struct AliasResolve <: LexerVariant end

"marker type: there are no alias encodings for tokens in a  [`LexerTable`](@ref) which have to be resolved"
struct NoAlias <: LexerVariant end

"""
Structure for token lists with fast lookup in a lexer.
  
list is an enumeration of all recognized values for a token category.

idx is used to accelerate and normalize lookup.

 * fast lookup: idx[b+1] == index in list of 1st token starting with byte b, or 0 (no token begins with b)

 * alias lookup in LexerTable{AliasResolve}: list[idx[256+i]] is the normalized token for token list[i] (usually==i)

asize denotes the size of the alfabet used for encoding, in case of byte encodings it is 256.
"""
struct LexerTable{T<:LexerVariant}
    list   :: HTokenVector # token list
    idx :: Vector{UInt32} # idx[b+1] == index in list of 1st token starting with byte b. b2list[257] == 1 + last index in list. idx[i+257]: index in list for 
end


"lexer table without aliases"
LexerTable(list::AbstractVector)= LexerTable(HTokenVector(list))

function LexerTable(list::HTokenVector)
    idxSize = 256
    idx = Vector{UInt32}(undef,idxSize)
    list = Base.sort!(compress(list,0))
    # find 1st entry in list per byte b
    i = 1
    bi = codeunit(list[i],1)
    for b in 0:255
        if b==bi
            idx[b+1] = i
            while i<=length(list) && b == codeunit(list[i],1)
                i += 1
                bi = codeunit(list[i],1)
            end
        else
            idx[b+1] = 0
        end
    end
    LexerTable{NoAlias}(list,idx)
end


"lexer table with "
function LexerTable(dict::AbstractDict) where T
    lt = LexerTable(keys(dict))

    idxSize = 256+length(dict)
    if T <: AliasResolve 
        idxSize +=length(list)
    end
    idx = Vector{UInt32}(undef,idxSize)
    list = Base.sort(list)
    # find 1st entry in list per byte b
    i = 1
    bi = codeunit(list[i],1)
    for b in 0:255
        if b==bi
            idx[b+1] = i
            while i<=length(list) && b == codeunit(list[i],1)
                i += 1
                bi = codeunit(list[i],1)
            end
        else
            idx[b+1] = 0
        end
    end
    # 
end



"""
Customizable basic lexer.

UTF8 can be processed with the restriction, that conditions cannot be put 
per character but only per unicode byte. Possible is for example, to treat
all 2-byte-UTF8-encodings with 1st byte C2 as control character and those
with 1st byte C3:D0 as letters.

Customization is done by function customize, its methods define conditions
for recognizing a token category and termination of a token.
"""
struct Lexer{T} <: AbstractLexer
    source :: IOShared # lexer sets mark always to the begin of the current tokensource line
    class::T # character class definition

    sym::LexerTable # list of defined tokens of type T_SYMBOL
    key::LexerTable # list of defined tokens of type T_KEY

    # current token state fields
    lineNo :: UInt32 # line number where current token starts. lexer sets mark position to begin of line
    linePos::UInt32 # position of token (1-based) within line (== 1+offset(token)-mark position)
    lines :: UInt32 # number of lines within this token (mostly 0, 1 for an end-of-line token and >0 for other line-spanning tokens)
    escapeIndex :: UInt32 # >0 is string index into token: unresolved escape char
    #token::BufferFlyToken # 


end


"""
Customizable basic lexer.

UTF8 can be processed with the restriction, that conditions cannot be put 
per character but only per unicode byte. Possible is for example, to treat
all 2-byte-UTF8-encodings with 1st byte C2 as control character and those
with 1st byte C3:D0 as letters.

Customization is done by function customize, its methods define conditions
for recognizing a token category and termination of a token.
"""
mutable struct ByteLexer{M<:Union{Nothing,Vector[Regex]}, E<:Union(Nothing,)} <: AbstractLexer
    source :: IOShared # lexer sets mark always to the begin of the current source line
    cls::Vector{UInt32} # character class definition
    sym::LexerTable # list of defined tokens of type T_SYMBOL
    key::LexerTable # list of defined tokens of type T_KEY

    # current token state fields
    lineNo :: UInt32 # line number where current token starts. lexer sets mark position to begin of line
    linePos::UInt32 # position of token (1-based) within line (== 1+offset(token)-mark position)
    lines :: UInt32 # number of lines within this token (mostly 0, 1 for an end-of-line token and >0 for other line-spanning tokens)
    escapeIndex :: UInt32 # >0 is string index into token: unresolved escape char
    #token::BufferFlyToken # 


end

function iterate(l::SimpleLexer, state::Int = 1)
    (state>=1) && (state <= l.source.ncodeunits()) && return nothing

end
















#= my old java source
/*
 * (C) Copyright 2002-2015 Rudolph Consulting, D-22926 Ahrensburg, Klaus-Groth-Str. 47. All rights reserved.
 */

package de.r2c.core;

import java.io.*;

import net.miginfocom.layout.LC;

//import java.io.ObjectStreamField;
//import java.io.UnsupportedEncodingException;
//import java.util.ArrayList;
//import java.util.Comparator;
//import java.util.Locale;
//import java.util.regex.Matcher;
//import java.util.regex.Pattern;
//import java.util.regex.PatternSyntaxException;
//import sail.COMPILE;
//import sail.Assert;

/**
 * A performance-oriented lexical analyzer and parser.
 * <p>
 *
 * This class has similar responsibilities as the well-known
 * {@link java.util.StringTokenizer}and {@link java.io.StreamTokenizer}
 * classes. It serves as the lexer for {@link Parser}.
 * <p>
 *
 * A Lexer instance represents the current token and is derived from Token. The
 * implementation is based on Str, using its capabilities to re-use Str objects
 * holding strings. By {@link #next()}you read the next token into this, which
 * can be accessed (and modified!) by all Str methods.
 * <p>
 *
 * Token types are defined in {@link LexerConst}, they are similar to the
 * TT_*-constants in {@link java.io.StreamTokenizer}. The main parsing routine
 * {@link #next}returns the token type and not a token string. The current
 * token string is simply this, accessible by the Str API.
 * <p>
 *
 * The design avoids object allocations as far as possible. You can parse
 * without any object allocations (except setup). You can read ahead and go back
 * in the token sequence. If you need frequent access to the tokens, store the
 * token sequence in a {@link ColToken}, which stores each token in elements of
 * some primitive type arrays.
 * <p>
 *
 * <b>Usage </b>
 * <p>
 *
 * The constructor returns an uninitialized Token. Before using it, you MUST
 * call {@link #init} to initialize the lexer, and set up the
 * lexer definition using define*() methods. The {@link #defineStandard} methods
 * sets up a default Lexer which already might fit your needs.
 * <p>
 *
 * Assign some text to parse by one of the {@link #setSource} methods, and start
 * parsing by calling {@link #next()}.
 * <p>
 *
 * Consider using the derived class {@link Parser}, if you have more than just
 * some tokens to parse. With a Parser, you can define your production rules
 * annotated with actions in a readable EBNF syntax, resulting in a fast,
 * reliable, debuggable Parser.
 * <p>
 *
 * <b>Design rationale </b>
 * <p>
 *
 * Its design is somewhere in between the very limited standard java
 * StreamTokenizer/StringTokenizer and a lexer generated by full-blown parser
 * generators like ANTLR, javaCC, JFLEX. It tries to implement the practical
 * needs on a 80/20 basis: 80% of whatever is needed with 20% of the efford of a
 * full-blown parser implementation.
 * <p>
 *
 * If you have a common parsing task, there are good chances that Lexer fulfills
 * 100% of your needs. A typical example is parsing data files in a custom
 * format, with a comlexity comparable to the CSV format or java properties
 * files. Lexer is capable of tokenizing most computer programming languages
 * like C (and derivatives like c++ and c#), java, Pascal etc. However, you
 * should consider using {@link Parser} and a grammar to implement the parser
 * instead of writing it by hand.
 * <p>
 *
 * The usual token types are defined, they are customizable to some extent and
 * additional user token types can be defined. However, there are no arbitrary
 * regular expressions to define tokens, because in most practical cases, the
 * predefined types are sufficient. You CAN however implement any desired token
 * type by a {@link LexerExtension} class. Some typical extensions are given by the
 * classes {@LexerNumber}and {@LexerKeyword}.
 * <p>
 *
 * No efford was made to design a compatible API. Like the Col classes,
 * compatibility is implemented by wrapper Classes in sail.util.
 * <p>
 *
 *
 * @author Robert Rudolph
 */
public class Lexer extends Token { // implements java.util.Enumeration{


    /**
     * ignore newline token in lexer?.
     * <p>
     *
     * If true, the {@link LexerConst.NEWLINE}token is skipped automatically by
     * {@link #next()}. - Line numbers are still processed by the lexer.
     * <p>
     *
     * Initial value: true.
     */
    public boolean ignoreNewline;


    /**
     * ignore whitespace token in lexer?.
     * <p>
     *
     * If true, the {@link LexerConst.WHITE} token is skipped automatically by
     * {@link #next()}.
     * <p>
     *
     * Initial value: true.
     */
    public boolean ignoreWhite;


    /**
     * ignore TAB token in lexer?.
     * <p>
     *
     * If true, the {@link LexerConst.TAB} token is skipped automatically by
     * {@link #next()}.
     * <p>
     *
     * Initial value: false.
     */
    public boolean ignoreTab;


    /**
     * ignore comment token in lexer?.
     * <p>
     *
     * If true, any {@link LexerConst.COMMENT}and {@link LexerConst.COMMENT2}
     * token is skipped automatically by {@link #next()}.
     * <p>
     *
     * Initial value: true.
     */
    public boolean ignoreComment;


    /**
     * escape char in {@link LexerConst#STRING}and in {@link LexerConst#CHAR}
     * tokens.
     * <p>
     *
     *
     * @see #defineString
     */
    public char symbolStringEscape;


    /**
     * current line number.
     * <p>
     *
     * Initialized by {@link #setSource} to 1 and incremented on every NEWLINE
     * token.
     */
    protected int lineNo;


    /**
     * starting position of the current line in buffer.
     * <p>
     *
     * offset into {@link #buf}of the 1st char of the current line.
     */
    protected int linePos;


    /**
     * Token type of a char out-of-alphabet.
     * <p>
     *
     * This value is assigned to any character which is not included in the
     * alphabet range.
     * <p>
     * Default: {@link LexerConst#ILLEGAL}. Change by a call to
     * {@link #init(int,byte)}
     */
    protected byte defaultTokenType;


    /**
     * Token type of a char out-of-alphabet, masked to basic types (no flags).
     * <p>
     *
     * == {@link #defaultTokenType} & 31, set in {@link #init(int, int)}
     */
    protected byte defaultTokenTypeMasked;


    /**
     * the basic text to parse.
     * <p>
     *
     * PRECOND: base is changed by this ONLY. <p>
     * 
     */
    protected Str base;


    /**
     * index in {@link #parseBuf}, next char to parse (end of the current
     * token).
     * <p>
     *
     * Is changed by next()
     */
    protected int parsePos;


    /**
     * index in {@link #parseBuf}, first char of the current token.
     * <p>
     *
     * Is changed by next(), stays unchanged if some processing
     * changes the content of this.
     */
    protected int tokenStart;

    
//    /**
//     * extended token code as returned by next(). <p>
//     * 
//     * raw type: 0..15 according to {@link LexerConst} definitions,
//     * extension according to {@link #next()}.
//     */
//    protected int code;
    

    /**
     * internal char buffer, identical to the buffer of {@link #base}
     */
    protected char[] parseBuf;


    /**
     * offset into parseBuf, the end of the data to parse.
     */
    protected int parseEnd;


    /**
     * Definition of "double symbols" - symbol tokens consisting of 2 char-s.
     * <p>
     *
     * doubleSymbol[ch] is an array where all values are 0 except the defined
     * double symbols, which are stored with their symbol value (ch1*256+ch2).
     * <p>
     *
     * Attention: the lexer relies on correct settings:
     * <p>
     *
     * doubleSymbol[ch] != null if {@link #lexerType}[ch]==
     * {@link LexerConst#DOUBLESYMBOL}.
     * <p>
     * doubleSymbol[ch][ch2] == ch*256+ch2 indicates a doublesymbol (ch,ch2).
     * <p>
     * doubleSymbol[ch][ch2] == 0 indicates no doublesymbol, ch is treated as
     * (single) symbol.
     * <p>
     */
    protected char[][] doubleSymbol;


    /**
     * symbol type of the end-of-line symbol.
     * <p>
     *
     * Must be the last char of the end-of-line-sequence. This char is the only
     * one for which lexerType[ch]==NEWLINE is true.
     * <p>
     *
     * If you want to terminate the line by every LF, you can declare CR a
     * whitespace char and set lineSymbol to 10. This is the default, which
     * works well on windows and unix (but not on the MAC, here declare CR as
     * end-of-line char).
     *
     */
    protected char symbolEOL;


    /**
     * symbol or doublesymbol encoding of a comment beginning.
     * A doublesymbol encoding requires to flag the 1st symbol
     * as {@link LexerConst#COMMENT2}, otherwise use {@link LexerConst#COMMENT} 
     */
    protected char symbolLineComment;


    /**
     * symbol or doublesymbol encoding of a comment beginning.
     * A doublesymbol encoding requires to flag the 1st symbol
     * as {@link LexerConst#COMMENT2}, otherwise use {@link LexerConst#COMMENT} 
     */
    protected char symbolBeginComment;


    /**
     * symbol or doublesymbol encoding of a (multiline) comment end.
     */
    protected char symbolEndComment;


    protected char symbolFraction;


    protected char symbolExponent;


    protected char symbolUppercaseExponent;


    /**
     * token type up to which newlines are allowed.
     * <p>
     *
     * Used in CHAR and STRING processing: if the token type is less or equal to
     * newlineInToken, newline is allowed in that token type. Default: 0 (no
     * newline allowed).
     *
     * @see #defineStringEscape
     */
    protected int newlineInToken;


    /**
     * Position of first escape sequence in a CHAR or STRING. <p>
     *
     * 0: no escape.
     */
    protected int firstEscape; // escape in STRING: absolute index


    protected int lastEscape; // escape in STRING: absolute index


    /**
     * type classification depending on the first char of the token.
     */
    protected byte[] lexerType;
    // TODO trenne die FLAGs 'raus in eigenes array ==> spart viele & - Maskierungen

    /**
     * == lexerType.length
     */
    protected int lexerTypeSize;

    //    /**
    //     * true, if char is allowed in a name token (except 1st char).
    //     */
    //    protected boolean[] allowedInName;

    protected Col tokenNames;


    /**
     * default lexer names
     */
    protected static Col __tokenTypeNames;


    /**
     * Number of times EOF is allowed to parsed by next() without error.
     * <p>
     *
     * When the end-of-input is reached, next() returns {@link LexerConst#NONE}.
     * In order to prevent deadloop behavior at end-of-input on productions like
     * <code> (!ANY)* ANY </code>, next() checks countEOF on every attempt to
     * read a symbol at EOF. If countEOF is negative, an error is reported,
     * otherwise it is decremented.
     *
     * Default: 0, i.e. the first attempt to read behind end-of-input will
     * result in an error.
     */
    protected int countEOF;


    /**
     * Plugin of user-extensions to this lexer.
     * <p>
     *
     * Can be used to chain input sources or to implement user-defined token
     * types.
     */
    protected LexerExtension extension;


	protected int savedBaseStart;
	
	
	private Str parseHelper;


    private static java.lang.ref.SoftReference __lexerCache;


    /**
     * acqire a Lexer from the Lexer cache or construct a new one
     * @throws Exception 
     */
    public static Lexer acquireLexer(LexerUser user) throws Exception {
        Lexer ret = null;
        if (__lexerCache != null) {
            // search a cached lexer
            ColMap cache = (ColMap) __lexerCache.get();
            if (cache != null) {
                synchronized (cache) {
                    int lexerIdx = cache.locateKey(user.getLexerKey());
                    if (lexerIdx != CONST.NOTFOUND) {
                        ret = (Lexer) cache.getObject(lexerIdx);
                        cache.resize(lexerIdx, -1);
                        return ret;
                    }
                }
            }
        }
        return user.newLexer();
    }


    /**
     * release a Lexer to the Lexer cache.
     * <p>
     *
     * Setting up a Lexer is quite time- and memory-consuming, because many
     * Lexer-s have to cover the full char range. The symbol tables consume
     * about 256 KByte, in such a case.
     * <p>
     *
     * Lexer maintains a Lexer cache with weak references - the garbage
     * collector may collect them in need of memory.
     * This method puts a Lexer into that cache, a call, which is
     * recommended to be used if you expect to need some lexer with
     * the same setup at different points in your application.
     * @throws Exception 
     */
    public void releaseLexer(LexerUser user) throws Exception {
    	releaseSource();
        ColMap cache = null;
        if (__lexerCache != null) {
            cache = (ColMap) __lexerCache.get();
        }
        if (cache == null) {
            // create new Lexer cache.
            cache = new ColMap();
            cache.init(new ColObject(), new ColObject());
            __lexerCache = new java.lang.ref.SoftReference(cache);
        }
        // store lexer in cache
        synchronized (cache) {
            cache.append(user.getLexerKey(), this);
        }
    }


    //
    //    /** Calculates the number of times that this tokenizer's
    //     * <code>nextToken</code> method can be called before it generates an
    //     * exception. The current position is not advanced. <p>
    //     *
    //     * This method is implemented for compatibility purposes only and should
    //     * not be used in new applications: acually, calculating the number of
    // tokens
    //     * implies parsing until end-of-data.
    //     *
    //     * @return the number of tokens remaining in the string using the current
    //     * delimiter set.
    //     * @see java.util.StringTokenizer#nextToken()
    //     * @deprecated
    //     */
    //    public int countTokens() {
    //    }
    //
    //    /** Returns the same value as the <code>hasMoreTokens</code>
    //     * method. It exists so that this class can implement the
    //     * <code>Enumeration</code> interface.
    //     *
    //     * @return <code>true</code> if there are more tokens;
    //     * <code>false</code> otherwise.
    //     * @see java.util.Enumeration
    //     * @see java.util.StringTokenizer#hasMoreTokens()
    //     *
    //     */
    //    public boolean hasMoreElements() {
    //    }
    //
    //    /** Tests if there are more tokens available from this tokenizer's
    // string.
    //     * If this method returns <tt>true</tt>, then a subsequent call to
    //     * <tt>nextToken</tt> with no argument will successfully return a token.
    //     *
    //     * @return <code>true</code> if and only if there is at least one token
    //     * in the string after the current position; <code>false</code>
    //     * otherwise.
    //     *
    //     */
    //    public boolean hasMoreTokens() {
    //    }
    //

    
    /**
     * Get the lexer type of a char, with correct default
     * @param ch
     * @return
     */
    protected int lexerType(char ch){
        if (ch<lexerTypeSize){
            return (lexerType[ch]) & LexerConst.TOKENTYPEMASK;
        } 
        return defaultTokenTypeMasked;
    }
    
    
    /**
     * reads a token and returns the (enhanced) token type code.
     * <p>
     *
     * The basic token types are defined in {@link LexerConst},
     * they are stored in the type field of this. The following
     * types are not returned by next():
     * <ul>
     * <li>{@link LexerConst#SYMBOL}: the character code of the
     * symbol is returned.
     * </li>
     * <li>{@link LexerConst#DOUBLESYMBOL}: the encoded double
     * symbol is returned as (1st char + 256*2nd char).
     * </li>
     * <li>{@link LexerConst#EXT}: the lexer extension decides
     * what to return.
     * </li>
     * <li>{@link LexerConst#ILLEGAL}: an exception is thrown.
     * </li>
     * </ul>
     * <p>
     *
     * The lowest 8 bits of the 1st character of the token are
     * stored in the level field, except for the following 
     * token types:
     * 
     * <ul>
     * <li>{@link LexerConst#STRING}: The lowest 8 bits of the 
     * 1st character after the STRING delimiter are stored in the level 
     * field.
     * </li>
     * <li>{@link LexerConst#CHAR}: The lowest 8 bits of the 
     * 1st character after the CHAR delimiter are stored in the level 
     * field.
     * </li>
     * <li>{@link LexerConst#USER}: The lowest 8 bits of the 
     * 2nd character are stored in the level field.
     * </li>
     * </ul>
     * 
     * @return token code, an extended token type, see above.
     * The 
     * stored in the instance and requestable by {{@link #getCode}.
     */
    // TODO nur einen Token code speichern (4-Bit oder int)
    public int next() {
    	// typeLevel = LexerConst.NONE; // maybe unnecessary
        char[] buf = this.parseBuf;
        final byte[] lexerType = this.lexerType;
        this.buffer = buf; // default: we reference text in the parse buffer
        final int parseEnd = this.parseEnd;
        this.shared = buf.length; // to prevent changes in the original buffer
        int pos = this.tokenStart = this.parsePos;
        try {
            nexttoken: do { // read next token.
                this.start = pos;
                char ch;
                if (pos<parseEnd){
	                ch = buf[pos++];
	                this.typeLevel = lexerType(ch);
	                // work on following char-s: pos points to NEXT char, ch is
	                // CURRENT char
	                switch (typeLevel) {
	                case LexerConst.WHITE: {
	                    while ((pos<parseEnd)&&(lexerType(buf[pos]) == LexerConst.WHITE)) {
	                        ++pos;
	                        // TODO debug 'raus
	                        if (COMPILE.DEBUG) {
	                            if (pos >= buf.length) {
	                                tokenError("buffer overrun in white space scan");
	                            }
	                        }
	                    }
	                    if (this.ignoreWhite){
	                        continue nexttoken;
	                    }
	                    break nexttoken;
	                }
	                case LexerConst.NEWLINE: {
	                    if (pos<parseEnd){
	                    	char c2 =buf[pos];
	                    	if ((lexerType(c2) == LexerConst.NEWLINE)&&(c2!=ch)) {
	                    		++pos; // skip 2nd char of newline sequence
	                    	}
		                    ++this.lineNo;
		                    this.linePos = pos;
	                    }

	                    if (this.ignoreNewline){
	                        continue nexttoken;
	                    }
	                    break nexttoken;
	                }
	                case LexerConst.COMMENT: {
	                    // we have definitely a comment, determined by the current
	                    // char (single symbol comment delimiter)
	                    if (ch == this.symbolLineComment) {
	                        // o.k., we have a line comment
	                        pos = indexOfBaseEOL(pos);
	                        if (this.ignoreComment)
	                            continue nexttoken;
	                        break nexttoken;
	                    }
	                    // we have a (multiline) comment
	                    pos = locateSymbol(pos, this.symbolEndComment);
	                    if (this.ignoreComment){
	                        continue nexttoken;
	                    }
	                    break nexttoken;
	                }
	                case LexerConst.COMMENT2: {
	                    // we have a special case of a doublesymbol:
	                    // the current char matches the beginning of a comment
	                    // doublesymbol.
	                	if (pos<parseEnd){ // if handles the rare case of doublesymbol char as last char of source
		                    char c = (char) ((ch<<8) + buf[pos]);
		                    if (c == this.symbolLineComment) {
		                        // fine. is a line comment
		                        // process until EOLN or EOF and exit
		                        pos = indexOfBaseEOL(pos + 1);
		                        if (this.ignoreComment){
		                            continue nexttoken;
		                        }
		                        this.typeLevel = LexerConst.COMMENT; // corrects level
		                        break nexttoken;
		                    }
		                    if (c == this.symbolBeginComment) {
		                        // fine. is a (multiline) comment
		                        // process until the end-comment-symbol and exit
		                        pos = locateSymbol(pos + 1, this.symbolEndComment);
		                        if (this.ignoreComment)
		                            continue nexttoken;
		                        typeLevel = LexerConst.COMMENT;
		                        break nexttoken;
		                    }
	                	}
	                    // o.k., no comment.
	                    // continue with normal doubleSymbol processing.
	                    typeLevel = LexerConst.DOUBLESYMBOL;
	                }
	                case LexerConst.DOUBLESYMBOL: {
	                	if (pos<parseEnd){ // if handles the rare case of doublesymbol char as last char of source
		                    // PRECOND: doubleSymbol[ch] != null if lexerType[ch]==DOUBLESYMBOL !!
		                	char c = buf[pos];
		                	if (c<LexerConst.SIZE_IN_DOUBLESYMBOL){
		                        typeLevel = this.doubleSymbol[ch][c];
		                        if (typeLevel != 0) {
		                            ++pos; // token is a 2-char-symbol
		                            break nexttoken;
		                        }
		                    }
	                	}
	                    // else: continue with SYMBOL!
	                }
	                case LexerConst.SYMBOL: {
	                	typeLevel = ch; // convert to the given symbol token type
	                    break nexttoken;
	                }
	                case LexerConst.TAB: {
	                    // single char token, type is ready
	                    if (this.ignoreTab)
	                        continue nexttoken;
	                    break nexttoken;
	                }
	                case LexerConst.SIGNED: {
	                    if (pos>=parseEnd){
	                    	// last symbol in stream
	                    	typeLevel = ch;
	                        break nexttoken;
	                    }
                    	char ch2 = buf[pos];
		                if (lexerType(ch2) != LexerConst.INTEGRAL) {
	                        // is a single sign - typically '+' or '-'.
	                    	// continue as SYMBOL or DOUBLESYMBOL
	                    	if ((ch2<LexerConst.SIZE_IN_DOUBLESYMBOL)&& (doubleSymbol!=null)){
		                    	char[] ds = doubleSymbol[ch];
		                    	if (ds!=null){ // here, it would be NO structure error to have null
		                    		typeLevel = ds[ch2];
		                    		if (typeLevel!=0){
		                    			// is a DOUBLESYMBOL
		                    			++pos;
		                                break nexttoken;
		                    		}
		                    	}
	                    	}
	                    	// is a symbol.
	                        this.typeLevel = ch;
	                        break nexttoken;
	                    }
	                    // is a signed number - continue with INTEGRAL
	                    ++pos; // skip 1st digit
                        this.typeLevel = LexerConst.INTEGRAL;
	                }
	                case LexerConst.INTEGRAL: {
	                	try{ // omit the range chacks on pos<parseEnd 
		                    while ((lexerType(ch=buf[pos]) == LexerConst.INTEGRAL)) {
		                        ++pos;
		                    }
		                    if (ch == this.symbolFraction) {
		                        this.typeLevel = LexerConst.FRACTION;
		                        ++pos;
			                    while ((lexerType(ch=buf[pos]) == LexerConst.INTEGRAL)) {
		                            ++pos;
		                        }
		                    }
		                    if ((ch == this.symbolExponent) || (ch == this.symbolUppercaseExponent)) {
		                        typeLevel = LexerConst.FRACTION;
		                        // exponential part: skip exponent, if present skip sign
		                        if (++pos>=parseEnd){
			                        tokenError("EOF reached, but exponent in (fractional) number expected");
		                        }
		                        if (lexerType(buf[pos]) == LexerConst.SIGNED) {
		                            ++pos;
		                        }
		                        // collect all following digits
		                        while ((lexerType(buf[pos]) == LexerConst.INTEGRAL)) {
		                            ++pos;
		                        }
		                    }
	                	}catch (ArrayIndexOutOfBoundsException e){
	                		// nothing to do - simply end of number.
	                		// correction of pos is done in next statement
	                	}
	                	if (pos>parseEnd){
	                		pos=parseEnd;
	                	}
	                    break nexttoken;
	                }
	                case LexerConst.IDENT: {
	                	if (defaultTokenType<0){
	                        while ((pos<parseEnd)&&(((ch=buf[pos])>=this.lexerTypeSize)||(lexerType[ch] < 0))) { 
	                            ++pos;
	                        }
	                        break nexttoken;
	                	}else{ // char codes out of alfabet are not allowed in identifiers 
	                        while ((pos<parseEnd)&&(((ch=buf[pos])<this.lexerTypeSize)&&(lexerType[ch] < 0))) {
	                            ++pos;
	                        }
	                        break nexttoken;
	                	}
	                }
	                case LexerConst.USER: {
                        while ((pos<parseEnd)&&((ch=buf[pos])<this.lexerTypeSize)&&((lexerType[ch]&LexerConst.FLAG_USER) != 0)) {
                            ++pos;
                        }
                        break nexttoken;
	                }
	                case LexerConst.CHAR:
	                case LexerConst.STRING: {
	                    char c;
	                    this.firstEscape = 0; // flag for: no first escape - 0 is delimiter pos, so outside of token!
	                    final char stringEscape = this.symbolStringEscape;
	                    // POSTCOND of the if: pos is pos of terminating char, not found if pos>=parseEnd
	                    if (stringEscape != 0) {
	                        // escape search
	                        while ((pos<parseEnd)&&(c = buf[pos]) != ch) {
	                            if (c == stringEscape) {
	                                // build token in new buffer
	                                this.lastEscape = pos;
	                                if (this.firstEscape == 0) {
	                                    this.firstEscape = pos;
	                                }
	                                ++pos;
	                                // is safe - >= parseEnd is checked below
	                            }
	                            ++pos;
	                            if ((c <32) &&(lexerType[c]==LexerConst.NEWLINE)){
	                                if (this.newlineInToken < typeLevel) {
	                                    this.end = pos-1;
	                                    tokenError("newline not allowed in string");
	                                }
                                	char c2=c;
            	                	if ((pos<parseEnd)&&((c2=buf[pos])!=c)&&(lexerType(c2) == LexerConst.NEWLINE)){
                                		++pos; // skip 2nd char of newline sequence
                                	}
            	                    ++this.lineNo;
            	                    this.linePos = pos;
	                            }
	                        }
	                    } else {
	                        // delimiter escape by doubling the delimiter
	                        while (pos<parseEnd) {
	                            if (((c = buf[pos]) != ch)) {
	                                // sub-updateState: process newline
		                            if ((c <32) &&(lexerType[c]==LexerConst.NEWLINE)){
	                                    if (this.newlineInToken < typeLevel) {
	                                        this.end = pos;
	                                        tokenError("newline not allowed in string");
	                                    }
	                                	char c2=c;
	            	                	if ((pos+1<parseEnd)&&((c2=buf[pos+1])!=c)&&(lexerType(c2) == LexerConst.NEWLINE)){
	                                		++pos; // skip 2nd char of newline sequence
	                                	}
	                                    ++this.lineNo;
	                                    this.linePos = pos+1;
	                                }
	                            }else{
		                            // delimiter found. Exit if not doubled delimiter
		                            if ((pos+1 >= parseEnd) || (buf[pos+1] != ch)) {
		                                break;
		                            }
		                            this.lastEscape = pos;
		                            if (this.firstEscape == 0) {
		                                this.firstEscape = pos;
		                            }
	                            }
		                        ++pos;
	                        }
	                    }
	        	        if (++pos > parseEnd) {
	        	            this.end = this.start;
	        	            tokenError("string token did not terminate before end of input data");
	        	        }
	                    break nexttoken;
	                }
	                case LexerConst.ILLEGAL: {
	                    tokenError("character not allowed, code: " + (int) ch);
	                }
	                default: // LexerConst.EXT
	                {
	                	if (this.extension==null){
	                        tokenError("No Lexer extension to process Lexer token type: " + typeLevel);
	                	}
                        typeLevel = this.extension.next(typeLevel, ch, buf, pos);
                        this.end = this.parsePos; 
                        return typeLevel;
	                }
	                } // end of switch
                } // end of: if not EOF
                // attention: all case statements have to terminate by break/continue nexttoken or return.

                // this location is reached ONLY in the EOF case (pos>parseEnd).
                // try to extend input data
                typeLevel = LexerConst.NONE;
                if (this.extension != null) {
                    if (this.extension.moreInput()) {
                        // o.k., we got more input.
                        continue nexttoken;
                    }
                }
                // keep old pos
                if (this.countEOF-- < 0) {
                    tokenError("try to read beyond end of input data");
                }
                break nexttoken;
            } while (true);
            // o.k. symbol is parsed completely.
            this.end = pos;
            this.parsePos = pos;
            return typeLevel;
        } catch (Exception e) {
            this.end = pos;
            if (pos >= parseEnd) {
                this.end = this.start;
                tokenError("token did not terminate before end of input data");
            }
            tokenError(e.toString());
            return 0; // never reached
        }
    }
    
    
    /**
     * unescape if token type is CHAR or STRING.
     * @return this
     */
    public Lexer checkUnEscape(){
    	if ((this.typeLevel==LexerConst.CHAR)||(this.typeLevel==LexerConst.STRING)){
    		unEscape();
    	}
    	return this;
    }

    
    /**
     * Remove CHAR/STRING delimiters and escapes inside from the token.
     * <p>
     *
     *
     * Deletes one char (the delimiters) at the begin and end of this.
     * Un-escapes escape sequences inside the token. PRECOND begin and end char
     * are both a {@link LexerConst#CHAR}or {@link LexerConst#STRING}
     * delimiter. Token type is not checked - to unescape only if token type
     * is CHAR or STRING, use {@link #checkUnEscape()}
     * is STRING or CHAR, use 
     *
     */
    public Lexer unEscape() {
        if (COMPILE.ASSERT) {
            Assert.lessEqual(2, this.length(), this);
            int tb = this.lexerType[charAt(0)];
            int te = this.lexerType[this.buffer[this.end - 1]];
            Assert.isEqual(tb, te, this);
            Assert.lessEqual(LexerConst.STRING, tb, this);
            Assert.lessEqual(tb, LexerConst.CHAR, this);
        }
        //
        char esc = this.symbolStringEscape;
        if (esc == 0) {
            esc = charAt(0);
        }
        // delete delimiters
        ++this.start;
        --this.end;
        // set up search range in relative coords
        int escapePos = this.firstEscape - this.start;  
        int escapeLast = this.lastEscape - this.start;
        // un-escape if necessary
        while ((escapePos >= 0) && (escapePos <= escapeLast)) { // ">=0" because: firstEscape==1 ==> escapePos ==0
            delete(escapePos, escapePos+1);
            --escapeLast; // 1 char was deleted
            if (this.symbolStringEscape != 0) {
                // extension case: get replacement char
                setCharAt(escapePos, this.extension.escape(escapePos,charAt(escapePos)));
            }
            escapePos = indexOf(esc, escapePos + 1);
        }
        return this;
    }


    //    public int skipUntil(int type){
    //
    //    }
    
    

    /**
     * Search a position in the base buffer. <p>
     * 
     * Indended use is a situation where the normal lexer rules are
     * invalid or ineffectice. The position returned can be used with the
     * methods {@link #setParsePosition(int)}, {@link #text(int, int)},
     * {@link #text(Str, int, int)} to set the lexer position or to
     * retrieve text from the lexer base buffer.
     * @param pos from where to search in the base buffer. A valid
     * value is usually retrieved by {@link #getParsePosition()}
     * @param match what to search
     * @return a position in the current lexer base buffer or -1.
     * 
     */
    public int indexOfBase(int pos, char match) {
        return base.indexOf(match,pos);
    }

    /**
     * Search a position in the base buffer. <p>
     * 
     * Indended use is a situation where the normal lexer rules are
     * invalid or ineffectice. The position returned can be used with the
     * methods {@link #setParsePosition(int)}, {@link #text(int, int)},
     * {@link #text(Str, int, int)} to set the lexer position or to
     * retrieve text from the lexer base buffer.
     * @param pos from where to search in the base buffer. A valid
     * value is usually retrieved by {@link #getParsePosition()}
     * @param match what to search
     * @return a position in the current lexer base buffer or -1.
     * 
     */
    public int indexOfBase(int pos, Str match) {
        return indexOf(this.parseBuf, 0, this.parseEnd, match.buffer,
                match.start, match.end, pos);
    }

    /**
     * Search a position in the base buffer. <p>
     * 
     * Indended use is a situation where the normal lexer rules are
     * invalid or inefficient. The position returned can be used with the
     * methods {@link #setParsePosition(int)}, {@link #text(int, int)},
     * {@link #text(Str, int, int)} to set the lexer position or to
     * retrieve text from the lexer base buffer.
     * @param pos from where to search in the base buffer. A valid
     * value is usually retrieved by {@link #getParsePosition()}
     * @param match what to search
     * @return a position in the current lexer base buffer or -1.
     * 
     */
    public int indexOfBase(int pos, FindStr match) {
        return match.searchIn(this.parseBuf, 0, this.parseEnd,pos);
    }


    public void require(int code, String errorMessage) {
        if (code != this.typeLevel) {
            tokenError(errorMessage);
        }
    }


    /**
     * read next symbol and test symbol type. <p>
     *
     * @param type the symbol type required - see {@link LexerConst}
     * @throws Exception 
     */
    public void nextRequire(int code) throws Exception {
        next();
        require(code);
    }


    /**
     * read next symbol and test symbol type. <p>
     *
     * @param type the symbol type required - see {@link LexerConst}
     * @throws Exception 
     */
    public void nextRequire(String ident) throws Exception {
        next();
        require(ident);
    }


    public void require(int code) throws Exception {
        if (code != this.typeLevel) {
            tokenError("token " + getTokenName(code) + " required but " + getTokenName(typeLevel),"found");
        }
    }


    public void require(String ident) throws Exception {
        if (!this.equals(ident)){
            tokenError("token " + ident, "required");
        }
    }


    public void requireIgnoreCase(String ident) throws Exception {
        if (!this.equalsIgnoreCase(ident)){
            tokenError("token " + ident, "required");
        }
    }


    /**
     * Search a position of a lexer symbol in the base buffer. <p>
     * 
     * Indended use is a situation where the normal lexer rules are
     * invalid or ineffectice. The position returned can be used with the
     * methods {@link #setParsePosition(int)}, {@link #text(int, int)},
     * {@link #text(Str, int, int)} to set the lexer position or to
     * retrieve text from the lexer base buffer.
     * @param pos from where to search in the base buffer. A valid
     * value is usually retrieved by {@link #getParsePosition()}
     * @param match what to search
     * @return a position in the current lexer base buffer or -1.
     * 
     */
    /**
     * scans for a (double)symbol in the lexer base buffer. <p>
     * 
     *
     * Any NEWLINE found in the skipped text is processed. If you call
     * indexOfBaseSymbol outside of a next() call, i. e. without consuming the
     * skipped text, save this.lineNo and this.linePos before the call and
     * restore them afterwards.
     *
     * @param pos
     *            position to start, MUST be a valid index into buf
     * @param symbol
     *            1-char (0..255) or 2-char (256*ch1+ch2) symbol to search
     * @return position directly after symbol
     * @throws ParseException
     *             if not found.
     */
    protected int locateSymbol(int pos, char symbol) {
    	final char[] buf = this.parseBuf;
    	buffer = buf;
    	start = parsePos;
        try{
	        if (symbol <= 255) {
	            // single char symbol
	            char c;
	            while ((c = buf[pos]) != symbol) {
	                ++pos;
	                if ((c<32)&&(lexerType[c] == LexerConst.NEWLINE)) {
                    	char c2;
	                	if ((pos<parseEnd)&&((c2=buf[pos])!=c)&&(lexerType(c2) == LexerConst.NEWLINE)){
                    		++pos; // skip 2nd char of newline sequence
                    	}
	                    ++this.lineNo;
	                    this.linePos = pos;
	                }
	            }
	        } else {
	            // TODO double char symbol: use simplified boyer-moore?!
	            char c2 = (char) (symbol & 255);
	            char c1 = (char) (symbol >> 8);
	            do {
	                char c;
	                while ((c = buf[pos]) != c1) {
	                    ++pos;
		                if ((c<32)&&(lexerType[c] == LexerConst.NEWLINE)) {
	                    	char cc=c;
		                	if ((pos<parseEnd)&&((cc=buf[pos])!=c)&&(lexerType(cc) == LexerConst.NEWLINE)){
	                    		++pos; // skip 2nd char of newline sequence
	                    	}
		                    ++this.lineNo;
		                    this.linePos = pos;
		                }
	                }
	            } while ((buf[++pos] != c2));
	        }
        }catch (IndexOutOfBoundsException e){
        	// nothing to do, handled by finally code
        }finally{
	        // advance 1 char (last part of symbol), check notfound condition
	        if (++pos > parseEnd) {
	            this.end = this.start;
	            tokenError("EOF reached, symbol not found:", symbolToString(symbol));
	        }
        }
        return pos;
    }


    
    /**
     * returns position of the next line terminator.
     * <p>
     *
     * the next char classified as newline is searched, beginning with pos. the index in buf is
     * returned, or end if not found.
     * <p>
     *
     * @param pos
     *            start of searching
     * @return position of the next NEWLINE char or end-of-data position
     * within the base buffer of this lexer.
     */
    protected int indexOfBaseEOL(int pos) {
        // current line at EOF is an empty line
        char[] buf = this.parseBuf;
        while ((pos<this.parseEnd)&&(lexerType(buf[pos])!=LexerConst.NEWLINE)) {
        	++pos;
        }
        return pos;
    }

    /**
     * search pattern within unparsed text 
     * @param pattern preprocessed search pattern
     * @return text until pattern in this, parsePosition is set behind pattern
     * @throws ParseException if not found
     */
	public void parseUntil(FindStr pattern){
		start = parsePos;
    	int pos = pattern.searchIn(parseBuf, parsePos, parseEnd, parsePos);
    	if (pos>=parsePos){
    		end = pos;
    		parsePos = pattern.patternEnd(pos);
    		return;
    	}
    	end = parsePos;
    	tokenError("pattern not found:",pattern.toString());
    }

	
    /**
     * consume unparsed text until pattern into this. <p>
     * 
     * On success, this contains all unparsed text until but excluding pattern,
     * and all text including pattern is consumed.
     * 
     * @param pattern preprocessed search pattern
     * @return patternID, see {@link FindStrs#patternID(long)}
     * @throws ParseException if not found
     */
    public int parseUntil(FindStrs pattern){
    	start = parsePos;
    	long ret = pattern.searchIn(parseBuf, parsePos, parseEnd);
    	if (ret>=0){
    		end = FindStrs.patternPos(ret);
    		parsePos = pattern.patternEnd(ret);
    		return FindStrs.patternID(ret);
    	}
    	end = parsePos;
    	tokenError("pattern not found:",pattern.toString());
    	return -1; // never reached
    }

    
    /**
     * consume unparsed text until given (double)symbol into this. <p>
     * 
     * On success, this contains all unparsed text until but excluding symbol,
     * and all text including symbol is consumed.
     * @param pattern
     * @return pattern found, parsePosition is set behind pattern
     * @throws ParseException if not found
     */
    public void parseUntilSymbol(char symbol){
    	start =parsePos;
    	parsePos = locateSymbol(parsePos, symbol);
    	end = parsePos-1;
    	if (symbol>255){
    		--end;
    	}
    }

    
	
    /**
     * consume unparsed text until pattern into this. <p>
     * 
     * On success, this contains all unparsed text until but excluding pattern,
     * and all text including pattern is consumed.
     * 
     * @see Str#parseUntil(Str, char)
     * @param pattern
     * @return patternID, see {@link FindStrs#patternID(long)}
     * @throws ParseException if not found
     */
    public void parseUntil(char pattern){
    	start = parsePos;
    	int pos  = base.indexOf((int)pattern,parsePos);
    	if (pos>=0){
    		end = pos;
    		parsePos = pos+1;
    		return;
    	}
    	end = parsePos;
    	tokenError("char not found: "+pattern);
    }
    
	
    /**
     * consume unparsed text until pattern into this. <p>
     * 
     * On success, this contains all unparsed text until but excluding pattern,
     * and all text including pattern is consumed.
     * 
     * @param pattern a list of one-char-delimiters, whichever is found first
     * @return position of found delimiter in pattern list
     * @throws ParseException if not found
     */
    public int parseUntil(char... pattern){
    	text(parseHelper,parsePos,parseEnd);
    	int ret = parseHelper.parseUntil(this, pattern);
    	if (ret>=0){
    		parsePos = parseHelper.start;
    		return ret;
    	}
    	start = end = parsePos;
    	tokenError("none of following delimiters found: "+ new String(pattern));
    	return -1; // never reached
    }


    

    /**
     * consume unparsed text until pattern into this. <p>
     * 
     * On success, this contains all unparsed text until but excluding pattern,
     * and all text including pattern is consumed.
     * 
     * @param pattern0 a list of one-char-delimiters, whichever is found first
     * @param pattern1 a list of one-char-delimiters, whichever is found first
     * @see Str#parseUntil(Str, char, char)
     * @return position of found delimiter in pattern list
     * @throws ParseException if not found
     */
    public int parseUntil(char pattern0, char pattern1){
    	text(parseHelper,parsePos,parseEnd);
    	int ret = parseHelper.parseUntil(this, pattern0,pattern1);
    	if (ret>=0){
    		parsePos = parseHelper.start;
    		return ret;
    	}
    	start = end = parsePos;
    	tokenError("none of following delimiters found: "+ pattern0+pattern1);
    	return -1; // never reached
    }



    /**
     * consume unparsed text until pattern into this. <p>
     * 
     * On success, this contains all unparsed text until but excluding pattern,
     * and all text including pattern is consumed.
     * @see Str#parseUntil(Str, char, char, char)
     * @param pattern a list of one-char-delimiters, whichever is found first
     * @return position of found delimiter in pattern list
     * @throws ParseException if not found
     */
    public int parseUntil(char pattern0, char pattern1, char pattern2){
    	text(parseHelper,parsePos,parseEnd);
    	int ret = parseHelper.parseUntil(this, pattern0,pattern1,pattern2);
    	if (ret>=0){
    		parsePos = parseHelper.start;
    		return ret;
    	}
    	start = end = parsePos;
    	tokenError("none of following delimiters found: "+ pattern0+pattern1+pattern2);
    	return -1; // never reached
    }


    /**
     * consume unparsed text until EOL or EOD into this. <p>
     * 
     * contains all unparsed text until but excluding the next found
     * EOL marker. All text including EOL is consumed. EOR is any
     * end-of line marker, i.e. one of CR | LF | CR LF | LF CR.
     * 
     * @param pattern
     * @return patternID, see {@link FindStrs#patternID(long)}
     * @throws ParseException if not found
     */
    public void parseLine() {
    	start = parsePos;
		int pos  = indexOfBaseEOL(parsePos);
		end = pos++;
        if ((pos<this.parseEnd)&&(lexerType(parseBuf[pos])==LexerConst.NEWLINE)&&(parseBuf[pos]!=parseBuf[pos-1])) {
        	++pos;
        }
        parsePos = pos;
	}


    /**
     * returns the internal current parse position (just behind the current token).
     * <p>
     *
     * Can be used to save the position, then "read ahead" some symbols and 
     * restore to the saved position by {@link #setParsePosition}.
     * <p>
     *
     * If you go back frequently, consider keeping the symbols as
     * {@link Token}- setParsePosition causes reparsing which often is 
     * more time consuming than storing the symbols. <p>
     * 
     * 
     */
    public final int getParsePosition() {
        return this.parsePos;
    }


    /**
     * returns the end of the parse puffer.
     * <p>
     *
     * Can be used to extract text from the parse buffer in conjunction with
     * {@link #getParsePosition}and {@link #text}.
     * <p>
     *
     */
    public final int getParseEnd() {
        return this.parseEnd;
    }
    

    /**
     * returns the position of the current token.
     * <p>
     *
     * Can be used to "read ahead" some symbols and return to the current
     * position by {@link #setParsePosition}.
     * <p>
     *
     * If you go back a symbol frequently, consider storing the symbols in a
     * {@link ColToken}- reset causes reparsing which usually is more time
     * consuming than storing the symbols once.
     */
    public final int getTokenStart() {
        return this.tokenStart;
    }


    /**
     * sets the parser to a new buffer position.
     * <p>
     *
     * Method is used by {@link LexerExtension} subclasses and to implement a
     * simple mark/reset mechanism.
     * <p>
     *
     * The internal parse position is set, the token type and the token text
     * remains unchanged. The line counting is not adjusted. The lexer updateState is
     * updated by {@link #next}.
     * <p>
     *
     * If you go back a symbol frequently, consider storing the symbols in a
     * {@link ColToken}- reset causes reparsing which usually is more time
     * consuming than storing the symbols once.
     * <p>
     *
     * <b>Attention </b>: setParsePosition() does not keep book of line numbers.
     * If you use it across line boundaries, the line numbers become invalid.
     *
     * @return parser reset to a previos position, current token is undefined.
     *         Call next() to get a valid token updateState.
     */
    public void setParsePosition(int pos) {
        // TODO line updateState
        this.parsePos = pos;
        if (countEOF<0){
        	// reset EOF counter
        	countEOF = 0;
        }
    }

    
    /**
     * Set how often {@link #next()} will return {@link LexerConst#NONE} 
     * instead of an exception. <p>
     * 
     * The attribute <code>countEOF</code> is initialized to 0 by the 
     * {@link #setSource} and {@link #acquireSource} methods and their 
     * variants if it was negative.
     * Default is 0.
     * 
     * If the attempt is made to parse a token when the end of input data
     * is reached, {@link #next()} returns {@link LexerConst#NONE}, if 
     * the attribute <code>countEOF</code> is currently not negative, and
     * <code>countEOF</code> is decremented. 
     * Otherwise, a {@link ParseException} is thrown. <p>
     * 
     * @param countEOF
     */
    public void setCountEOF(int countEOF){
    	this.countEOF = countEOF;
    }

//    /**
//     * defines a user-parsed token up to the given position in the internal
//     * buffer.
//     * <p>
//     *
//     * sets the type to {@link LexerConst.EXT}, changes the end of the token
//     * and sets the position to continue parsing to end.
//     *
//     * @param end
//     *            index into the internal parse buffer {@link #parseBuf}.
//     */
//    protected void setUserToken(int end) {
//        this.token = LexerConst.EXT;
//        this.end = end;
//        this.parsePos = end;
//    };


    //    /** Returns the next token from this string tokenizer.
    //     *
    //     * @return the next token from this string tokenizer.
    //     * @exception NoSuchElementException if there are no more tokens in this
    //     * tokenizer's string.
    //     *
    //     */
    //    public Str nextToken() {
    //    }
    //
    //
    //    /** Returns the next token in this string tokenizer's string. First,
    //     * the set of characters considered to be delimiters by this
    //     * <tt>StringTokenizer</tt> object is changed to be the characters in
    //     * the string <tt>delim</tt>. Then the next token in the string
    //     * after the current position is returned. The current position is
    //     * advanced beyond the recognized token. The new delimiter set
    //     * remains the default after this call.
    //     *
    //     * @param delim the new delimiters.
    //     * @return the next token, after switching to the new delimiter set.
    //     * @exception NoSuchElementException if there are no more tokens in this
    //     * tokenizer's string.
    //     *
    //     */
    //    public String nextToken(String delim) {
    //    }

    // DEBUG FUNCTIONS

    /**
     * report an error detected at the current token.
     * <p>
     *
     * error message is mandatory (not null!)
     *
     * Generates a detailed error message and throws it as a ParseException.
     * <p>
     *
     * @param message
     *            error message
     * @throws Exception 
     * @throws ParseException
     *             logging the message, the current position in the buffer and
     *             the current line
     */
    public void tokenError(String message)  {
        ParseException.fire(message, this);
    }

    
    /**
     * short for {@link #tokenError}(message+" "+message2)
     * @param message
     * @param message2
     */
    public void tokenError(String message, String message2)  {
        ParseException.fire(message+' '+message2, this);
    }


    public void tokenError(Exception e) {
        ParseException.fire(e, this);
    }


    public String getTokenName(int token) throws Exception {
        if ((token >= 0)) {
            if ((token <= this.tokenNames.size())) {
                return this.tokenNames.getString(token);
            }
            // TODO char anzeigen?
            if (token >= 32) {
                return "'"+symbolToString((char) token)+"'";
            }
        }
        return ColInt.toString(token);
    }


    /**
     * appends a description of the current token updateState to result.
     * <p>
     *
     * Usually called in debug code and if an exception occurs.
     *
     * @param result
     *            a Str where to append the updateState (allocates a new one if null)
     * @return result
     * @throws Exception 
     */
    protected Str appendTokenState(Str result) throws Exception {
        if (result == null) {
            result = new Str();
        }
        int pos = this.start;
        int end = this.end;
        boolean tokenChanged = false;
        if ((this.buffer != this.parseBuf) || (pos < this.linePos)) {
            // very rare case. however we are analyzing an error ...
            pos = this.parsePos;
            end = pos;
            tokenChanged = true;
        }
        // test on EOF error - too large end or start ...
        int linePos = pos - this.linePos;
        result.append("position ").append(this.lineNo).append(':').append(
                linePos).append(" token=").append(
                getTokenName(this.typeLevel)).appendln();
        this.appendCurrentLine(result);
        result.appendln().fill(' ', linePos);
        if (end == pos) {
            result.append('^');
        } else {
            result.fill('+', end - pos);
        }
        if (tokenChanged) {
            result.appendln().append(
                    "actual token text (changed after parsing): <")
                    .append(this).append('>');
        }
        return result.appendln();
    }


    /**
     * append current line - for debugging purposes.
     *
     * @param result here the line is appended. null not allowed.
     * @return result with current line appended
     */
    public Str appendCurrentLine(Str result) {
    	int lineBegin = this.linePos;
        int lineEnd = indexOfBaseEOL(lineBegin);
        if (result == null) {
            result = new Str();
        }
        return result.append(this.parseBuf, lineBegin, lineEnd - lineBegin);
    }


    /**
     * @return the current line number.
     * @see #appendCurrentLine
     */
    public int getLineNo() {
        return this.lineNo;
    }


    /**
     * @return the position of the current line in the internal buffer
     * @see #appendCurrentLine
     */
    public int getLinePos() {
        return this.linePos;
    }


    /**
     * helper for debugging: convert a lexer symbol oder doublesymbol into a
     * String.
     */
    public static String symbolToString(char symbol) {
        if (symbol <= 255) {
            return Character.toString(symbol);
        }
        return Character.toString((char) (symbol >> 8)).concat(
                Character.toString((char) (symbol & 255)));
    }


    // ?? necessary?!
    public void acquireSource(char[] toParse, int start, int end) {
        this.base.acquire(toParse, start, end);
        completeSetSource();
    }


    /**
     * Set the source and acquire the buffer until releaseSource is called.
     * <p>
     *
     * <b>Attention: </b> this method can be used to avoid buffer duplication.
     * Its safe use requires that you call {@link #releaseSource}() at the end
     * of your work (use a try-catch-block!) and that no other object accesses
     * the char just behind the last char of toParse in the internal buffer of
     * toParse until releaseSource() is called.
     *
     * @param toParse
     *            a String containing the text to parse.
     */
    public void acquireSource(String toParse) {
        this.base.assign(toParse);
        completeSetSource();
    }


    public void acquireSource(Str toParse) {
        base = toParse;
        completeSetSource();
    }


    /**
     * Set the source, possibly causing buffer duplication.
     * <p>
     * Use this method, if the source is needed elsewhere.
     * The method {@link #acquireSource(Str)} maybe more 
     * performant, because the buffer is not shared once more.
     * @param toParse
     *            a String containing the text to parse.
     * @see #acquireSource
     *
     */
    public void setSource(Str toParse) {
        this.base.assignAcquireShare(toParse);
        completeSetSource();
    }


    public void setSource(String toParse) {
        this.base.assign(toParse);
        completeSetSource();
    }


    protected void completeSetSource() {
        final Str base = this.base;
        this.parsePos = this.savedBaseStart = base.start;
        base.start = 0; // necessary to avoid offset changes on reallocations!!
        this.parseEnd = base.end;
//        // auf Modifikation des parsebuffers verzichten!!
//        if (base.buffer.length > this.parseEnd) {
//            this.savedEOFmarker = base.buffer[this.parseEnd];
//            base.buffer[this.parseEnd] = 0;
//        }else{
//            this.savedEOFmarker = -1;
//            base.append((char) 0); // will reallocate the buffer ...
//            --base.end; // and "delete" the terminating 0 char
//        }
//        // store 0 char as delimiter in the buffer
        // store parse buffer data in local copies
        this.parseBuf = base.buffer;
        // initialize token to NONE
        this.clear();
        this.typeLevel = LexerConst.NONE;
        // line numbering
        this.lineNo = 1;
        this.linePos = this.parsePos;
        if (this.countEOF < 0) {
            this.countEOF = 0;
        }
    }


    /**
     * Release the current buffer Str containing the complete source. <p>
     * 
     * Returns the lexer source, and resets the internal buffer
     * (empty source). This method can be useful, if you want
     * to further use the source buffer, e. g. in a {@link ColStr#acquire(Str)}
     * in which parsed tokens are to be stored. Because the parsed tokens
     * use references into that Str buffer, assigning them into a ColStr
     * with the same buffer can be done without any content copying. 
     * 
     * @return the internal buffer with the complete Lexer source
     */
    public Str releaseSource() {
        Str base = this.base;
        base.start = this.savedBaseStart;
        this.base = new Str();
        return base;    
    }


    /**
     * Release the current buffer Str, reduced to the unparsed text. <p>
     * 
     * Returns the lexer source, and resets the internal buffer
     * (empty source). This method can be useful, if you want
     * to further use the source buffer, e. g. in a {@link ColStr#acquire(Str)}
     * in which parsed tokens are to be stored. 
     * 
     * @return the internal buffer with the Lexer source
     */
    public Str releaseUnparsedSource() {
        Str base = this.base;
        this.base = new Str();
        base.reduce(this.parsePos);
        return base;    
    }

    public static char stringToSymbol(String symbol) {
        int symLen = 0;
        if ((symbol == null) || ((symLen = symbol.length()) == 0)) {
            return 0;
        }
        if (COMPILE.ASSERT) {
            Assert.interval(symLen,1,2, symbol);
        }
        char sym = symbol.charAt(0);
        if (symLen == 2) {
            sym = (char) ((sym << 8) + symbol.charAt(1));
        }
        return sym;
    }


    public static char doubleSymbol(char first, char second) {
        return (char) (first << 8 + second);
    }


    /**
     * returns 0 or the lexer symbol type for the given Str.
     * <p>
     *
     * If symbol is a defined lexer symbol, its lexer type is returned. That is,
     * symbol consists of 1 or 2 char-s, the first char is of lexerType SYMBOL
     * or DOUBLESYMBOL or COMMENT2, and if symbol has a 2nd char, symbol is a
     * registered doublesymbol.
     *
     * @param symbol
     *            string to test against lexer symbol definitions
     * @return associated lexer symbol or 0 (not recognized as a lexer symbol)
     */
    public char isLexerSymbol(Str symbol) {
        int symLen = 0;
        char id = 0;
        if ((symbol != null) && ((symLen = symbol.length()) > 0)
                && (symLen <= 2)) {
            char ch = symbol.charAt(0);
            int type = this.lexerType[ch];
            if (type >= LexerConst.SYMBOL) {
                // condition uses property of SYMBOL: COMMENT2 and DOUBLESYMBOLs
                // are larger values
                if (symLen == 1) {
                    id = ch;
                } else {
                    if ((type == LexerConst.DOUBLESYMBOL)
                            || (type == LexerConst.COMMENT2)) {
                        id = this.doubleSymbol[ch][symbol.charAt(1)];
                    }
                }
            }
        }
        return id;
    }


    /*
     * (re)defines the token type determined by the 1st char. <p>
     *
     * The lexer {@link #next(}pre-classifies a token by its first char. The
     * types recognized and processed by next() are defined in
     * {@link LexerConst}. @param ch the character which gets a new token type
     * assigned @param type a type from {@link LexerConst}
     */
    public void defineLexerType(char ch, int type) {
        this.lexerType[ch] = (byte) type;
//        if ((((type&127)==LexerConst.SIGNED)||((type&127)==LexerConst.SIGNED))&&(this.doubleSymbol==null)){
//        	this.doubleSymbol = 
//        }
    }

    /*
     * (re)defines the token type in the given char range. <p>
     *
     * Short for <pre>
     * for (char ch=firstChar;ch<=lastChar;++ch){
     *   defineLexerType(ch,type);
     * }
     * </pre>
     */
    public void defineLexerType(char firstChar, char lastChar, int type) {
    	for (char ch=firstChar;ch<=lastChar;++ch){
    		this.lexerType[ch] = (byte) type;
    	}
    }


    /** 
     * if an IDENT is parsed, the IDENT is enlargend for every char allowed in name. <p>
     * 
     * By default, all letters may start and continue an IDENT. You can call this 
     * method to allow symbols like e.g. '.' or '$' to occur in an IDENT.
     * @param ch char for which the lexer state is changed
     * @param isAllowedInName
     */
    public void defineAllowedInName(char ch, boolean isAllowedInName) {
        if (isAllowedInName) {
            this.lexerType[ch] |= (byte) LexerConst.FLAG_IDENT;
        } else {
            this.lexerType[ch] &= (byte) (LexerConst.FLAG_IDENT-1);
        }
    }


    /**
     * defines a new token type for a 2-char-symbol. <p>
     *
     * The first char of the 2-char-symbol will become a symbol, if the second
     * char parsed does not match to any defined doubleSymbol. <p>
     *
     * To "undefine" a doublesymbol, you must call {@link #init}.
     *
     * @param doubleSymbol code for a 2-char-symbol, given by (1st char)*256 +
     * (2nd char). The token type of the 1st char becomes DOUBLESYMBOL. Any
     * token beginning with it will be a SYMBOL or DOUBLESYMBOL. <p>
     *
     * PRECOND: only characters in the range 1..255 can be used to constitute a
     * doubleSymbol. <p>
     *
     * POSTCOND: the symbol type to the first character of a doubleSymbol is not
     * changed - otherwise, the doubleSymbol token definition is destroyed.
     */
    public void defineDoubleSymbol(int doubleSymbol) {
        defineDoubleSymbol((char)(doubleSymbol >> 8), (char)(doubleSymbol & 255));
    }



    /**
     * defines a new token type for a 2-char-symbol. <p>
     *
     * The first char of the 2-char-symbol will become a symbol, if the second
     * char parsed does not match to any defined doubleSymbol. <p>
     *
     * To "undefine" a doublesymbol, you must call {@link #init}.
     *
     * @param doubleSymbol code for a 2-char-symbol, given by (1st char)*256 +
     * (2nd char). The token type of the 1st char becomes DOUBLESYMBOL. Any
     * token beginning with it will be a SYMBOL or DOUBLESYMBOL. <p>
     *
     * PRECOND: only characters in the range 1..255 are allowed to constitute a
     * doubleSymbol. <p>
     *
     * POSTCOND: the symbol type to the first character of a doubleSymbol is not
     * changed - otherwise, the doubleSymbol token definition is destroyed.
     */
    public void defineDoubleSymbol(char ch1, char ch2) {
        if (COMPILE.ASSERT) {
            Assert.interval(ch1, 1, 255, this);
            Assert.interval(ch2, 1, 255, this);
        }
        if (this.lexerType[ch1] != LexerConst.DOUBLESYMBOL) {
            this.lexerType[ch1] = LexerConst.DOUBLESYMBOL;
            // initialize the structure
            if (this.doubleSymbol==null){
            	this.doubleSymbol = new char[LexerConst.SIZE_IN_DOUBLESYMBOL][];
            }
            this.doubleSymbol[ch1] = new char[LexerConst.SIZE_IN_DOUBLESYMBOL];
        }
        this.doubleSymbol[ch1][ch2] = (char)((ch1<<8) + ch2);
    }


    /**
     * defines a new token type für a 2-char-symbol. <p>
     *
     * @param doubleSymbol a String of 2 char-s
     */
    public void defineDoubleSymbol(String doubleSymbol) {
        defineDoubleSymbol(stringToSymbol(doubleSymbol));
    }


    /**
     * set the lexer rules for comment tokens.
     * <p>
     *
     * The lexer
     *
     * @param symbolLineComment
     *            symbol (1 or 2-char) to indicate the begin of a comment which
     *            terminates with the end of the current line. 0: do not define
     *            a line terminated comment
     * @param symbolBeginComment
     *            symbol (1 or 2-char) to indicate the begin of a comment which
     *            terminates with symbolEndComment. 0: do not define this type
     *            of comment
     * @param symbolEndComment
     *            terminating symbol (1 or 2-char) of an comment
     * @param ignoreComment
     *            true: the lexer {@link #next()}will skip any comment token.
     */
    public void defineComment(char symbolLineComment, char symbolBeginComment,
            char symbolEndComment, boolean ignoreComment) {
        this.ignoreComment = ignoreComment;
        if (symbolLineComment > 0) {
            this.symbolLineComment = symbolLineComment;
            if (symbolLineComment > 255) {
                defineDoubleSymbol(symbolLineComment);
                defineLexerType((char) (symbolLineComment >> 8),
                        LexerConst.COMMENT2);
            } else {
                defineLexerType(symbolLineComment, LexerConst.COMMENT);
            }
        }
        if (symbolBeginComment > 0) {
            this.symbolBeginComment = symbolBeginComment;
            if (symbolBeginComment > 255) {
                defineDoubleSymbol(symbolBeginComment);
                defineLexerType((char) (symbolBeginComment >> 8),
                        LexerConst.COMMENT2);
            } else {
                defineLexerType(symbolBeginComment, LexerConst.COMMENT);
            }
            this.symbolEndComment = symbolEndComment;
        }

    }


    public void defineComment(String symbolLineComment,
            String symbolBeginComment, String symbolEndComment,
            boolean ignoreComment) {
        defineComment(stringToSymbol(symbolLineComment),
                stringToSymbol(symbolBeginComment),
                stringToSymbol(symbolEndComment), ignoreComment);
    }


    public void defineIgnoreWhite(boolean ignoreWhite, boolean ignoreNewline,
            boolean ignoreTab) {
        this.ignoreWhite = ignoreWhite;
        this.ignoreNewline = ignoreNewline;
        this.ignoreTab = ignoreTab;
    }


    /**
     * STRING and CHAR Token parametrisation.
     * <p>
     *
     * @param delimiterSTRING
     *            the delimiter to be used for a {@link LexerConst#STRING}token
     *            or 0 (do not define a STRING token delimiter)
     * @param delimiterCHAR
     *            the delimiter to be used for a {@link LexerConst#CHAR}token
     *            or 0 (do not define a STRING token delimiter)
     * @param newlineInToken
     *            determines whether newline sequences are permitted. Valid
     *            values are <BR>
     *            0: default, STRING and CHAR tokens must not contain newline
     *            sequences, <BR>
     *            {@link LexerConst#STRING}: STRING may, CHAR may not contain
     *            newline sequences, <BR>
     *            {@link LexerConst#CHAR}: both STRING and CHAR tokens may
     *            contain newline sequences
     *
     * @param symbolStringEscape
     *            escape char if found within a string, it is removed and the
     *            next char is regarded a part of the string or char sequence.
     *            Used for the delimiters and maybe control chars.
     *            Default is 0: in this case the string delimiter has to be 
     *            doubled inside a CHAR or STRING token (e. g. used in the CSV 
     *            format or in the PASCAL language).
     *            <p>
     *
     * <p>
     *
     * A typical escape char is '\' in the C and derived languages like java. If
     * the lexer encounters the escape symbol inside a CHAR or STRING token, the
     * escape char and the following character are not immideately replaced,
     * because the replacement is not canocical enough. To do the replacement,
     * a LexerExtension is needed (attach it by  {@link #defineExtension(LexerExtension)} 
     * and call {@link #unEscape()} on any STRING or CHAR token by a value which is
     * returned by the current registered {@link LexerExtension}. In other
     * words: a LexerExtension must be registered if a symbolStringEscape char
     * different from 0 is defined.
     */
    public void defineString(char delimiterSTRING, char delimiterCHAR,
            char stringEscape, int newlineInToken) {
        if (delimiterSTRING != 0) {
            defineLexerType(delimiterSTRING, LexerConst.STRING);
        }
        if (delimiterCHAR != 0) {
            defineLexerType(delimiterCHAR, LexerConst.CHAR);
        }
        this.newlineInToken = newlineInToken;
        if (COMPILE.ASSERT) {
            if ((newlineInToken != 0) && (newlineInToken != LexerConst.CHAR)
                    && (newlineInToken != LexerConst.STRING)) {
                Assert.fire(AssertConst.IllegalArgumentException,
                        "param newlineInToken has unsupported value",
                        newlineInToken, this);
            }
        }
        if ((stringEscape != 0) && (this.extension == null)) {
            this
                    .tokenError("string escape given, but no lexer extension registered");
        }
        this.symbolStringEscape = stringEscape;

    }


    /**
     * Defines the symbols identifying a FRACTION number.
     * <p>
     *
     * @param symbolFraction
     *            the char which separates the integral part of the fraction
     *            from the fraction part. Standard: '.'
     *            0: no processing of fraction numbers by the lexer
     *
     * @param symbolExponent
     *            the char which separates the fraction or integral part from
     *            the exponent part. If you supply a lowercase char, the
     *            uppercase of it is also recognized. Standard: 'e'.
     *            <p>
     *
     */
    public void defineSymbolFraction(char symbolFraction, char symbolExponent) {
        this.symbolFraction = symbolFraction;
        this.symbolExponent = symbolExponent;
        this.symbolUppercaseExponent = Str.toUpperCase(symbolExponent);
    }


    public void defineExtension(LexerExtension userExtension) {
        this.extension = userExtension;
        userExtension.register(this);
    }


    /**
     * resets the lexer syntax.
     * <p>
     *
     * All lexer token type definitions are cleared, the current updateState is: all
     * char-s in the alfabet are recognized as "end-of-input".
     *
     * @param alphabetSize
     *            size of the alphabet which is permitted. All char-s with a
     *            code value greater or equal alphabetSize share the same lexer
     *            token type (the default token type).
     *            So, choose alphabetSize sufficient large such that almost all
     *            characters in the source are in the alphabet. In many cases, a
     *            value of 128 (ASCII) or 256 (BYTE char-s) is sufficient.
     *            <p>
     *
     * @param defaultTokenType
     *            the lexer token type assigned to any character which is not in
     *            the alphabet. Recommendation: {@link LexerConst#ILLEGAL}
     */
    public void init(int alphabetSize, int defaultTokenType) {
        this.defaultTokenType = (byte) defaultTokenType;
        this.defaultTokenTypeMasked = (byte) (defaultTokenType & LexerConst.TOKENTYPEMASK);
        this.lexerType = new byte[alphabetSize];
        this.lexerTypeSize = alphabetSize;
        this.doubleSymbol = null;
    }


    /*
     * short for init(alphabetSize, {@link LexerConst#ILLEGAL}).
     */
    public void init(int alphabetSize) {
        init(alphabetSize, LexerConst.ILLEGAL);
    }

    
    /**
     * sets all codes below 32 to WHITE except the codes 0 (NONE), 10 and 13 (NEWLINE).
     */
    public void setupControlCharDefault() {
        defineLexerType((char)0,' ', LexerConst.NONE);
        // use platform-specific 1st characters of EOL sequence for NEWLINE.
       	defineLexerType((char)1, ' ',LexerConst.WHITE);
       	defineLexerType((char)10, LexerConst.NEWLINE);
       	defineLexerType((char)13, LexerConst.NEWLINE);
    }



    /**
     * Defines a minimalistic lexer behavior.
     * <p>
     *
     * All current definitions are discarded, the following type definitions are
     * implemented (see {@link LexerConst}for the token type definitions):
     * <p>
     * <ol>
     * <li>{@link LexerConst#WHITE} are all chars 0..32 except NEWLINE</li>
     * <li>{@link LexerConst#NEWLINE} are char(10) and char(13), to be compatible
     * with Unix, Windows and Macintosh newline sequences.</li>
     * <li>{@link LexerConst#IDENT} is the type of all java letters in the alfabet (
     * {@link Character.isLetter}() returns true).
     * An IDENT symbol may contain all characters of lexer type IDENT and DIGIT.</li>
     * <li>{@link LexerConst#CHAR} is not defined</li>
     * <li>{@link LexerConst#STRING} is not defined</li>
     * <li>{@link LexerConst#TAB} is not defined</li>
     * <li>{@link LexerConst#COMMENT} is not defined</li>
     * <li>{@link LexerConst#LCOMMENT} is not defined</li>
     * <li>{@link LexerConst#NUMBER} is a sequence of DIGITs (signs are treated as SYMBOL).</li>
     * <li>{@link LexerConst#FRACTION} is not defined</li>
     * <li>{@link LexerConst#LCOMMENT} is not defined</li>
     * <li>{@link LexerConst#ILLEGAL} maybe assigned to any character not within the alfabet.
     * </li>
     * <li>{@link LexerConst#DOUBLESYMBOL}is not defined</li>
     * <li>{@link LexerConst#SYMBOL}s are all other char-s.</li>
     * <li>Internal lexer type {@link LexerConst#SIGNED} is not defined</li>
     * <li>Internal lexer type {@link LexerConst#DIGIT} maps all java digits in the alfabet(
     * {@link Character.isDigit}(ch) returns true), usually '0' .. '9',
     * but also unicode digits with higher encodings.
     * </li>
     * </ol>
     * <p>
     *
     * WHITE, NEWLINE and TAB are ignored, i. e. next() skips these tokens -
     * defineIgnoreWhite(true,true,true) is called.
     * <p>
     *
     *
     */
    public void setupBasic() {
        this.doubleSymbol = null;
        char c;
        setupControlCharDefault();
        for (c = (char) this.lexerTypeSize; (--c) > ' ';) {
            defineLexerType(c, (Character.isLetter(c)) ? LexerConst.IDENT
                    | LexerConst.FLAG_IDENT
                    : ((Character.isDigit(c)) ? LexerConst.INTEGRAL
                            | LexerConst.FLAG_IDENT
                            : (Character.isSpaceChar(c) ? LexerConst.WHITE
                                    : LexerConst.SYMBOL)));
        }
        // define what is allowed in a name
        defineIgnoreWhite(true, true, true);
		defineSymbolFraction((char)0,(char)0);
    }

    /**
     * Defines a standard lexer behavior.
     * <p>
     *
     * All current definitions are discarded, the following type definitions are
     * implemented (see {@link LexerConst}for the token type definitions):
     * <p>
     * <ol>
     * <li>{@link LexerConst#NONE}is char 0.</li>
     * <li>{@link LexerConst#WHITE}are all chars 1..32 except NEWLINE</li>
     * <li>{@link LexerConst#NEWLINE}is the first char of the
     * platform-specific end-of-line-sequence. char(10) on UNIX, char(13) on
     * Windows and Macintosh.</li>
     * <li>{@link LexerConst#DIGIT}s are all java digits (
     * {@link Character.isDigit}(ch) returns true), usually '0' .. '9'.</li>
     * <li>{@link LexerConst#IDENT}is the type of all java letters (
     * {@link Character.isLetter}() returns true), usually 'a'..'z',
     * 'A'..'Z','_','$'. An IDENT may contain all characters of lexer type IDENT
     * and DIGIT.</li>
     * <li>{@link LexerConst#CHAR}is delimited by quote('), no escape
     * character defined</li>
     * <li>{@link LexerConst#STRING}is delimited by doublequote ("), no escape
     * character defined</li>
     * <li>{@link LexerConst#TAB}is the ASCII TAB char (code 9)</li>
     * <li>{@link LexerConst#COMMENT}is not defined</li>
     * <li>{@link LexerConst#LCOMMENT}is not defined</li>
     * <li>{@link LexerConst#ILLEGAL}is any character not within the alfabet.
     * </li>
     * <li>{@link LexerConst#DOUBLESYMBOL}is not defined</li>
     * <li>{@link LexerConst#SYMBOL}s are all other char-s.</li>
     * </ol>
     * <p>
     *
     * An IDENT token can continue with a digit or a char of type IDENT.
     * <p>
     *
     * WHITE, NEWLINE and TAB are ignored, i. e. next() skips these tokens -
     * defineIgnoreWhite(true,true,true) is called.
     * <p>
     *
     *
     */
    public void setupStandard() {
    	setupBasic();
        lexerType['_'] = (byte) (LexerConst.IDENT | LexerConst.FLAG_IDENT); // TODO
                                                                            // necessary?
        lexerType['$'] = (byte) (LexerConst.IDENT | LexerConst.FLAG_IDENT); // TODO
                                                                            // necessary?
        defineLexerType((char)9, LexerConst.TAB);
        defineLexerType('+', LexerConst.SIGNED);
        // internal type - check for a number (digit follows) or symbol
        // (otherwise)
        defineLexerType('-', LexerConst.SIGNED); // ditto
        // define what is allowed in a name
        defineSymbolFraction('.', 'e');
        // default string and char types
        defineString('"','\'',(char)0,0);
    }

    
    /**
     * Defines a lexer behavior suitable for parsing of java-like programming languages.
     * <p>
     *
     * All current definitions are discarded, the following type definitions are
     * implemented (see {@link LexerConst} for the token type definitions):
     * <p>
     * <ol>
     * <li>{@link LexerConst#WHITE}are all chars 0..32 except NEWLINE and TAB</li>
     * <li>{@link LexerConst#NEWLINE}is the first char of the
     * platform-specific end-of-line-sequence. char(10) on UNIX, char(13) on
     * Windows and Macintosh.</li>
     * <li>{@link LexerConst#DIGIT}s are all java digits (
     * {@link Character.isDigit}(ch) returns true), usually '0' .. '9',
     * but additionally UNICODE digits if the alfabet has more than 256
     * chacacters.</li>
     * <li>{@link LexerConst#IDENT}is the type of all java letters (
     * {@link Character.isLetter}() returns true), usually 'a'..'z',
     * 'A'..'Z','_','$'. An IDENT token may contain all characters of lexer type IDENT
     * and DIGIT.</li>
     * <li>{@link LexerConst#CHAR}is delimited by quote('), '\' is the escape character,
     * and a lexer extension handling the escape according to the java language
     * specification is attached to the lexer. </li>
     * <li>{@link LexerConst#STRING}is delimited by doublequote ("), '\' is the escape character,
     * and a lexer extension handling the escape according to the java language
     * specification is attached to the lexer.</li>
     * <li>{@link LexerConst#TAB}is the ASCII TAB char (code 9)</li>
     * <li>{@link LexerConst#COMMENT}is a java or C multiline comment starting with "/*".
     * Comments are reported as tokens.</li>
     * <li>{@link LexerConst#LCOMMENT}is a java or C single line comment starting with "//".
     * LCOMMENTS are reported as tokens.</li>
     * <li>{@link LexerConst#ILLEGAL}is any character not within the alfabet.
     * </li>
     * <li>{@link LexerConst#DOUBLESYMBOL}is defined according to java syntax, it covers
     * all java 2-char-operators, i.e. ++ -- += -= *= /= &= |= ^= %= || && 
     * == != &lt;= >= >> &lt;&lt;. It does NOT cover 3-char-operators like >>> <<= etc.,
     * those would need individual treatment in the parser</li>
     * <li>{@link LexerConst#SYMBOL}s are all other char-s.</li>
     * </ol>
     * <p>
     *
     * WHITE, NEWLINE and TAB are ignored, i. e. next() skips these tokens -
     * defineIgnoreWhite(true,true,true) is called.
     * <p>
     *
     * 
     */
    public void setupJava() {
    	setupStandard();
        // standard double symbols
        defineComment((char)(('/'<<8)+'/'),(char)(('/'<<8)+'*'),(char)(('*'<<8)+'/'),false);
        defineDoubleSymbol('+','+');
        defineDoubleSymbol('-','-');
        defineDoubleSymbol('+','=');
        defineDoubleSymbol('-','=');
        defineDoubleSymbol('*','=');
        defineDoubleSymbol('/','=');
        defineDoubleSymbol('&','=');
        defineDoubleSymbol('|','=');
        defineDoubleSymbol('^','=');
        defineDoubleSymbol('%','=');
        defineDoubleSymbol('=','=');
        defineDoubleSymbol('<','=');
        defineDoubleSymbol('>','=');
        defineDoubleSymbol('!','=');
        defineDoubleSymbol('&','&');
        defineDoubleSymbol('|','|');
        defineString('"', '\'', '\\', 0);
        defineExtension(new LexerKeyword()); // to process escapes
    }

    /**
     * stores some portion of the parse buffer into a Str.
     * <p>
     *
     * @param result
     *            the Str to store the text portion
     * @param start
     *            index in parse buffer. To get it, use {@link getParsePosition()}to get the
     *            index of the first nonconsumed char (directly behind the last char of the current token)
     *            or {@link #getParseEnd()} to get the end of the source text
     *            <p>
     * @param end first index NOT to include in result
     *
     * @param result
     *            the Str to store the text portion. Must be a valid Str
     * @return result, with text portion copied into it as a shared content
     */
    public Str text(Str result, int start, int end) {
        return result.assign(start, end, this.base);
        //        result.buffer = this.parseBuf;
        //        result.start = start;
        //        result.end = end;
        //        result.shared = this.parseBuf.length;
    }

	
	/**
	 * Access remaining (unparsed) text, without consuming it. <p>
	 * 
	 * This allows to use the rich Str API on the lexer text stream,
	 * for example any parse* method like {@link #parseUntil} and its variants. <p> 
	 * 
	 * Attention: Str methods do not update parsePos, to continue with any text
	 * consuming Lexer method like e.g. {@link #next()}, you must 
	 * call {@link #setParsePosition(int)} with any suitable buffer position,  
	 * e.g. {@link #getEnd()} assuming this lexer is still referencing 
	 * a portion of its base buffer.
	 * 
	 * @return this, contents is all unparsed text of this lexer
	 */
	public void accessRest() {
		assign(parsePos,parseEnd,base);
	}
	

    /**
     * Returns stores some portion of the parse buffer as a String.
     * <p>
     * 
     * Prefer {@link #text(Str, int, int)} if further processing is
     * required.
     *
     * @param start
     *            first index in parse buffer. To get it, use {@link #getStart()}to
     *            get the index of the first char or {@link getEnd()}to get the
     *            index directly behind the last char of the current token.
     *            <p>
     *
     * @param end
     *            end index index in parse buffer.
     *
     *
     * @return String with the marked text.
     *
     * 
     */
    public String text(int start, int end){
    	return this.base.substring(start,end);
    }


    /**
     * Tokenizes the whole lexer source and puts all tokens into a ColToken list. <p>
     *
     * @param tokenSizeGuess (slightly underestimated) guess of the average token size.
     * Is used to initially size the token list. A value which is too large will cause
     * (expensive) resize operations of the token list.
     * @return Col with all parsed tokens. Lexer source is consumed by ColToken
     * and left undefined
     * @throws Exception 
     */
    public ColToken tokenList(int tokenSizeGuess) throws Exception {
        int tok = 0; // next token index
        ColToken list = new ColToken(this.base);
        list.expectedAppends(this.base.length() / tokenSizeGuess); // should be large enough
        int listSize = list.size();
        while (next() != LexerConst.NONE) {
            if (tok >= listSize) {
                list.append(listSize);
                listSize += listSize;
            }
            if (this.buffer != this.parseBuf){
            	// append to base
            	int start = base.end;
            	base.append(this);
            	this.end = base.end;
            	this.start = start;
            	this.buffer = this.parseBuf = this.base.buffer;
            }
            list.setSlice(tok++, this.start, this.end-this.start, typeLevel);
        }
        list.setSize(tok);
        this.base = new Str();
        return list;
    }


    //    /**
    //     * process escape sequences, remove delimiters. <p>
    //     *
    //     * In this default implementation, the delimiters of a
    //     * {@link ColToken.STRING} and {@link ColToken.CHAR} token
    //     * are removed, and standard java escapes in sourcecode
    //     * strings are processed, i.e. the backslash '\' is the escape character.
    //     * Overload it if you need different escape processing.
    //     * @param result the Str to store the result into. or 0 (a new one will be
    // allocated)
    //     * @return result
    //     */
    //    public Str escape(Str result){
    //        if (result==null){
    //            result=new Str();
    //        }
    //        result.assign(this);
    //        if ((this.type==LexerConst.CHAR)||(this.type==LexerConst.STRING)){
    //            ++result.start;
    //            --result.end;
    //        }
    //        // TODO escape processing
    //        return result;
    //    }
    //

    /**
     * Creates a new, unitinialized instance.
     * <p>
     *
     * Remember to call {@link #init}before any usage!
     */
    public Lexer() {
        this.base = new Str();
        this.tokenNames = Lexer.__tokenTypeNames;
        this.parseHelper = new Str();
    }


    // parse the token type names
    static {
        //        String[] tokenNames = new String[LexerConst.USER + 1];
        //        java.util.StringTokenizer st = new
        // java.util.StringTokenizer(LexerConst.TYPE_NAMES, ",", false);
        //        int i = 0;
        //        while (st.hasMoreTokens()) {
        //            tokenNames[i++] = st.nextToken();
        //        }
		Lexer.__tokenTypeNames = Col.createList(LexerConst.TYPE_NAMES, ',', false);
    }

}
=#