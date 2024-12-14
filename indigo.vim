" Vim syntax file, based on the Haskell syntax file

" Quit when a syntax file was already loaded.
if exists('b:current_syntax') | finish|  endif

" (Qualified) identifiers (no default highlighting)
syn match ConId "\(\<[A-Z][a-zA-Z0-9_']*\.\)*\<[A-Z][a-zA-Z0-9_']*\>" contains=@NoSpell
syn match VarId "\(\<[A-Z][a-zA-Z0-9_']*\.\)*\<[a-z][a-zA-Z0-9_']*\>" contains=@NoSpell

" Infix operators--most punctuation characters and any (qualified) identifier
" enclosed in `backquotes`. An operator starting with : is a constructor,
" others are variables (e.g. functions).
syn match inVarSym "\(\<[A-Z][a-zA-Z0-9_']*\.\)\=[-!#$%&\*\+/<=>\?@\\^|~.][-!#$%&\*\+/<=>\?@\\^|~:.]*"
syn match inConSym "\(\<[A-Z][a-zA-Z0-9_']*\.\)\=:[-!#$%&\*\+./<=>\?@\\^|~:]*"
syn match inVarSym "`\(\<[A-Z][a-zA-Z0-9_']*\.\)\=[a-z][a-zA-Z0-9_']*`"
syn match inConSym "`\(\<[A-Z][a-zA-Z0-9_']*\.\)\=[A-Z][a-zA-Z0-9_']*`"

" Reserved symbols--cannot be overloaded.
syn match inDelimiter  "(\|)\|\[\|\]\|,\|;\|{\|}"

" Strings and constants
syn match   inSpecialChar	contained "\\\([0-9]\+\|o[0-7]\+\|x[0-9a-fA-F]\+\|[\"\\'&\\abfnrtv]\|^[A-Z^_\[\\\]]\)"
syn match   inSpecialChar	contained "\\\(NUL\|SOH\|STX\|ETX\|EOT\|ENQ\|ACK\|BEL\|BS\|HT\|LF\|VT\|FF\|CR\|SO\|SI\|DLE\|DC1\|DC2\|DC3\|DC4\|NAK\|SYN\|ETB\|CAN\|EM\|SUB\|ESC\|FS\|GS\|RS\|US\|SP\|DEL\)"
syn match   inSpecialCharError	contained "\\&\|'''\+"
syn region  inString		start=+"+  skip=+\\\\\|\\"+  end=+"+  contains=inSpecialChar,@NoSpell
syn match   inCharacter		"[^a-zA-Z0-9_']'\([^\\]\|\\[^']\+\|\\'\)'"lc=1 contains=inSpecialChar,inSpecialCharError
syn match   inCharacter		"^'\([^\\]\|\\[^']\+\|\\'\)'" contains=inSpecialChar,inSpecialCharError
syn match   inNumber		"\v<[0-9]%(_*[0-9])*>|<0[xX]_*[0-9a-fA-F]%(_*[0-9a-fA-F])*>|<0[oO]_*%(_*[0-7])*>|<0[bB]_*[01]%(_*[01])*>"
syn match   inFloat		"\v<[0-9]%(_*[0-9])*\.[0-9]%(_*[0-9])*%(_*[eE][-+]?[0-9]%(_*[0-9])*)?>|<[0-9]%(_*[0-9])*_*[eE][-+]?[0-9]%(_*[0-9])*>|<0[xX]_*[0-9a-fA-F]%(_*[0-9a-fA-F])*\.[0-9a-fA-F]%(_*[0-9a-fA-F])*%(_*[pP][-+]?[0-9]%(_*[0-9])*)?>|<0[xX]_*[0-9a-fA-F]%(_*[0-9a-fA-F])*_*[pP][-+]?[0-9]%(_*[0-9])*>"

" Keyword definitions.
syn keyword inModule		module
syn match   inImportGroup	"\<import\>.*" contains=inImport,inImportModuleName,inImportMod,inLineComment,inBlockComment,inImportList,@NoSpell nextgroup=inImport
syn keyword inImport import contained nextgroup=inImportModuleName
syn match   inImportModuleName '\<[A-Z][A-Za-z.]*' contained
syn region  inImportList start='(' skip='([^)]\{-})' end=')' keepend contained contains=ConId,VarId,inDelimiter,inBlockComment,inTypedef,@NoSpell

syn keyword inImportMod contained as qualified hiding from
syn keyword inInfix as satisfies is
syn keyword inStructure struct trait impl for let external
syn keyword inTypedef type
syn keyword inNewtypedef newtype
syn keyword inTypeFam family
syn keyword inStatement do end
syn keyword inConditional if then else

" Not real keywords, but close.
if exists("in_highlight_boolean")
  " Boolean constants from the standard prelude.
  syn keyword inBoolean True False
endif
if exists("in_highlight_types")
  " Primitive types from the standard prelude and libraries.
  syn keyword inType Int Integer Char Bool Float Double IO Void Addr Array String
endif
if exists("in_highlight_more_types")
  " Types from the standard prelude libraries.
  syn keyword inType Maybe Either Ratio Complex Ordering IOError IOResult ExitCode
  syn keyword inMaybe Nothing
  syn keyword inExitCode ExitSuccess
  syn keyword inOrdering GT LT EQ
endif
if exists("in_highlight_debug")
  " Debugging functions from the standard prelude.
  syn keyword inDebug undefined error trace
endif


" Comments
syn match   inLineComment      "##*\([^-!#$%&\*\+./<=>\?@\\^|~].*\)\?$" contains=@Spell
" syn match     inLineComment      "#.*$" contains=@Spell
syn region  inBlockComment     start="/\*"  end="\*/" contains=inBlockComment,@Spell
" syn region  inPragma	       start="{-#" end="#-}"

syn keyword inTodo	        contained FIXME TODO XXX NOTE

" Define the default highlighting.
" Only when an item doesn't have highlighting yet

hi def link inModule			  inStructure
hi def link inImport			  Include
hi def link inImportMod			  inImport
hi def link inInfix			  PreProc
hi def link inStructure			  Structure
hi def link inStatement			  Statement
hi def link inConditional		  Conditional
hi def link inSpecialChar		  SpecialChar
hi def link inTypedef			  Typedef
hi def link inNewtypedef		  Typedef
hi def link inVarSym			  inOperator
hi def link inConSym			  inOperator
hi def link inOperator			  Operator
hi def link inTypeFam			  Structure
if exists("in_highlight_delimiters")
" Some people find this highlighting distracting.
hi def link inDelimiter			  Delimiter
endif
hi def link inSpecialCharError		  Error
hi def link inString			  String
hi def link inCharacter			  Character
hi def link inNumber			  Number
hi def link inFloat			  Float
hi def link inConditional			  Conditional
hi def link inLiterateComment		  inComment
hi def link inBlockComment		  inComment
hi def link inLineComment			  inComment
hi def link inComment			  Comment
hi def link inPragma			  SpecialComment
hi def link inBoolean			  Boolean
hi def link inType			  Type
hi def link inMaybe			  inEnumConst
hi def link inOrdering			  inEnumConst
hi def link inEnumConst			  Constant
hi def link inDebug			  Debug

let b:current_syntax = "indigo"
