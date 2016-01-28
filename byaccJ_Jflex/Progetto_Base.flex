
%%

%byaccj

%{
  private Parser yyparser;

  public Yylex(java.io.Reader r, Parser yyparser) {
    this(r);
    this.yyparser = yyparser;
  } 
%}

%x IN_TAG IN_ATTRIBUTES

NL  = \n|\r|\r\n

INTRO = <\?xml(.*)\?>
INTRO_II = <\!DOCTYPE[^>]*>
COMMENT = <\!\-\-(!((.|\n|\r|\r\n)*\-\->(.|\n|\r|\r\n)*))\-\->

OPEN_TAG = <
CLOSE_TAG = >
SLASH = \/
CDATA = \"[^\"]*\"
PCDATA = [^<>\"\n\t]*
EQ = =

EDITION = edition
ID = id
TITLE = title
CAPTION = caption
PATH = path

BOOK = book
DEDICATION = dedication
PREFACE = preface
AUTHORNOTES = authornotes
NOTE = note
PART = part
TOC = toc
LOF = lof
LOT = lot
ITEM = item
CHAPTER = chapter
SECTION = section
FIGURE = figure
TABLE = table
ROW = row
CELL = cell



%%



<YYINITIAL,IN_TAG,IN_ATTRIBUTES>{NL}	{ }

<YYINITIAL,IN_TAG,IN_ATTRIBUTES> [ \t]+ { }

<YYINITIAL,IN_TAG,IN_ATTRIBUTES> {COMMENT}  { }

{INTRO} {}

{INTRO_II} {}

{OPEN_TAG} { yybegin(IN_TAG);
				return Parser.OPEN_TAG;
			}

{PCDATA} { yyparser.yylval = new ParserVal(yytext());
			return Parser.PCDATA;
		}

<IN_TAG,IN_ATTRIBUTES> {SLASH} { 
				Parser.subTabs();
				return Parser.SLASH;
			}
				
<IN_TAG>{BOOK} { yybegin(IN_ATTRIBUTES);
				Parser.addTabs();
				return Parser.BOOK;
			}
			
<IN_TAG>{DEDICATION} { yybegin(IN_ATTRIBUTES);
				Parser.addTabs();
				return Parser.DEDICATION;
			}

<IN_TAG>{PREFACE} { yybegin(IN_ATTRIBUTES);
				Parser.addTabs();
				return Parser.PREFACE;
			}
			
<IN_TAG>{AUTHORNOTES} { yybegin(IN_ATTRIBUTES);
				Parser.addTabs();
				return Parser.AUTHORNOTES;
			}
			
<IN_TAG>{PART} { yybegin(IN_ATTRIBUTES);
				Parser.addTabs();
				return Parser.PART;
			}
			
<IN_TAG>{NOTE} { yybegin(IN_ATTRIBUTES);
				Parser.addTabs();
				return Parser.NOTE;
			}
			
<IN_TAG>{TOC} { yybegin(IN_ATTRIBUTES);
				Parser.addTabs();
				return Parser.TOC;
			}
			
<IN_TAG>{LOF} { yybegin(IN_ATTRIBUTES);
				Parser.addTabs();
				return Parser.LOF;
			}
			
<IN_TAG>{LOT} { yybegin(IN_ATTRIBUTES);
				Parser.addTabs();
				return Parser.LOT;
			}
			
<IN_TAG>{ITEM} { yybegin(IN_ATTRIBUTES);
				Parser.addTabs();
				return Parser.ITEM;
			}
			
<IN_TAG>{CHAPTER} { yybegin(IN_ATTRIBUTES);
				Parser.addTabs();
				return Parser.CHAPTER;
			}
			
<IN_TAG>{SECTION} { yybegin(IN_ATTRIBUTES);
				Parser.addTabs();
				return Parser.SECTION;
			}
			
<IN_TAG>{FIGURE} { yybegin(IN_ATTRIBUTES);
				Parser.addTabs();
				Parser.addTabs();
				return Parser.FIGURE;
			}
			
<IN_TAG>{TABLE} { yybegin(IN_ATTRIBUTES);
				Parser.addTabs();
				return Parser.TABLE;
			}
			
<IN_TAG>{ROW} { yybegin(IN_ATTRIBUTES);
				Parser.addTabs();
				return Parser.ROW;
			}
			
<IN_TAG>{CELL} { yybegin(IN_ATTRIBUTES);
				Parser.addTabs();
				return Parser.CELL;
			}
			
<IN_ATTRIBUTES>{EQ} { return Parser.EQ; }

<IN_ATTRIBUTES>{EDITION} { return Parser.EDITION;}
			
<IN_ATTRIBUTES>{ID} { return Parser.ID;}
			
<IN_ATTRIBUTES>{TITLE} { return Parser.TITLE;}

<IN_ATTRIBUTES>{CAPTION} { return Parser.CAPTION;}

<IN_ATTRIBUTES>{PATH} { return Parser.PATH;}
			  
<IN_ATTRIBUTES>{CDATA} { yyparser.yylval = new ParserVal(yytext());
				return Parser.CDATA;
			}
			  
<IN_ATTRIBUTES>{CLOSE_TAG} { yybegin(YYINITIAL);
				return Parser.CLOSE_TAG;
			}
