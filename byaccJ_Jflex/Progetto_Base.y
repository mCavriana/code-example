%{
  import java.io.*;
%}
	  
%token INTRO COMMENT EDITION ID TITLE CAPTION PATH BOOK DEDICATION PREFACE AUTHORNOTES 
			NOTE PART TOC LOF LOT ITEM CHAPTER SECTION FIGURE TABLE ROW CELL
%token OPEN_TAG CLOSE_TAG SLASH EQ
%token<sval> CDATA PCDATA
%type<sval> pcdatas book dedication authornotes part parts part_contents opt_part_contents 
			opt_part_contents2 preface book_contents last_book_contents notes note toc lot lof 
			chapters chapter section sections s_contents s_content items item figure table
			cells cell rows row
     
%%

xml : book { System.out.print("\nConversione completata, nome file: " + outputFileName + "\n");
				try {output.append($1);}
				catch(Exception writeException){}; }
      
book : OPEN_TAG BOOK EDITION EQ CDATA CLOSE_TAG book_contents
		{ 
			if ($5.equals("")) {
				$$ = "{\n" + writeTabs(tabulations, 1) + "\"tag\": \"book\",\n" + writeTabs(tabulations, 1) + "\"content\": [\n" + $7 + "\n" + writeTabs(tabulations, 1) + "]\n}" ;
				} 
			else {
				$$ = "{\n" + writeTabs(tabulations, 1) + "\"tag\": \"book\",\n" + writeTabs(tabulations, 1) + "@edition\": " + $5 + ",\n" + writeTabs(tabulations, 1) + "\"content\": [\n" + $7 + "\n" + writeTabs(tabulations, 1) + "]\n}"; 
				}
		}
	 | OPEN_TAG BOOK CLOSE_TAG book_contents 
		{ 	
			$$ = "{\n" + writeTabs(tabulations, 1) + "\"tag\": \"book\",\n" + writeTabs(tabulations, 1) + "\"content\": [\n" + $4 + "\n" + writeTabs(tabulations, 1) + "]\n}";
		}
     ;
	 
book_contents : dedication preface parts last_book_contents 
				{ if($4.equals("")) 
					{
						$$ = $1 + ",\n" + $2 + ",\n" + $3 ; 
					} else {
						$$ = $1 + ",\n" + $2 + ",\n" + $3 + ",\n" + $4; 
					}
				}
			  | preface parts last_book_contents 
				{ if($3.equals("")) 
					{			
						$$ = $1 + ",\n" + $2 ;
					} else {
						$$ = $1 + ",\n" + $2 + ",\n" + $3; 
					}
				}
			  ;
			  
last_book_contents : authornotes OPEN_TAG SLASH BOOK CLOSE_TAG { $$ = $1; }
				   | OPEN_TAG SLASH BOOK CLOSE_TAG  {$$ = "";}
			
dedication : OPEN_TAG DEDICATION CLOSE_TAG pcdatas OPEN_TAG SLASH DEDICATION CLOSE_TAG 
			 { 
			 if ($4.equals(""))
				{
					$$ = writeTabs(tabulations) + "{\n" + writeTabs(tabulations, 1) + "\"tag\": \"dedication\",\n" + writeTabs(tabulations) + "}";
				}
				else {
					$$ = writeTabs(tabulations) + "{\n" + writeTabs(tabulations, 1) + "\"tag\": \"dedication\",\n" + writeTabs(tabulations, 1) + "\"content\": [\n" + writeTabs(tabulations, 2) + "\"" + $4 + "\"\n" + writeTabs(tabulations, 1) + "]\n" + writeTabs(tabulations) + "}"; 
				}
			 }
		   ;

authornotes : OPEN_TAG AUTHORNOTES CLOSE_TAG notes OPEN_TAG SLASH AUTHORNOTES CLOSE_TAG 
			{	
				$$ = writeTabs(tabulations) + "{\n" + writeTabs(tabulations, 1) + "\"tag\": \"authornotes\",\n" + writeTabs(tabulations, 1) + "\"content\": [\n" + $4 + "\n" + writeTabs(tabulations, 1) + "]\n" + writeTabs(tabulations) + "}"; 
			}	
			;
			
notes : note { $$ = $1; }
	  | notes note { $$ = $1 + ",\n" + $2; }
      ;
	
note : OPEN_TAG NOTE CLOSE_TAG pcdatas OPEN_TAG SLASH NOTE CLOSE_TAG 
		{ 	
			if ($4.equals("")) {
				$$ = writeTabs(tabulations) + "{\n" + writeTabs(tabulations, 1) + "\"tag\": \"note\"\n" + writeTabs(tabulations) + "}";
			}
			else {
				$$ = writeTabs(tabulations) + "{\n" + writeTabs(tabulations, 1) + "\"tag\": \"note\",\n" + writeTabs(tabulations, 1) + "\"content\": [\n" + writeTabs(tabulations, 2) + "\"" + $4 + "\"\n" + writeTabs(tabulations, 1) + "]\n" + writeTabs(tabulations) + "}"; 
			}
		}
		
	 ;

preface : OPEN_TAG PREFACE CLOSE_TAG pcdatas OPEN_TAG SLASH PREFACE CLOSE_TAG 
			{ 	
				if ($4.equals("")) {
					$$ = writeTabs(tabulations) + "{\n" + writeTabs(tabulations, 1) + "\"tag\": \"preface\"\n" + writeTabs(tabulations) + "}";
				}
				else {
					$$ = writeTabs(tabulations) + "{\n" + writeTabs(tabulations, 1) + "\"tag\": \"preface\",\n" + writeTabs(tabulations, 1) + "\"content\": [\n" + writeTabs(tabulations, 2) + "\"" + $4 + "\"\n" + writeTabs(tabulations, 1) + "]\n" + writeTabs(tabulations) + "}"; 
				}
			}			
        ;

parts : part { $$ = $1; }
	  | parts part { $$ = $1 + ",\n" + $2; }
	  ;
	  
part : OPEN_TAG PART ID EQ CDATA TITLE EQ CDATA CLOSE_TAG part_contents
		{			
			if($5.equals(""))
				{$$ = writeTabs(tabulations) + "{\n" + writeTabs(tabulations, 1) + "\"tag\": \"part\",\n" + writeTabs(tabulations, 1) + "\"@id\": " + $5 + ",\n" + writeTabs(tabulations, 1) + "\"content\": [\n" + $10 + "\n" + writeTabs(tabulations, 1) + "]\n" + writeTabs(tabulations) + "}";}
			else
				{$$ = writeTabs(tabulations) + "{\n" + writeTabs(tabulations, 1) + "\"tag\": \"part\",\n" + writeTabs(tabulations, 1) + "\"@id\": " + $5 + ",\n" + writeTabs(tabulations, 1) + "\"@title\": " + $8 + ",\n" + writeTabs(tabulations, 1) + "\"content\": [\n" + $10 + "\n" + writeTabs(tabulations, 1) + "]\n" + writeTabs(tabulations) + "}";}
			
		}
	 | OPEN_TAG PART TITLE EQ CDATA ID EQ CDATA CLOSE_TAG part_contents
		{ 	
			if($5.equals(""))
				{$$ = writeTabs(tabulations) + "{\n" + writeTabs(tabulations, 1) + "\"tag\": \"part\",\n" + writeTabs(tabulations, 1) + "\"@id\": " + $8 + ",\n" + writeTabs(tabulations, 1) + "\"content\": [\n" + $10 + "\n" + writeTabs(tabulations, 1) + "]\n" + writeTabs(tabulations) + "}";}
			else
				{$$ = writeTabs(tabulations) + "{\n" + writeTabs(tabulations, 1) + "\"tag\": \"part\",\n" + writeTabs(tabulations, 1) + "\"@title\": " + $5 + ",\n" + writeTabs(tabulations, 1) + "\"@id\": " + $8 + ",\n" + writeTabs(tabulations, 1) + "\"content\": [\n" + $10 + "\n" + writeTabs(tabulations, 1) + "]\n" + writeTabs(tabulations) + "}";} 	
		}
	 | OPEN_TAG PART ID EQ CDATA CLOSE_TAG part_contents
		{ 	
			$$ = writeTabs(tabulations) + "{\n" + writeTabs(tabulations, 1) + "\"tag\": \"part\",\n" + writeTabs(tabulations, 1) + "\"@id\": " + $5 + ",\n" + writeTabs(tabulations, 1) + "\"content\": [\n" + $7 + "\n" + writeTabs(tabulations, 1) + "]\n" + writeTabs(tabulations) + "}";
		}
	 ;
	 
part_contents : toc chapters opt_part_contents
					{ if($3.equals("")){
						$$ = $1 + ",\n" + $2;
					  } else {
						$$ = $1 + ",\n" + $2 + ",\n" + $3;
					  }
					}					
			  ;
			  
opt_part_contents : lof opt_part_contents2
					{ if($2.equals("")){
						$$ = $1; 
					  }	else {
						$$ = $1 + ",\n" + $2;
					  }						
					}
				  | opt_part_contents2 { $$ = $1;}
				  ;

opt_part_contents2 : lot OPEN_TAG SLASH PART CLOSE_TAG {}
					 { $$ = $1;}
				   | OPEN_TAG SLASH PART CLOSE_TAG {$$ = "";}
				   ;

toc : OPEN_TAG TOC CLOSE_TAG items OPEN_TAG SLASH TOC CLOSE_TAG
		{	
			$$ = writeTabs(tabulations) + "{\n" + writeTabs(tabulations, 1) + "\"tag\": \"toc\",\n" + writeTabs(tabulations, 1) + "\"content\": [\n" + $4 + "\n" + writeTabs(tabulations, 1) + "]\n" + writeTabs(tabulations) + "}";
		}
	;

lof : OPEN_TAG LOF CLOSE_TAG items OPEN_TAG SLASH LOF CLOSE_TAG
		{	
			$$ = writeTabs(tabulations) + "{\n" + writeTabs(tabulations, 1) + "\"tag\": \"lof\",\n" + writeTabs(tabulations, 1) + "\"content\": [\n" + $4 + "\n" + writeTabs(tabulations, 1) + "]\n" + writeTabs(tabulations) + "}";
		}
	;
	
lot : OPEN_TAG LOT CLOSE_TAG items OPEN_TAG SLASH LOT CLOSE_TAG
		{	
			$$ = writeTabs(tabulations) + "{\n" + writeTabs(tabulations, 1) + "\"tag\": \"lot\",\n" + writeTabs(tabulations, 1) + "\"content\": [\n" + $4 + "\n" + writeTabs(tabulations, 1) + "]\n" + writeTabs(tabulations) + "}";	
		}
	;
		
items : item { $$ = $1 ;}
	  | items item { $$ = $1 + ",\n" + $2; }
	  ;
	  
item : OPEN_TAG ITEM ID EQ CDATA CLOSE_TAG pcdatas OPEN_TAG SLASH ITEM CLOSE_TAG 
		{ 	
			$$ = writeTabs(tabulations) + "{\n" + writeTabs(tabulations, 1) + "\"tag\": \"item\",\n" + writeTabs(tabulations, 1) + "\"@id\": " + $5 + ",\n" + writeTabs(tabulations, 1) + "\"content\": [\n" + writeTabs(tabulations, 2) + "\"" + $7 + "\"\n" + writeTabs(tabulations, 1) + "]\n" + writeTabs(tabulations) + "}"; 
		}
	 ;
		
chapters : chapter { $$ = $1; }
		 | chapters chapter { $$ = $1 + ",\n" + $2; } 
		 ;
		 
chapter : OPEN_TAG CHAPTER ID EQ CDATA TITLE EQ CDATA CLOSE_TAG sections OPEN_TAG SLASH CHAPTER CLOSE_TAG 
			{ 	
				$$ = writeTabs(tabulations) + "{\n" + writeTabs(tabulations, 1) + "\"tag\": \"chapter\",\n" + writeTabs(tabulations, 1) + "\"@id\": " + $5 + ",\n" + writeTabs(tabulations, 1) + "\"@title\": " + $8 + ",\n" + writeTabs(tabulations, 1) + "\"content\": [\n" + $10 + "\n" + writeTabs(tabulations, 1) + "]\n" + writeTabs(tabulations) + "}";
			}
		| OPEN_TAG CHAPTER TITLE EQ CDATA ID EQ CDATA CLOSE_TAG sections OPEN_TAG SLASH CHAPTER CLOSE_TAG 
			{	
				$$ = writeTabs(tabulations) + "{\n" + writeTabs(tabulations, 1) + "\"tag\": \"chapter\",\n" + writeTabs(tabulations, 1) + "\"@title\": " + $5 + ",\n" + writeTabs(tabulations, 1) + "\"@id\": " + $8 + ",\n" + writeTabs(tabulations, 1) + "\"content\": [\n" + $10 + "\n" + writeTabs(tabulations, 1) + "]\n" + writeTabs(tabulations) + "}";
			}
		; 
		 
sections : section { $$ = $1; } 
		 | sections section { $$ = $1 + ",\n" + $2; }
		 ;

section : OPEN_TAG SECTION ID EQ CDATA TITLE EQ CDATA CLOSE_TAG s_contents OPEN_TAG SLASH SECTION CLOSE_TAG 
			{	 
				$$ = writeTabs(tabulations) + "{\n" + writeTabs(tabulations, 1) + "\"tag\": \"section\",\n" + writeTabs(tabulations, 1) + "\"@id\": " + $5 + ",\n" + writeTabs(tabulations, 1) + "\"@title\": " + $8 + ",\n" + writeTabs(tabulations, 1) + "\"content\": [\n" + $10 + "\n" + writeTabs(tabulations, 1) + "]\n" + writeTabs(tabulations) + "}"; 
			}
		| OPEN_TAG SECTION TITLE EQ CDATA ID EQ CDATA CLOSE_TAG s_contents OPEN_TAG SLASH SECTION CLOSE_TAG
			{	 
				$$ = writeTabs(tabulations) + "{\n" + writeTabs(tabulations, 1) + "\"tag\": \"section\",\n" + writeTabs(tabulations, 1) + "\"@title\": " + $5 + ",\n" + writeTabs(tabulations, 1) + "\"@id\": " + $8 + ",\n" + writeTabs(tabulations, 1) + "\"content\": [\n" + $10 + "\n" + writeTabs(tabulations, 1) + "]\n" + writeTabs(tabulations) + "}"; 
			}
		;
		
s_contents : s_content { $$ = $1; }
		   | s_contents s_content
			{
				String temp = "";
				temp = $1 + ",\n" + $2;
				$$ = temp.replaceAll( "(\",\n[\t]+\"(?!((content\")|(@))))" , "");
			}
		   ;
		   
s_content : section { $$ = $1; }
		  | figure { $$ = $1; }
		  | table { $$ = $1; }
		  | PCDATA { $$ = writeTabs(tabulations) + "\"" + $1.replace("\r", "").replace("\n", "") + "\""; }
		  ;
		  
figure : OPEN_TAG FIGURE ID EQ CDATA CAPTION EQ CDATA PATH EQ CDATA SLASH CLOSE_TAG
			{ 	
				if($11.equals("")){
					$$ = writeTabs(tabulations) + "{\n" + writeTabs(tabulations, 1) + "\"tag\": \"figure\",\n" + writeTabs(tabulations, 1) + "\"@id\": " + $5 + ",\n" + writeTabs(tabulations, 1) + "\"@caption\": " + $8 + ",\n" + writeTabs(tabulations, 1) + "\"@path\": \"placeholder.jpg\"\n" + writeTabs(tabulations) + "}" ;
				} else {
					$$ = writeTabs(tabulations) + "{\n" + writeTabs(tabulations, 1) + "\"tag\": \"figure\",\n" + writeTabs(tabulations, 1) + "\"@id\": " + $5 + ",\n" + writeTabs(tabulations, 1) + "\"@caption\": " + $8 + ",\n" + writeTabs(tabulations, 1) + "\"@path\": " + $11 + "\n" + writeTabs(tabulations) + "}" ;
				}
			}
	   | OPEN_TAG FIGURE ID EQ CDATA PATH EQ CDATA CAPTION EQ CDATA SLASH CLOSE_TAG
			{	
				if($8.equals("")) {
					$$ = writeTabs(tabulations) + "{\n" + writeTabs(tabulations, 1) + "\"tag\": \"figure\",\n" + writeTabs(tabulations, 1) + "\"@id\": " + $5 + ",\n" + writeTabs(tabulations, 1) + "\"@path\": \"placeholder.jpg\",\n" + writeTabs(tabulations, 1) + "\"@caption\": " + $11 + "\n" + writeTabs(tabulations) + "}" ;
				} else {
					$$ = writeTabs(tabulations) + "{\n" + writeTabs(tabulations, 1) + "\"tag\": \"figure\",\n" + writeTabs(tabulations, 1) + "\"@id\": " + $5 + ",\n" + writeTabs(tabulations, 1) + "\"@path\": " + $8 + ",\n" + writeTabs(tabulations, 1) + "\"@caption\": " + $11 + "\n" + writeTabs(tabulations) + "}" ;
				}
			}
	   | OPEN_TAG FIGURE ID EQ CDATA CAPTION EQ CDATA SLASH CLOSE_TAG
			{ 	
				$$ = writeTabs(tabulations) + "{\n" + writeTabs(tabulations, 1) + "\"tag\": \"figure\",\n" + writeTabs(tabulations, 1) + "\"@id\": " + $5 + ",\n" + writeTabs(tabulations, 1) + "\"@caption\": " + $8 + ",\n" + writeTabs(tabulations, 1) + "\"@path\": \"placeholder.jpg\"\n" + writeTabs(tabulations) + "}" ; 
				
			}
	   | OPEN_TAG FIGURE CAPTION EQ CDATA ID EQ CDATA PATH EQ CDATA SLASH CLOSE_TAG
			{ 	
				if($11.equals("")) {
					$$ = writeTabs(tabulations) + "{\n" + writeTabs(tabulations, 1) + "\"tag\": \"figure\",\n" + writeTabs(tabulations, 1) + "\"@caption\": " + $5 + ",\n" + writeTabs(tabulations, 1) + "\"@id\": " + $8 + ",\n" + writeTabs(tabulations, 1) + "\"@path\": \"placeholder.jpg\"\n" + writeTabs(tabulations) + "}" ; 
				} else {
					$$ = writeTabs(tabulations) + "{\n" + writeTabs(tabulations, 1) + "\"tag\": \"figure\",\n" + writeTabs(tabulations, 1) + "\"@caption\": " + $5 + ",\n" + writeTabs(tabulations, 1) + "\"@id\": " + $8 + ",\n" + writeTabs(tabulations, 1) + "\"@path\": " + $11 + "\n" + writeTabs(tabulations) + "}" ;
				}
			}
	   | OPEN_TAG FIGURE CAPTION EQ CDATA PATH EQ CDATA ID EQ CDATA SLASH CLOSE_TAG
			{	
				if($8.equals("")) {
					$$ = writeTabs(tabulations) + "{\n" + writeTabs(tabulations, 1) + "\"tag\": \"figure\",\n" + writeTabs(tabulations, 1) + "\"@caption\": " + $5 + ",\n" + writeTabs(tabulations, 1) + "\"@path\": \"placeholder.jpg\",\n" + writeTabs(tabulations, 1) + "\"@id\": " + $11 + "\n" + writeTabs(tabulations) + "}" ;
				} else {
					$$ = writeTabs(tabulations) + "{\n" + writeTabs(tabulations, 1) + "\"tag\": \"figure\",\n" + writeTabs(tabulations, 1) + "\"@caption\": " + $5 + ",\n" + writeTabs(tabulations, 1) + "\"@path\": " + $8 + ",\n" + writeTabs(tabulations, 1) + "\"@id\": " + $11 + "\n" + writeTabs(tabulations) + "}" ;
				}
			}
	   | OPEN_TAG FIGURE CAPTION EQ CDATA ID EQ CDATA SLASH CLOSE_TAG
			{ 	
				$$ = writeTabs(tabulations) + "{\n" + writeTabs(tabulations, 1) + "\"tag\": \"figure\",\n" + writeTabs(tabulations, 1) + "\"@caption\": " + $5 + ",\n" + writeTabs(tabulations, 1) + "\"@id\": " + $8 + ",\n" + writeTabs(tabulations, 1) + "\"@path\": \"placeholder.jpg\"\n" + writeTabs(tabulations) + "}" ;
			}
	   | OPEN_TAG FIGURE PATH EQ CDATA ID EQ CDATA CAPTION EQ CDATA SLASH CLOSE_TAG
			{ 	
				if($8.equals(""))	{
					$$ = writeTabs(tabulations) + "{\n" + writeTabs(tabulations, 1) + "\"tag\": \"figure\",\n" + writeTabs(tabulations, 1) + "\"@path\": \"placeholder.jpg\",\n" + writeTabs(tabulations, 1) + "\"@id\": " + $8 + ",\n" + writeTabs(tabulations, 1) + "\"@caption\": " + $11 + "\n" + writeTabs(tabulations) + "}" ;
				} else {
					$$ = writeTabs(tabulations) + "{\n" + writeTabs(tabulations, 1) + "\"tag\": \"figure\",\n" + writeTabs(tabulations, 1) + "\"@path\": " + $5 + ",\n" + writeTabs(tabulations, 1) + "\"@id\": " + $8 + ",\n" + writeTabs(tabulations, 1) + "\"@caption\": " + $11 + "\n" + writeTabs(tabulations) + "}" ; 
				}
			}
	   | OPEN_TAG FIGURE PATH EQ CDATA CAPTION EQ CDATA ID EQ CDATA SLASH CLOSE_TAG
			{ 	
				if($11.equals(""))	{
					$$ = writeTabs(tabulations) + "{\n" + writeTabs(tabulations, 1) + "\"tag\": \"figure\",\n" + writeTabs(tabulations, 1) + "\"@path\": \"placeholder.jpg\",\n" + writeTabs(tabulations, 1) + "\"@caption\": " + $8 + ",\n" + writeTabs(tabulations, 1) + "\"@id\": " + $11 + "\n" + writeTabs(tabulations) + "}" ;
				} else {
					$$ = writeTabs(tabulations) + "{\n" + writeTabs(tabulations, 1) + "\"tag\": \"figure\",\n" + writeTabs(tabulations, 1) + "\"@path\": " + $5 + ",\n" + writeTabs(tabulations, 1) + "\"@caption\": " + $8 + ",\n" + writeTabs(tabulations, 1) + "\"@id\": " + $11 + "\n" + writeTabs(tabulations) + "}" ;
				}	
			}
	   ;
	   
table : OPEN_TAG TABLE ID EQ CDATA CAPTION EQ CDATA CLOSE_TAG rows OPEN_TAG SLASH TABLE CLOSE_TAG
		{ 	
			$$ = writeTabs(tabulations) + "{\n" + writeTabs(tabulations, 1) + "\"tag\": \"table\",\n" + writeTabs(tabulations, 1) + "\"@id\": " + $5 + ",\n" + writeTabs(tabulations, 1) + "\"@caption\": " + $8 + ",\n" + writeTabs(tabulations, 1) + "\"content\": [\n" + $10 + "\n" + writeTabs(tabulations, 1) + "]\n" + writeTabs(tabulations) + "}"; 
		}
	  | OPEN_TAG TABLE CAPTION EQ CDATA ID EQ CDATA CLOSE_TAG rows OPEN_TAG SLASH TABLE CLOSE_TAG
		{	
			$$ = writeTabs(tabulations) + "{\n" + writeTabs(tabulations, 1) + "\"tag\": \"table\",\n" + writeTabs(tabulations, 1) + "\"@caption\": " + $5 + ",\n" + writeTabs(tabulations, 1) + "\"@id\": " + $8 + ",\n" + writeTabs(tabulations, 1) + "\"content\": [\n" + $10 + "\n" + writeTabs(tabulations, 1) + "]\n" + writeTabs(tabulations) + "}";
		}
		;
		
rows : row { $$ = $1 ;}
	 | rows row { $$ = $1 + ",\n" + $2; }
	 ;
	 
row : OPEN_TAG ROW CLOSE_TAG cells OPEN_TAG SLASH ROW CLOSE_TAG
		{	
			$$ = writeTabs(tabulations) + "{\n" + writeTabs(tabulations, 1) + "\"tag\": \"row\",\n" + writeTabs(tabulations, 1) + "\"content\": [\n" + $4 + "\n" + writeTabs(tabulations, 1) + "]\n" + writeTabs(tabulations) + "}";
		}
	;
	
cells : cell { $$ = $1; }
	  |	cells cell { $$ = $1 + ",\n" + $2; }
	  ;
	  
cell : OPEN_TAG CELL CLOSE_TAG pcdatas OPEN_TAG SLASH CELL CLOSE_TAG
			{ 	
				$$ = writeTabs(tabulations) + "{\n" + writeTabs(tabulations, 1) + "\"tag\": \"cell\",\n" + writeTabs(tabulations, 1) + "\"content\": [\n" + writeTabs(tabulations, 2) + "\"" + $4 + "\"\n" + writeTabs(tabulations, 1) + "]\n" + writeTabs(tabulations) + "}";
			}
	  ;
	
pcdatas : PCDATA { $$ = $1.replace("\r", "").replace("\n", ""); }
		| pcdatas PCDATA { String temp=""; temp = $1.replace("\r", "").replace("\n", ""); $$ = temp + $2; }
		
		

%%

  private Yylex lexer;


  private int yylex () {
    int yyl_return = -1;
    try {
      yylval = new ParserVal(0);
      yyl_return = lexer.yylex();
    }
    catch (IOException e) {
      System.err.println("IO error :"+e);
    }
    return yyl_return;
  }


  public void yyerror (String error) {
    System.err.println ("Error: " + error);
  }


  public Parser(Reader r) {
	tabulations = 0;
    lexer = new Yylex(r, this);
  }

  static FileWriter output;
  
  public static void main(String args[]) throws IOException {
	outputFileName = args[0].replace(".xml", "") + ".json";
	output = new FileWriter(outputFileName);
    Parser yyparser;
    if ( args.length > 0 ) {
      // parse a file
      yyparser = new Parser(new FileReader(args[0]));
      yyparser.yyparse();
    }
    else {
      System.out.println("ERROR: Provide an input file as Parser argument");
    }
	output.flush();
	output.close();
  }
  
	public static String outputFileName = "";
	public static int tabulations;
	
	public String writeTabs(int i, int j) {
		String s = "";
		int c = i*2+j;
		for (int k=0; k < c ; k++) {
			s = s + "\t";
		}
		return s;
	}
	
	public String writeTabs(int i) {
		return writeTabs(i, 0);	
	}
		
	public static void addTabs() {
		tabulations = tabulations + 1;
	}
	
	public static void subTabs() {
		tabulations = tabulations - 2;
	}