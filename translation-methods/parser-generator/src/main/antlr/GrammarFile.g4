grammar GrammarFile ;

grammarFile : name start stmt+ EOF ;

name : '# name: ' VARNAME ;

start : '# start: ' nonTerm ;

nonTerm : VARNAME ;

stmt : rule | lexeme ;

rule : nonTerm attrInhParam? attrSynParam? ':' productions ;

attrInhParam : LBRACE (VARNAME ':' type)+ RBRACE ;

attrSynParam : '->' type ;

type : INT | STRING ;

productions : product ('|' product)* ;

product :  productElement* returnElement? ;

returnElement : '>>' (nonTerm | VARNAME | CODE | CODEPLUS ) ;

productElement : element | CODE | EPS ;

element : nonTerm arguments? | LEXEME ;

arguments : LBRACE argument (',' argument)* RBRACE ;

argument : VARNAME ;

lexeme : LEXEME ':' (STR | regex) ;

regex : REG ;

EPS : 'eps' ;
INT : 'Int' ;
STRING : 'String' ;
LBRACE : '(' ;
RBRACE : ')' ;
REG : '@' .+? '@';
STR : '"' .+? '"' ;
NUM : [0-9]+ ;
LEXEME : [A-Z]+ ;
VARNAME : [a-zA-Z0-9]+ ;
CODE : '{' ~[{}]+ '}' ;
CODEPLUS : '`{' .+? '}`';
WS : [ \t\r\n]+ -> skip ;

