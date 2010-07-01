
/*
  	Copyright 2009-2010 Brown University and Worcester Polytechnic Institute.
    
    This file is part of Margrave.

    Margrave is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Margrave is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with Margrave.  If not, see <http://www.gnu.org/licenses/>.
 */


package edu.wpi.margrave;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import java_cup.runtime.Symbol;
import kodkod.ast.*;

%%

%cup
%line
%column
%unicode
%ignorecase
%class MCommandLexer

%{

Symbol newSym(int tokenId) 
{
    return new Symbol(tokenId, yyline, yycolumn);
}

Symbol newSym(int tokenId, Object value) 
{
    return new Symbol(tokenId, yyline, yycolumn, value);
}


Set<MIDBCollection> seenPolicies = new HashSet<MIDBCollection>();  

%}

whitespace            = [ \n\r\t]
identifier            = [^()\",=: \n\r\t]+
qidentifier           = \"[^\" \n\r\t]+\"
naturalnum			  = [0-9]+
possibleqmark		  = [pP][oO][sS][sS][iI][bB][lL][eE]\?
guaranteedqmark       = [gG][uU][aA][rR][aA][nN][tT][eE][eE][dD]\?
/* For more exotic EDB/IDB names: ones that contain : etc., quotes! */

%%


/* Semicolons are reserved by the system, as they are used to separate command statements. */
/* Therefore, no semicolon token. */

explore           { return newSym(MCommandSym.EXPLORE); }
and               { return newSym(MCommandSym.AND); }
or                { return newSym(MCommandSym.OR); }
not               { return newSym(MCommandSym.NOT); }
implies           { return newSym(MCommandSym.IMPLIES); }
iff               { return newSym(MCommandSym.IFF); }
under             { return newSym(MCommandSym.UNDER); } 
":"               { return newSym(MCommandSym.COLON); }

"="               { return newSym(MCommandSym.EQUALS); }
"("               { return newSym(MCommandSym.LPAREN); }
")"	              { return newSym(MCommandSym.RPAREN); }
show              { return newSym(MCommandSym.SHOW); }
all               { return newSym(MCommandSym.ALL); }
one               { return newSym(MCommandSym.ONE); }
populated         { return newSym(MCommandSym.POPULATED); }
unpopulated       { return newSym(MCommandSym.UNPOPULATED); }
idboutput         { return newSym(MCommandSym.IDBOUTPUT); }
is                { return newSym(MCommandSym.IS); } 
publish			  { return newSym(MCommandSym.PUBLISH); }
","               { return newSym(MCommandSym.COMMA); }
ceiling			  { return newSym(MCommandSym.CEILING); }
debug             { return newSym(MCommandSym.DEBUG); }
tupling           { return newSym(MCommandSym.TUPLING); } 
rename            { return newSym(MCommandSym.RENAME); }
info              { return newSym(MCommandSym.INFO); }
collapse          { return newSym(MCommandSym.COLLAPSE); }
compare           { return newSym(MCommandSym.COMPARE); }
// silent            { return newSym(MCommandSym.SILENT); }
for               { return newSym(MCommandSym.FOR); }
cases             { return newSym(MCommandSym.CASES); }

/* Added by TN 06/29/10 for DrRacket conversion */
add               { return newSym(MCommandSym.ADD); }
subsort           { return newSym(MCommandSym.SUBSORT); }
sort              { return newSym(MCommandSym.SORT); }
constraint        { return newSym(MCommandSym.CONSTRAINT); }
disjoint          { return newSym(MCommandSym.DISJOINT); }
nonempty          { return newSym(MCommandSym.NONEMPTY); }
singleton         { return newSym(MCommandSym.SINGLETON); }
atmostone         { return newSym(MCommandSym.ATMOSTONE); }
partial          { return newSym(MCommandSym.PARTIAL); }
function          { return newSym(MCommandSym.FUNCTION); }
total          { return newSym(MCommandSym.TOTAL); }
abstract          { return newSym(MCommandSym.ABSTRACT); }
subset          { return newSym(MCommandSym.SUBSET); }
set          { return newSym(MCommandSym.SET); }
target          { return newSym(MCommandSym.TARGET); }
predicate          { return newSym(MCommandSym.PREDICATE); }
rule          { return newSym(MCommandSym.RULE); }
to          { return newSym(MCommandSym.TO); }
create          { return newSym(MCommandSym.CREATE); }
vocabulary          { return newSym(MCommandSym.VOCABULARY); }
decision          { return newSym(MCommandSym.DECISION); }
requestvar          { return newSym(MCommandSym.REQUESTVAR); }
othervar          { return newSym(MCommandSym.OTHERVAR); }
policy          { return newSym(MCommandSym.POLICY); }
leaf          { return newSym(MCommandSym.LEAF); }
rcombine          { return newSym(MCommandSym.RCOMBINE); }
pcombine          { return newSym(MCommandSym.PCOMBINE); }
prepare          { return newSym(MCommandSym.PREPARE); }
load          { return newSym(MCommandSym.LOAD); }
xacml          { return newSym(MCommandSym.XACML); }
sqs          { return newSym(MCommandSym.SQS); }
get          { return newSym(MCommandSym.GET); }
count          { return newSym(MCommandSym.COUNT); }
size          { return newSym(MCommandSym.SIZE); }
ceiling          { return newSym(MCommandSym.CEILING); }
rules          { return newSym(MCommandSym.RULES); }
// with          { return newSym(MCommandSym.WITH); }
higher          { return newSym(MCommandSym.HIGHER); }
priority          { return newSym(MCommandSym.PRIORITY); }
than          { return newSym(MCommandSym.THAN); }
qualified       { return newSym(MCommandSym.QUALIFIED); }
// idbs            { return newSym(MCommandSym.IDBS); }
next           { return newSym(MCommandSym.NEXT); }
in           { return newSym(MCommandSym.IN); }
at           { return newSym(MCommandSym.AT); }
child           { return newSym(MCommandSym.CHILD); }
request           { return newSym(MCommandSym.REQUEST); }
vector           { return newSym(MCommandSym.VECTOR); }



/* case insensitivity doesn't apply to "possible?" because of the qmark. Had to be specific above. */
{possibleqmark}   { return newSym(MCommandSym.POSSIBLEQMARK); }
{guaranteedqmark} { return newSym(MCommandSym.GUARANTEEDQMARK); }

{naturalnum}      { return newSym(MCommandSym.NATURAL, Integer.parseInt(yytext())); }
{identifier}	  { return newSym(MCommandSym.IDENTIFIER, yytext().toLowerCase()); }
{qidentifier}     { return newSym(MCommandSym.IDENTIFIER, yytext().substring(1, yytext().length()-1).toLowerCase()); }


{whitespace}      { /* Ignore whitespace */ }
.		  { throw new MLexerException("Could not start a new lexical token.", yycolumn, yyline, yychar, yytext()); }