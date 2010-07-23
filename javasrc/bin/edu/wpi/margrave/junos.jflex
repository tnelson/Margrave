
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
import java_cup.runtime.Symbol;

%%

%cup
%line
%column
%unicode
%ignorecase
%class MJunosLexer

%{

Symbol newSym(int tokenId) 
{
    return new Symbol(tokenId, yyline, yycolumn);
}

Symbol newSym(int tokenId, Object value) 
{
    return new Symbol(tokenId, yyline, yycolumn, value);
}


%}

whitespace            = [ \t\n\r]


/* Tricky! . doesn't include newlines. So it's either:
   - Not a star (including cr/lf)
   - Stars up until non-star, non-slash (including cr/lf)
   repeated. (Be sure to allow for 1 or more trailing stars.) */

commentcontents          = ([^*]|[\r\n]|("*"+([^*/]|[\r\n])))*
commentsection 	         = ("/*"{commentcontents}("*")+"/")|(##.*)



dec64                    = (6[0-3]|[0-5]?[0-9])
hexbyte                  = ([0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F])
ipv6address              = {hexbyte}:{hexbyte}:{hexbyte}:{hexbyte}::{dec64}

dec32                    = (3[0-1]|[0-2]?[0-9])
decbyte                  = (25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)
ipv4address              = {decbyte}\.{decbyte}\.{decbyte}\.{decbyte}\/{dec32}

identifier               = [a-zA-Z0-9_\-\*<>\.]+

%%


/* ******* RULES START HERE ******* */

/* Order matters for same-length matches. Keywords first. */

"version"           { return newSym(MJunosSym.VERSION); }

"policy-options"    { return newSym(MJunosSym.POLICYOPTIONS); }
"policy-statement"  { return newSym(MJunosSym.POLICYSTATEMENT); }
"term"            { return newSym(MJunosSym.TERM); }
"from"            { return newSym(MJunosSym.FROM); }
"then"            { return newSym(MJunosSym.THEN); }
"family"          { return newSym(MJunosSym.FAMILY); }
"filter"          { return newSym(MJunosSym.FILTER); }
"firewall"        { return newSym(MJunosSym.FIREWALL); }
"groups"           { return newSym(MJunosSym.GROUP); }
"prefix-list"     { return newSym(MJunosSym.PREFIXLIST); }
"next-header"     { return newSym(MJunosSym.NEXTHEADER); }
"source-prefix-list" { return newSym(MJunosSym.SOURCEPREFIXLIST); }
"destination-prefix-list" { return newSym(MJunosSym.DESTINATIONPREFIXLIST); }
"destination-address" { return newSym(MJunosSym.DESTINATIONADDRESS); }
"source-address"      { return newSym(MJunosSym.SOURCEADDRESS); }
"destination-port"    { return newSym(MJunosSym.DESTINATIONPORT); }
"source-port"         { return newSym(MJunosSym.SOURCEPORT); }
"protocol"            { return newSym(MJunosSym.PROTOCOL); }
"interface"           { return newSym(MJunosSym.INTERFACE); }
"local-preference"    { return newSym(MJunosSym.LOCALPREFERENCE); }
"as-path"             { return newSym(MJunosSym.ASPATH); }
"community"           { return newSym(MJunosSym.COMMUNITY); }
"apply-groups"        { return newSym(MJunosSym.APPLYGROUPS); } 
"route-filter"        { return newSym(MJunosSym.ROUTEFILTER); }
"prefix-list-filter"  { return newSym(MJunosSym.PREFIXLISTFILTER); }
"next-hop"            { return newSym(MJunosSym.NEXTHOP); }
"orlonger"            { return newSym(MJunosSym.ORLONGER); }

/* Decision keywords */
/* One term can contain more than one decision, e.g. reject and log and count */
/* Motivates new rule structure, >1 decision per rule.*/

"reject"          { return newSym(MJunosSym.REJECT); }
"accept"          { return newSym(MJunosSym.ACCEPT); }
"count"           { return newSym(MJunosSym.COUNT); }
"discard"         { return newSym(MJunosSym.DISCARD); }
"log"             { return newSym(MJunosSym.LOG); }


{ipv4address}     { return newSym(MJunosSym.IPV4, yytext().toLowerCase()); }
{ipv6address}     { return newSym(MJunosSym.IPV6, yytext().toLowerCase()); }
{identifier}      { return newSym(MJunosSym.IDENTIFIER, yytext().toLowerCase()); }

{commentsection}  { return newSym(MJunosSym.COMMENT, yytext()); }

"{"               { return newSym(MJunosSym.LBRACE); }
"}"	          { return newSym(MJunosSym.RBRACE); }
"["               { return newSym(MJunosSym.LBRACKET); }
"]"	          { return newSym(MJunosSym.RBRACKET); }
";"               { return newSym(MJunosSym.SEMICOLON); }



{whitespace}      { /* Ignore whitespace */ }
.		  { throw new MLexerException("Could not start a new lexical token.", yycolumn, yyline, yychar, yytext()); }