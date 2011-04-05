#lang scribble/base

@(require scribble/manual
          racket/base
          "helper.rkt"
          (for-label racket/base
                     margrave/margrave))

@title{Getting Started}

Welcome to Margrave!

Margrave is part of an ongoing research project. We appreciate your
feedback and suggestions (including feature requests). 

@;--------------------------------------------------------------------
@section[#:tag "install"]{Installing Margrave}


@itemlist[
  @item{Make sure that you have a recent version of Java installed.
        Margrave requires Java 6 or later. You can download the latest
        version of Java at 
        
        @url{http://www.java.com/en/download/}.}
  @item{Download and install Racket from
        
        @url{http://racket-lang.org/download/}}
  @item{Open DrRacket (the graphical development environment for Racket) and select the 
        @onscreen{File|Install .plt file} menu option. Enter this URL in the @onscreen{Web} tab:
        
        @; !!!!!!!!!
        @; FOR DEVELOPMENT ONLY. When releasing, change to stable margrave-tool URL.
        @;@url{http://www.margrave-tool.org/v3/margrave-full-300-stable.plt}
        @url{http://www.cs.wpi.edu/~tn/temp/margrave-dev-1106a.plt}
        
        Alternatively, download the .plt file linked above and enter its location in the @onscreen{File} tab.}
]


Your Margrave install directory depends on your username and the version of Racket you have installed. 
Every time you run Margrave, it will tell you where it is installed. For instance:

@multiline-racketblock[
"Searching for java executable ...
Using the java executable in: /usr/bin/java
--------------------------------------------------
Starting Margrave's Java engine...
    Margrave path was: /users/tn/Racket/5.0.1/collects/margrave"]

would tell you that Margrave is installed in @tt{/users/tn/Racket/5.0.1/collects/margrave}.

@;--------------------------------------------------------------------
@section[#:tag "running-margrave"]{Running Margrave}

Margrave runs in the DrRacket programming environment. To begin, first open DrRacket.

@bold{If this is your first time running DrRacket}, you will see a message about choosing a language.
Go to the @onscreen{Language|Choose Language} menu and select ``Use the language declared in the source'', then click @onscreen{Ok}.



@subsection[#:tag "margrave-prompt"]{Tutorial: The Command Prompt}

Run DrRacket and open a new editor via the @onscreen{File|New} menu option.
Change the first line in the file to @tt{#lang margrave}, then click @onscreen{Run}. 
@margin-note{When you click @onscreen{Run}, Margrave will always print out its current install location.}

Margrave will automatically detect where your Java installation is located, start its
Java engine, and display a Margrave prompt on the bottom half of the screen.

@racketinput[]

Margrave runs queries against policies, so let's start by loading a policy. 
Click after the prompt in the bottom window, type:

@racketinput[#,(tt "LOAD POLICY \"*MARGRAVE*/tests/conference1.p\";")]

and press the enter key. Margrave will reply with:

@racketblock[#, @inline-result{ConferencePolicy1}]

When loading policies, if you start a file path with @tt{*MARGRAVE*}, the @tt{*MARGRAVE*}
will be replaced with Margrave’s installation directory. Once the policy is loaded,
Margrave prints the policy's name for use in queries. Here the policy's name is 
@tt{ConferencePolicy1}.

Ask questions using the @tt{EXPLORE} command and get answers with the @tt{SHOW ONE/NEXT/ALL} commands.
For instance, to ask when the policy we just loaded yields a permit decision:

@racketinput[#,(tt "EXPLORE conferencepolicy1:permit(s, a, r);")]
@racketblock[#, @inline-result{Query created successfully.}]

@racketinput[#,(tt "SHOW ONE;")]
@multiline-racketblock[
"********* SOLUTION FOUND at size = 3 ******************
s: author reviewer 
a: readpaper 
r: paper 
conflicted = {} 
assigned = {} 

STATISTICS:  
Margrave computed that 3 would be a sufficient size ceiling.
No ceiling explicitly provided in the query's CEILING clause.
Used size ceiling: 3
********************************************************"]

@tt{SHOW ONE} tells Margrave to print a single scenario (in no particular order). 

@margin-note{Commands in Margrave are case-insensitive. Entering @tt{Show One} will do the same thing as entering @tt{SHOW ONE}.}

To get additional scenarios, use @tt{SHOW NEXT}:

@racketinput[#,(tt "SHOW NEXT;")]
@multiline-racketblock[
"********* SOLUTION FOUND at size = 3 ******************
s: author reviewer 
a: readpaper 
r: paper 
conflicted = {[s, r]}
assigned = {[s, r]}

STATISTICS: 
Margrave computed that 3 would be a sufficient size ceiling.
No ceiling explicitly provided in the query's CEILING clause.
Used size ceiling: 3
********************************************************"]

If Margrave finds no more solutions, it will say so. 

You can display @italic{all} the scenarios in the result with the @tt{SHOW ALL} command:

@racketinput[#, (tt "SHOW ALL;")]

(Results omitted.)

@tt{SHOW} commands always display results for the most recent @tt{EXPLORE} command.

You can write more refined queries using @tt{AND}, @tt{OR}, @tt{NOT}, @tt{IMPLIES}, and @tt{IFF}, as well as parentheses. This query asks for scenarios where a request is both permitted and denied:

@multiline-racketinput[
"EXPLORE conferencepolicy1:permit(s, a, r) 
      AND conferencepolicy1:deny(s, a, r);"]
@racketblock[#, @inline-result{Query created successfully.}]

@racketinput[#,(tt "SHOW ONE;")]
@multiline-racketblock[
"---> No more solutions! <---

STATISTICS: 
Margrave computed that 3 would be a sufficient size ceiling.
No ceiling explicitly provided in the query's CEILING clause.
Used size ceiling: 3"]

Queries can involve multiple policies. Let's load a second policy and try asking how the permit different requests:

@racketinput[#,(tt "LOAD POLICY \"*MARGRAVE*/tests/conference2.p\";")]
@racketblock[#, @inline-result{ConferencePolicy2}]

@multiline-racketinput[
"EXPLORE (ConferencePolicy1:permit(s,a,r) 
  AND NOT ConferencePolicy2:permit(s,a,r))
      OR  (ConferencePolicy2:permit(s,a,r)
  AND NOT ConferencePolicy1:permit(s,a,r));"]
@racketblock[#, @inline-result{Query created successfully.}]

@racketinput[#,(tt "SHOW ONE;")]
@multiline-racketblock[
"********* SOLUTION FOUND at size = 3 ******************
s: author reviewer 
a: readpaper 
r: paper 
conflicted = {[s, r]}
assigned = {[s, r]}

STATISTICS: 
Margrave computed that 3 would be a sufficient size ceiling.
No ceiling explicitly provided in the query's CEILING clause.
Used size ceiling: 3
********************************************************"]

Since queries like this one completely describe how new and old versions of a policy disagree, we call them @deftech{change-impact} queries.

It can be cumbersome to manually write change-impact queries for policies with many decisions, so we provide a shortcut with the @tt{COMPARE} command:

@multiline-racketinput[
"COMPARE ConferencePolicy1 ConferencePolicy2;"]
@racketblock[#, @inline-result{Query created successfully.}]

Margrave commands are case-insensitive. The following queries are equivalent:

@multiline-racketinput[
"EXPLORE conferencepolicy1:permit(s, a, r);"]

@multiline-racketinput[
"explore ConferencePolicy1:Permit(s, a, r);"]




@subsection[#:tag "margrave-scripts"]{Tutorial: Margrave Scripts}

Of course, re-entering commands at the prompt can be tedious. You can avoid re-entering commands by using a script instead.
A Margrave script is a sequence of semicolon-terminated Margrave commands preceded by @bold{#lang margrave}. 

Open a new DrRacket editor and change the first line to
@tt{#lang margrave}. Instead of clicking @onscreen{Run} right away, add the following lines just under the @tt{#lang margrave} line:

@multiline-racketblock[
"LOAD POLICY \"*MARGRAVE*/tests/conference1.p\";
LOAD POLICY \"*MARGRAVE*/tests/conference2.p\";
EXPLORE conferencepolicy1:permit(s, a, r);
SHOW ONE;"]

Then click @onscreen{Run}. 
In the previous tutorial, you created and ran an empty script to get to
the prompt immediately. In this case, however, Margrave executes those four commands before giving you a prompt:

@multiline-racketblock[
"ConferencePolicy1
ConferencePolicy2
Query created successfully.
********* SOLUTION FOUND at size = 3 ******************
s: author reviewer 
a: readpaper 
r: paper 
conflicted = {}
assigned = {[s, r]}

STATISTICS: 
Margrave computed that 3 would be a sufficient size ceiling.
No ceiling explicitly provided in the query's CEILING clause.
Used size ceiling: 3
********************************************************
> "]

You can create scripts in DrRacket and save them via the @onscreen{File|Save Definitions As} menu option.
To open a saved Margrave script, use DrRacket's @onscreen{File|Open} menu option. 
The script will execute when you click @onscreen{Run}, and its results will appear in the bottom
half of the window, followed by a Margrave prompt.



@subsection{Moving On}

For help understanding scenario output and a primer on Margrave's intermediate policy language, go to 
@secref["gs-existing"].

For help using Margrave on IOS configurations, go to @secref["gs-ios"].

If you would rather jump right to more complex examples, see the scripts in the 
@tt{examples/scripts} sub-directory of your Margrave installation.

  
@;--------------------------------------------------------------------
@section[#:tag "gs-ios"]{IOS in Margrave}

Margrave supports a core subset of the IOS language involving
standard and most extended ACLs, static NAT,
ACL-based and map-based dynamic NAT, static routing,
and policy-based routing. Support for filtering based on TCP flags is
available for named, extended ACLs via the @tt{match-all} and @tt{match-any}
keywords (rather than the deprecated @tt{established} keyword). At the moment, our support for state is
limited to reflexive access-lists.

To parse and load an IOS policy into Margrave, use:

@tt{LOAD IOS <config-file-name>}

where @tt{<config-file-name>} is the file name of the IOS configuration 
saved as a text file.

Margrave will produce several intermediate policy files (discussed in Section 4 of @cite{nbdfk10}) in
the same path as @tt{<config-file-name>} and load them. 
For instance, if you have a configuration saved to a file @tt{config.txt} in
the directory @tt{/myfiles/Margrave/IOS}, you should invoke:

@multiline-racketinput["LOAD IOS \"/myfiles/Margrave/IOS/config.txt\";"]

@margin-note{If the configuration contains unsupported IOS commands, Margrave will skip the line in question and print a warning.}

For detailed examples of running queries in IOS, see the @tt{ios-demo.rkt} script
in the @tt{examples/scripts} sub-directory of your Margrave installation.
If you would like to experiment with a small IOS 
configuration, we suggest using
@tt{examples/policies/ios-demo/initial/demo.txt}.

@subsection{Understanding IOS Scenarios}

The @tt{SHOW ALL}, @tt{SHOW ONE}, and @tt{SHOW NEXT} commands format query results and display them 
in a concise format. A @tt{SHOW ONE} command for an IOS query will produce output in this style:

@multiline-racketblock[
"********* SOLUTION FOUND at size = 16 ******************
message: icmpmessage 
protocol: prot-tcp 
src-port-out: port 
dest-port-out: port 
next-hop: ipaddress 
entry-interface: fe0 
length: length 
dest-addr-in: 192.168.5.11 
src-addr-in: ipaddress 
dest-addr-out: ipaddress 
src-port-in: port 
exit-interface: interface 
hostname: hostname-router 
flags: tcpflags 
dest-port-in: port-25 
src-addr-out: ipaddress 

STATISTICS: 
Margrave computed that 1 would be a sufficient size ceiling.
No ceiling explicitly provided in the query's CEILING clause.
Used size ceiling: 1
********************************************************"
]

On the first line, @inline-result{Size = 16} says that this scenario involves 16 atoms, where
each atom is an IP address, port, interface, etc.

The scenario itself says that the query can be satisfied when the packet 
is a TCP request to the host @inline-result{192.168.5.11} on port @inline-result{25},
entering the firewall at the @inline-result{fe0} interface.
There are no restrictions on the source fields of the packet header in this
scenario: ``@inline-result{IPAddress}'' represents some IP address not explicitly
mentioned in the IOS configuration. Similarly for ``@inline-result{port}'' and ``@inline-result{interface}''.

The @inline-result{message} binding is only applicable for ICMP packets,
and can be ignored in this scenario. The @inline-result{length} binding is not yet used.
The @inline-result{-out} bindings give information about NAT effects on the packet; there
are none in this scenario. Similarly the @inline-result{flags} binding gives information
about the packet's TCP flags, which are not important in this scenario. 
The @inline-result{STATISTICS} section gives technical information about Margrave's scenario-finding process.


When printing, only the most specific applicable 
information will be shown. You will never see:

@multiline-racketblock{src-addr-in: 10.0.0.0/255.0.0.0 10.100.100.100}

since the host @inline-result{10.100.100.100} is always contained in the network 
@inline-result{10.0.0.0/255.0.0.0}. Instead, you would see

@multiline-racketblock{src-addr-in: 10.100.100.100}

@;--------------------------------------------------------------------
@section[#:tag "gs-existing"]{General Policies in Margrave}

Margrave's intermediate
language can capture many different kinds of policies. In this section, 
we discuss how to use the intermediate language to express policies for analysis.

First, we'll quickly overview what a Margrave policy looks like.

A policy's form depends on its @tech{vocabulary}. A vocabulary dictates what
a @tech{policy} @tech{request} is, what @tech{decision}s a policy renders,
and so on.  

Here is one of Margrave's built-in example policies, a fragment of an 
access-control policy for a conference management system. Its vocabulary and policy files
are respectively @tt{conference1.v} and @tt{conference1.p} in your installation's
@tt{tests} directory.

The vocabulary file contains:

@racketblock[(PolicyVocab ConferencePolicy
             (Types
               (Subject : Author Reviewer)
               (Action : SubmitPaper ReadPaper SubmitReview)
               (Resource : Paper Review))
             (Decisions 
               Permit
               Deny)
             (Predicates
               (Conflicted : Reviewer Paper)
               (Assigned : Reviewer Paper))

	     (ReqVariables (s : Subject)
                           (a : Action)
                           (r : Resource))
             (OthVariables )
             (Constraints
               (disjoint-all Resource)
               (disjoint-all Action)
               (atmostone-all Action)
	       (abstract Subject)
	       (abstract Action)
               (abstract Resource)
               (nonempty Subject)
               (nonempty Resource)))
             ]


The section ``@secref["vocabularies"]'' contains details about this language. For now, note that this particular vocabulary gives the following:

@itemlist[
          @item{the notion of ``subjects'', ``actions'', and ``resources'' and some sub-types for each;}
          @item{``permit'' and ``deny'' as valid decisions; and}
          @item{the fact that each request contains a subject, an action, and a resource (given by the @tt{ReqVariables} construct).}
          ]

The policy for this example is:

@racketblock[
(Policy ConferencePolicy1 uses conferencepolicy
        (Target )
        (Rules 
          (PaperNoConflict = (Permit s a r) :- 
                             (!Conflicted s r) (ReadPaper a) (Paper r))
          (PaperAssigned = (Permit s a r) :- 
                           (Assigned s r) (ReadPaper a) (Paper r))
          (PaperConflict = (Deny s a r) :- 
                           (Conflicted s r) (ReadPaper a) (Paper r)))
        (RComb FAC)
        (PComb FAC)
        (Children ))
]

This @tech{policy} uses the above vocabulary, and 
contains three @tech{rules} that each map certain 
@tech{request}s to @tech{decision}s. For instance, the first rule,
named @tt{PaperNoConflict}, causes the policy to permit requests to read 
papers, provided the subject is not known to be conflicted on the
paper. (For details, see the section ``@secref{policies}''.)


@subsection{Tutorial: Understanding Scenarios}

Paste the following script into DrRacket:

@multiline-racketblock-noresult[                       
"#lang margrave
LOAD POLICY \"*MARGRAVE*/tests/conference1.p\";
EXPLORE ConferencePolicy1:Deny(s, a, r) AND 
        reviewer(s) AND paper(r) AND readpaper(a);"
]        
 
and click @onscreen{Run}. At the command prompt, enter:

@multiline-racketinput["SHOW ALL;"]

One of the two scenarios printed will be this one:

@multiline-racketblock[
"********* SOLUTION FOUND at size = 3 ******************
s: author reviewer 
a: readpaper 
r: paper 
conflicted = {[s, r]}
assigned = {}

STATISTICS: 
Margrave computed that 3 would be a sufficient size ceiling.
No ceiling explicitly provided in the query's CEILING clause.
Used size ceiling: 3
********************************************************"]

This text represents a scenario where the query is
satisfied. The @tt{SHOW ALL}, @tt{SHOW ONE}, and @tt{SHOW NEXT}
commands format query results and display them 
in this concise format. The scenario above says:
``The query is satisfied when the subject is
both a @inline-result{reviewer} and an @inline-result{author}, the resource is a @inline-result{paper}, and 
the action is @inline-result{readpaper} (i.e. reading the paper), provided that
the subject is @inline-result{conflicted} on the paper but not @inline-result{assigned} to it.''

Here, @inline-result{s}, @inline-result{a}, and @inline-result{r} correspond to the variables that appear in
the query.
@inline-result{Size = 3} means that in this scenario, there were 3 objects. 
In this case, one is @(racketresultfont @italic{both}) an @inline-result{author} and a @inline-result{reviewer}.
Another is a @inline-result{paper}. The third represents the action @inline-result{readpaper}.

@inline-result{Conflicted} and @inline-result{assigned} are
binary @tech{predicates} mentioned in the policy. Any such facts 
will be printed after information about individual variables.

The @inline-result{STATISTICS} section gives technical information about Margrave's scenario-finding process.

Note: When printing, only the most specific applicable 
information will be shown. E.g., you will never see
@inline-result{s: reviewer subject}
because a reviewer is always a subject in this policy.


@;--------------------------------------------------------------------
@section[#:tag "gs-create"]{Creating Your Own Policies}

You can create your own policies in the same style as the examples above. The sections ``@secref["policies"]'' and ``@secref["vocabularies"]'' contain more details on the policy language. Place the vocabulary in a text-file with the .v extension, and the policy in a text-file with the .p extension. Make sure that you refer to the correct vocabulary name in the policy file. For instance, if your vocabulary is in the file @tt{myvocab.v}, make sure that the vocabulary is named @tt{myvocab} and the policy @tt{uses myvocab}.





