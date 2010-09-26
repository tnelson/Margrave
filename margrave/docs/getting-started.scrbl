#lang scribble/manual

@title{Getting Started}

@;defmodule[margrave]

Welcome to Margrave!

Margrave is a tool for analyzing access-control and configuration
policies (such as firewall policies). 
Margrave allows users to ask questions about the 
behavior of their policies, such as "Can a student ever access
another student's grades?" or "Did my changes to this firewall allow
any unexpected new traffic?" 

The following examples demonstrate how to get started using Margrave.
If you have not yet installed Margrave, see @secref{install}. 
To analyze your own IOS firewall configuration,
see @secref{gs-ios}. For non-IOS uses, see the overview in 
@secref{gs-existing} and the instructions for @secref{gs-create}
in Margrave's intermediate language.



Margrave is part of an ongoing research project. We appreciate your
feedback and suggestions (including feature requests). Send these
to tn@"@"cs.wpi.edu.

@;--------------------------------------------------------------------
@section[#:tag "install"]{Installing Margrave}

@;There are two versions of Margrave: lite and full. The lite
@;version allows commands and queries to be entered at a prompt. The full
@;version provides a GUI and better
@;support for writing programs involving Margrave queries, but requires
@;installing a host environment. 

@;@subsection{Lite Margrave}
@;
@;
@;@itemlist[
@;  @item{Make sure that you have a recent version of Java installed.
@;        Margrave requires Java 6 or later. You can download the latest
@;        version of Java at
@;        
@;        @url{http://www.java.com/en/download/}.}
@;  @item{Download the LITE Margrave distribution from
@;        
@;        @url{http://www.margrave-tool.org/v3}.
@;        
@;        This is available for both Windows (as a zip file) and 
@;        *nix (as a tar.gz file).}
@;  @item{Extract the distribution archive to the location of your choice.}
@;  @item{Set your MARGRAVE_HOME environment variable to the location of
@;        the margrave-lite executable in the extracted distribution.}      
@;]

@;Now that Margrave Lite is installed, 
@;see @secref["running-lite"] for instructions on running it.


@;@subsection{Full Margrave}

@itemlist[
  @item{Make sure that you have a recent version of Java installed.
        Margrave requires Java 6 or later. You can download the latest
        version of Java at 
        
        @url{http://www.java.com/en/download/}.}
  @item{Download and install Racket from
        
        @url{http://racket-lang.org/download/}}
  @item{Open DrRacket (the graphical development environment for Racket) and select the 
        @italic{File -> Install .plt file} menu option. Enter this URL in the @italic{Web} tab:
        
        @url{http://www.margrave-tool.org/v3/margrave-full-300-stable.plt}
        
        Alternatively, download the .PLT file linked above and enter its location in the @italic{File} tab.}
  @;@item{Extract the distribution archive to the location of your choice.}
 @; @item{Set your MARGRAVE_HOME environment variable to the location of
 @;       margrave-full-main.rkt in the extracted distribution.}
]

@;@bold{For Mac OSX users:}
@;Environment variables set in your .bashrc 
@;file will only affect your terminal environment, and thus
@;will only be visible to DrRacket if you run it from your terminal prompt.
@;Making MARGRAVE_HOME available globally requires altering your 
@;~/.MacOSX/environment.plist file. For more information see 
@;@url{http://developer.apple.com/library/mac/#documentation/MacOSX/Conceptual/BPRuntimeConfig/Articles/EnvironmentVars.html}.

@; ^^^^^ IF adding this back in, explain why they care (If you WANT to run DrRacket from outside your command prompt...)

Your Margrave installation is in a location that depends on your username and the version of Racket you have installed. 
Every time you run Margrave, it will tell you where it is installed. For instance:

@verbatim|{
Searching for java executable ...
Using the java executable in: /usr/bin/java
--------------------------------------------------
Starting Margrave's Java engine...
    Margrave path was: /export/users1/tn/Racket/5.0.1/collects/margrave
}|

would tell you that Margrave is installed in @italic{/export/users1/tn/Racket/5.0.1/collects/margrave}.

Now that Margrave is installed, 
@;see @secref["running-full"] for instructions on running it.
see @secref["running-margrave"] for instructions on running it.

@;--------------------------------------------------------------------
@section[#:tag "running-margrave"]{Running Margrave}

@;Both versions of Margrave 
Margrave runs in the DrRacket programming environment. To begin, first open DrRacket.

@bold{If this is your first time running DrRacket}, you will see a message about choosing a language.
Go to the @italic{Language->Choose Language} menu and select "Use the language declared in the source", then click Ok.
Margrave scripts all begin with @bold{#lang margrave}, which tells DrRacket to interpret them as Margrave scripts.


@;Whichever version you use, y

@;margin-note{In-lining of Margrave commands in Racket is forthcoming.}

@;Commands may use standard Racket functions as well. For instance,
@;the @racket[build-path] function will construct a relative path:

@;@racketblock[(build-path margrave-home-path
@;                          "examples" "lite" "lite-ios-demo.rkt")]

@;Use the @racket[margrave-home-path] identifier to refer to the directory given 
@;in the MARGRAVE_HOME environment variable.



@;@subsection[#:tag "running-lite"]{Running Lite Margrave}

@;Execute the margrave-lite executable (margrave-lite.exe or ./margrave-lite,
@;depending on your OS). It will automatically detect where your Java
@;installation is located, and attempt to start the Margrave engine from
@;the directory you provided via the MARGRAVE_HOME environment variable.

@;To execute individual commands, enter them at the prompt.

@;To load and run a script in Margrave Lite, call the @racket[run-lite] function
@;with the relative path of the script file.
@;For instance, to load the example IOS firewall script, enter this at the 
@;REPL prompt:

@;@racketblock[(run-lite 
@;              (build-path margrave-home-path
@;                          "examples" "lite" "lite-ios-demo.rkt"))]

@;To exit Lite Margrave, type @racket[(exit)] at the command prompt. If
@;you close Lite Margrave via ctrl-C, the Java-based engine may be left 
@;running.

@;To use Margrave on IOS configurations, go to @secref["gs-ios"].
@;To use Margrave on other kinds of policies, go to @secref["gs-existing"].
@;Examples of both can be found in @italic{<MARGRAVE_HOME>/examples/lite}.

@;@subsection[#:tag "running-full"]{Running Full Margrave}

@;The full version of Margrave runs in DrRacket, the graphical development
@;environment for Racket. 

@subsection[#:tag "margrave-prompt"]{The Command Prompt}

Open a new DrRacket editor and change the first line to
@bold{#lang margrave}, then click Run. 
@margin-note{When you click Run, Margrave will always print out its current install location.}
Margrave will automatically detect where your Java installation is located, start its
Java engine, and display a Margrave prompt on the bottom half of the screen.
The command prompt accepts semicolon-terminated Margrave commands. For instance, entering:

INFO;

will execute the @tech{INFO} Margrave command, returning information about the 
state of Margrave, including memory usage and other statistics. 


@;You can use Margrave in your racket programs by importing the Margrave modules.
@;To do this, just
@;@racket[(require "<margrave-home>/margrave.rkt")], where <margrave-home> is 
@;the path containing margrave.rkt. If using IOS, import "margrave-ios.rkt" 
@;in the same way.

@subsection[#:tag "margrave-scripts"]{Margrave Scripts}

Margrave scripts are sequences of Margrave commands preceded by @bold{#lang margrave}. In 
@secref["margrave-prompt"], you created and ran an empty script to get the prompt. If you had
continued to enter Margrave commands, they would have been executed before the prompt appeared.

To run a saved Margrave script, open the file via DrRacket's File->Open menu and click the Run button.
The script will execute, and its results will be shown in the bottom half of the window. 

A Margrave prompt will always appear when the script is complete.

@subsection{Moving On}

To use Margrave on IOS configurations, go to @secref["gs-ios"].
To use Margrave on other kinds of policies, go to @secref["gs-existing"].
Examples of both can be found in the @italic{examples/scripts} sub-directory
of your Margrave installation.


@;@subsubsub*section{A Word of Caution}
   
@;Margrave's scenario-finding engine must be started by
@;invoking @racket[(start-margrave-engine)] before
@;any policies are loaded or Margrave commands are executed. 
@;Running margrave-full-main.rkt will do this automatically. If you
@;are writing your own script, make sure that it calls
@;@racket[(start-margrave-engine)].


  
@;--------------------------------------------------------------------
@section[#:tag "gs-ios"]{IOS in Margrave}

Margrave supports a core subset of the IOS language involving
standard and most extended ACLs, static NAT,
ACL-based and map-based dynamic NAT, static routing,
and policy-based routing. At the moment, our support for state is
limited to reflexive access-lists.

To parse and load an IOS policy into Margrave, use:

@;@racket[(parse-and-load-ios config-file-name config-file-path)]

LOAD IOS config-file-name

where @racket[config-file-name] is the file name of the configuration 
saved as a text file.

Margrave will produce several intermediate policy files (discussed in Section 4 of @cite{nbdfk10}) in
the same path as @racket[config-file-name] and load them.

For instance, if you have a configuration saved to a file config.txt in
the directory "/myfiles/Margrave/IOS", you should invoke:

LOAD IOS "/myfiles/Margrave/IOS/config.txt";

For detailed examples of running queries in IOS, see the "ios-demo" example
in the @italic{examples/scripts} sub-directory of your Margrave installation.
If you would like to experiment with a small IOS 
configuration, we suggest using
@italic{examples/policies/ios-demo/initial/demo.txt}.

@;  This works if the user is using the docs in their installation:
@; @url{../examples/policies/ios-demo/initial/demo.txt}

@subsection{Understanding IOS Scenarios}

The SHOW ALL, SHOW ONE, and SHOW NEXT commands format query results and display them 
in a concise format. A SHOW ONE command for an IOS query will produce output in this style:

@racketblock[
*** SOLUTION: Size = 15.
src-addr-in: IPAddress
protocol: prot-tcp
dest-addr-in: 192.168.5.10
src-port-in: port
exit-interface: interface
entry-interface: fe0
dest-port-in: port-80
length: length
ahostname: hostname-router
src-addr-out: IPAddress
dest-addr-out: 192.168.5.10
message: icmpmessage]

On the first line, the @racket[Size = 15] says that this scenario involves 15 atoms, where
each atom is an IP address, port, interface, etc.

The scenario itself says that the query can be satisfied by when the packet 
is a TCP request to the host 192.168.5.10 on port 80, entering the firewall at the fe0 interface.
There are no restrictions on the source fields of the packet header in this
scenario: "IPAddress" represents some IP address not explicitly
mentioned in the IOS configuration. Similarly for "Port" and "Interface".

The @italic{message} binding is only applicable for ICMP packets,
and can be ignored in this scenario. The @italic{length} binding is not yet used.
The @italic{-out} bindings give information about NAT effects on the packet; there
are none in this scenario.



When printing, only the most specific applicable 
information will be shown. E.g., you will never see
@racketblock[src-addr-in: 10.0.0.0/255.0.0.0 10.100.100.100]

since the host 10.100.100.100 is always contained in the network 
10.0.0.0/255.0.0.0. Instead, you would see
@racketblock[src-addr-in: 10.100.100.100]

@;--------------------------------------------------------------------
@section[#:tag "gs-existing"]{General Policies in Margrave}

Margrave's intermediate
language can capture many different kinds of policies. In this section, 
we discuss how to use the intermediate language to express policies for analysis.

Before running an example, we'll quickly overview just what a Margrave policy looks like.

@;@italic{What does a policy look like in Margrave?}

A policy's form depends on its @tech{vocabulary}. A vocabulary dictates what
a @tech{policy} @tech{request} is, what @tech{decision}s a policy renders,
and so on.  

Let's look at one of Margrave's built-in example policies, 
an access-control policy for a conference management system. Its vocabulary and policy files
are respectively @italic{conference1.v} and @italic{conference1.p} in your installation's
@italic{tests} directory.

The vocabulary is:

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


@secref["vocabularies"] contains details about this language. For now, note that this particular vocabulary gives the following:

@itemlist[
          @item{A notion of "subjects", "actions", and "resources" and some sub-types for each;}
          @item{"permit" and "deny" as valid decisions; and}
          @item{a "subject", an "action", and a "resource" as part of each request (given by the ReqVariables construct).}
          ]

The policy for this example is:

@racketblock[
(Policy ConferencePolicy1 uses conferencepolicy
        (Target )
        (Rules 
          (PaperNoConflict = (Permit s a r) :- (!Conflicted s r) (ReadPaper a) (Paper r))
          (PaperAssigned = (Permit s a r) :- (Assigned s r) (ReadPaper a) (Paper r))
          (PaperConflict = (Deny s a r) :- (Conflicted s r) (ReadPaper a) (Paper r)))
        (RComb FAC)
        (PComb FAC)
        (Children ))
]

This @tech{policy} @tech{uses} the above vocabulary, and 
contains three @tech{rules} that each map certain 
@tech{request}s to @tech{decision}s. For instance, the first rule,
named PaperNoConflict, causes the policy to permit requests to read 
papers, provided the subject is not known to be conflicted on the
paper. (For details, see @secref{policies}.)


@subsection{Example Queries}


Margrave loads policies using the LOAD POLICY command. 
LOAD POLICY takes a single parameter: the filename of the policy file. 
Since this policy is stored in the tests sub-folder of Margrave, we load it by 
calling:

LOAD POLICY "*MARGRAVE*/tests/conference1.p";

@margin-note{@bold{Shortcut: } If the filename path begins with @racket[*MARGRAVE*], the @racket[*MARGRAVE*] will be replaced with Margrave's installation directory.}

If the policy loads successfully, LOAD POLICY prints the policy's identifier for use in queries. In this case, it returns:

@racket["ConferencePolicy1"]


Let's ask Margrave whether a reviewer can ever be denied access to read a paper.
The following Margrave query captures this question. 

@racketblock[
EXPLORE ConferencePolicy1:Deny(s, a, r) AND 
        reviewer(s) AND paper(r) AND readpaper(a);
]        

@italic["reviewer(s)"], @italic["paper(r)"] and @italic["readpaper(a)"]
signify properties about the request: The subject is a reviewer,
etc. The term 
@italic["ConferencePolicy:Deny(s, a, r)"] states that the policy
renders the "Deny" decision for the request. @margin-note{Rendering deny is
not the same as failing to render permit!}
The @racket[EXPLORE] statement as a whole instructs Margrave to find 
scenarios that meet all of those conditions.

(For more information on Margrave's query language, see @secref["query-language"].) 
 
Once the query is created, 
we can ask for @italic{all} scenarios that satisfy the query with:

@racketblock[
SHOW ALL;
]
            
whereas 

@racketblock[
SHOW ONE;
]

shows only the first scenario found. After using @racket[SHOW ONE], 

@racketblock[
SHOW NEXT;             
]

will cycle through the remaining scenarios.

One of the solutions will be this one:

@racketblock[
*** SOLUTION: Size = 3.
$s: reviewer author
$r: paper
$a: readpaper
conflicted = {[$s, $r]}
assigned = {}
]

The block above represents a scenario where the query could be 
satisfied. The SHOW ALL, SHOW ONE, and SHOW NEXT
commands format query results and display them 
in this concise format. The scenario above says:
"The query can be satisfied when the Subject is
both a Reviewer and an Author, the Resource is a Paper, and 
the action is reading the paper, provided that
the subject is Conflicted on the Paper but not Assigned to it."

Here, $s, $a, and $r correspond to the variables that appear in
the query.
Size = 3 means that in this scenario, there were 3 objects. 
In this case, one is BOTH a Reviewer and an
Author, another is a Paper, and the third is the action ReadPaper.

Conflicted and Assigned are
binary @tech{predicates} mentioned in the policy. Any such facts 
will be printed after information about individual variables.

Note: When printing, only the most specific applicable 
information will be shown. E.g., you will never see
@italic{$s: reviewer subject}
because a reviewer is always a subject.


@;--------------------------------------------------------------------
@section[#:tag "gs-create"]{Creating Your Own Policies}

You can create your own policies in the same style as the examples above. @secref["policies"] and @secref["vocabularies"] contain more details on the policy language. Place the vocabulary in a text-file with the .v extension, and the policy in a text-file with the .p extension. Make sure that you refer to the correct vocabulary name in the policy file. For instance, if your vocabulary is in the file myvocab.v, make sure that the vocabulary is named @italic{myvocab} and the policy @italic{uses myvocab}.

@;To create your own policy, first decide on its vocabulary. What decisions should it render? What do its requests look like? What contraints always hold in the policy's domain? Then consult @secref["vocabularies"]. Create a text-file with the vocabulary expression and save it with the .v extension.

@;Once your vocabulary is created, consult @secref["policies"], and create your policy expression in a text-file in the same directory with the .p extension. Make sure that you refer to the correct vocabulary name in the policy file. For instance, if your vocabulary is in the file myvocab.v, make sure that the vocabulary is named @italic{myvocab} and the policy @italic{uses myvocab}.




