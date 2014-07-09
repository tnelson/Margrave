#lang margrave
// MINIMIZE; // This command (no matter where it appears) activates minimization for the entire session.

LOAD POLICY c1 = "conference1.p";
LOAD POLICY c2 = "conference2.p";

///////////////////////////////////////////////////////

// "Show me when the policy permits"
LET whenpermit[s: Subject, a: Action, r: Resource] be 
  c1:permit(s,a,r);
show whenpermit;

///////////////////////////////////////////////////////

// "Show me WHY permitted" (no conflict, so assigned doesn't matter)
show whenpermit 
  INCLUDE c1:papernoconflict_applies(s,a,r), 
          c1:paperassigned_applies(s,a,r);

///////////////////////////////////////////////////////

// "Does the policy allow reading papers when conflicted?"
LET whenconflictedreadpaper[s: Subject, r: Resource] be 
  exists a: ReadPaper (c1:permit(s,a,r)) and conflicted(s, r);
poss? whenconflictedreadpaper;

///////////////////////////////////////////////////////

// "Show me differences"
COMPARE diff = c1 c2 (s,a,r);
show diff;

///////////////////////////////////////////////////////

// "Show me which rules ever apply during a difference:
show realized diff
          c1:papernoconflict_applies(s,a,r), 
          c1:paperassigned_applies(s,a,r),
          c1:paperconflict_applies(s,a,r),
          c2:papernoconflict_applies(s,a,r), 
          c2:paperassigned_applies(s,a,r),
          c2:paperconflict_applies(s,a,r);


// Before and after minimization turned on
count diff;
