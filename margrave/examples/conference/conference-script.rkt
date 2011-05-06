#lang margrave

#load policy PConf = "*margrave*/examples/conference/conference.p";

// Who can read scores?
let Q1[s: Subject, a: Action, r: Resource] be
  PConf.permit(s, a, r) and a : SubmitReview;
