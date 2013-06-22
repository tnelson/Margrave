#!/usr/bin/python
# 
# Margrave extension for Capirca
# Tim Nelson
# experimental code only!

"""Margrave generator."""

__author__ = 'tn@cs.wpi.edu (Tim Nelson)'

import datetime
import logging
import re
import os

from third_party import ipaddr
import aclgenerator
import nacaddr

def portrange(ptr):
  return "p"+str(ptr[0])+"-"+str(ptr[1])

def safeip(s):
  return "ip"+str(s).replace(":", ".")

def port(pt):
  return "p"+str(pt)

class Margrave(aclgenerator.ACLGenerator):
  """A Margrave policy object"""

  _PLATFORM = 'margrave'
  _DEFAULT_PROTOCOL = 'ip'
  _SUFFIX = '.p'

  myfilename = ""

  def __init__(self, pol, exp_info, filename):
    aclgenerator.ACLGenerator.__init__(self, pol, exp_info)
    phead, ptail = os.path.split(filename)
    ehead, etail = os.path.splitext(ptail)
    self.myfilename = ehead

  _OPTIONAL_SUPPORTED_KEYWORDS = set(['address',
                                      'counter',
                                      'expiration',
                                      'logging',
                                      'loss_priority',
                                      'owner',
                                      'policer',
                                      'port',
                                      'qos',
                                     ])

  vpros = set()
  vips = set()
  vports = set()
  vportranges = set()
  vopts = set()

  def _TranslatePolicy(self, pol, exp_info):
    print 'translating...'  


  def dupeAnd(self, pattern, lst, voc):
    return self.dupe(pattern, lst, voc, 'and')

  def dupeOr(self, pattern, lst, voc):
    return self.dupe(pattern, lst, voc, 'or')


  def dupe(self, pattern, lst, voc, op):
    ret_str = ""
    if len(lst) > 1:
      ret_str += '(' + op
    for ele in lst:
      if type(ele) is tuple:
        if len(ele) != 2:
          print "\n    ***** WARNING: ELE: %s\n" % ele
        elif ele[0] == ele[1]: 
          ret_str = ret_str + pattern.format(pred=port(ele[0]))  
          self.vports.add(ele[0])
        else:
          ret_str = ret_str + pattern.format(pred=portrange(ele))
          self.vportranges.add(ele)
      elif voc == 'ip':  
        ret_str = ret_str + pattern.format(pred=safeip(ele))
      else:
        ret_str = ret_str + pattern.format(pred=ele)
      if voc == 'ip':
        self.vips.add(ele)
      if voc == 'pro':
        self.vpros.add(ele)
      if voc == "option":
        self.vopts.add(ele)

    if len(lst) > 1:
      ret_str += ') '
    ret_str += '\n'
    return ret_str

  def termConditions(self, term):
    ret_str = []

    # Pass through each possible field. May need to break out conjunctions/disjunctions
    # Recall that Margrave policies now have (or ...) sugar for this sort of thing.
    if term.address:
      print "   ***** WARNING: SKIPPED OPTIONAL term.address field."

    if term.address_exclude:
      print "   ***** WARNING: SKIPPED OPTIONAL term.address_exclude field."


    if term.source_address:
      ret_str.append(self.dupeOr(' ({pred} sa)', term.source_address, 'ip'))
    if term.source_address_exclude:
      ret_str.append(self.dupeAnd(' (not ({pred} sa)) ', term.source_address_exclude, 'ip'))
    if term.destination_address:
      ret_str.append(self.dupeOr(' ({pred} da) ', term.destination_address, 'ip'))
    if term.destination_address_exclude:
      ret_str.append(self.dupeAnd(' (not ({pred} da)) ', term.destination_address_exclude, 'ip'))

    if term.source_prefix:
      print "   ***** WARNING: SKIPPED OPTIONAL term.source_prefix field."
    if term.destination_prefix:
      print "   ***** WARNING: SKIPPED OPTIONAL term.destination_prefix field."
    if term.protocol:
      ret_str.append(self.dupeOr(' ({pred} pro)', term.protocol, 'pro'))
    if term.protocol_except:
      ret_str.append(self.dupeAnd(' (not ({pred} pro))', term.protocol_except, 'pro'))
    if term.owner:
      print "   ***** WARNING: SKIPPED OPTIONAL term.owner field."
    if term.port:
      print "   ***** WARNING: SKIPPED OPTIONAL term.port field."
    if term.source_port:
      ret_str.append(self.dupeOr('({pred} sp)', term.source_port, 'port'))
    if term.destination_port:
      ret_str.append(self.dupeOr('({pred} dp)', term.destination_port, 'port'))
    #if term.action:
    #  ret_str.append('  action: %s' % term.action)
    if term.option:
      ret_str.append(self.dupeAnd('({pred} opt)', term.option, 'option'))
    if term.qos:
      print "   ***** WARNING: SKIPPED OPTIONAL term.qos field."
  #  if term.logging:
  #    ret_str.append('  logging: %s' % str(term.logging))
  #    print str(term.logging)
  #  if term.counter:
  #    ret_str.append('  counter: %s' % term.counter)
    if term.source_interface:
      print "   ***** WARNING: SKIPPED OPTIONAL term.source_interface field."
    if term.destination_interface:
      print "   ***** WARNING: SKIPPED OPTIONAL term.destination_interface field."
    
    #if term.expiration:
    #  ret_str.append('  expiration: %s' % term.expiration)
   
    if term.platform:
      print "   ***** WARNING: SKIPPED OPTIONAL term.platform field."
    if term.platform_exclude:
      print "   ***** WARNING: SKIPPED OPTIONAL term.platform_exclude field."
    if term.timeout:
      print "   ***** WARNING: SKIPPED OPTIONAL term.timeout field."

    # if no conditions, always true (empty conjunction)
    if (not term.source_address and 
       not term.source_address_exclude and 
       not term.destination_address and  
       not term.destination_address_exclude and
       not term.protocol and
       not term.protocol_except and
       not term.source_port and
       not term.destination_port and
       not term.option):
       ret_str.append(" true ")

    return ret_str;


## TODO: if log flag is set, spawn a dupe rule with log decision.

## todo: ivp4 and pv6 addresses wont overlap properly, since im using the 3rd pty library
## may tell me ip6 and ip4s can never overlap with each other.

  def termToRules(self, term):
    target = []
    
    for a in term.action:
      if len(term.action) < 2:
        name = term.name
      else:
        name = term.name + '-' + a
      
      reqvec = 'sa sp da dp pro opt'
      conditions = self.termConditions(term)
      target.append("(%s = (%s %s) :-\n    %s)\n" % ( name, a, reqvec, " ".join(conditions)))
      if term.logging:
        target.append("(%s = (log %s) :-\n    %s)\n" % ( name+'-log', reqvec, " ".join(conditions)))
        print "     *** LOGGING detected: splitting term: "+term.name
    return target

  def getaxioms(self):
    target = []

    # containment of ips
    # and disjointness of ip ranges
    # singleton constants
    for ip in self.vips:
      if ip._prefixlen == ip._max_prefixlen:
        target.append('(atmostone {x})'.format(x=safeip(str(ip))))
      for ip2 in self.vips:
        if ip!=ip2 and ip in ip2:
          target.append('(subset {x} {y})'.format(x=safeip(str(ip)),y=safeip(str(ip2))))
        #if not ip.overlap(ip2):
        if not ip in ip2 and not ip2 in ip:  
          target.append('(disjoint {x} {y})'.format(x=safeip(str(ip)),y=safeip(str(ip2))))

    # containment of ports
    # and disjointness of port ranges
    for ptr in self.vportranges:
      for pt in self.vports:
        if pt >= ptr[0] and pt <= ptr[1]:
          target.append('(subset {x} {y})'.format(x=port(pt),y=portrange(ptr)))
      for ptr2 in self.vportranges:
        if ptr[0] > ptr2[1] or ptr[1] < ptr2[0]:
          target.append('(disjoint {x} {y})'.format(x=portrange(ptr),y=portrange(ptr2)))
        if ptr[0] >= ptr2[0] and ptr[1] <= ptr2[1]: 
          target.append('(subset {x} {y})'.format(x=portrange(ptr),y=portrange(ptr2)))

    # singleton constants and disjointness of constants
    for pt in self.vports:
      target.append('(atmostone {x})'.format(x=port(pt)))      
      for pt2 in self.vports:
        if pt != pt2:
          target.append('(disjoint {x} {y})'.format(x=port(pt),y=port(pt2)))
  
    return "\n".join(target)

  def getpreds(self):
    target = []

    for ip in self.vips:
      target.append('({x} IPAddress)'.format(x=safeip(str(ip))))
    for pt in self.vports:
      target.append('({x} Port)'.format(x=port(pt)))
    for ptr in self.vportranges:
      target.append('({x} Port)'.format(x=portrange(ptr)))
    for pro in self.vpros:
      target.append('({x} Protocol)'.format(x=pro))
    for opt in self.vopts:
      target.append('({x} Options)'.format(x=opt))
      
    return "\n".join(target)

  def vocabStr(self):
    target = ['(Theory '+self.myfilename,
              '  (Vocab '+self.myfilename, 
              '     (Types IPAddress Port Protocol Options)',
              '     (Predicates      \n{preds}'.format(preds=self.getpreds()),
              '     )',
              '  )',
              '  (Axioms   \n{axioms}'.format(axioms=self.getaxioms()),
              '  )',
               ')'] 
    return '\n'.join(target);
  

  def __str__(self):

    target = []
    # add the p4 tags
    target.extend(aclgenerator.AddRepositoryTags('; '))

   
    target += ["(Policy (uses "+self.myfilename+") ", "  (Variables ", 
               "    (da IPAddress)",
               "    (sa IPAddress)",
               "    (sp Port)",
               "    (dp Port)",
               "    (pro Protocol)",
               "    (opt Options)",
              "  )",
              "  (Rules "]


    target += ['\n', 
    ';;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;']
    for header, terms in self.policy.filters:
      #target.append(str(header))
      for comment in header.comment:
        for line in comment.split('\n'):
          target.append('; %s' % line)
      target.append(';;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;')

      for t in terms:
        for comment in t.comment:
          target.append(';;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;')
          for line in comment.split('\n'):
            target.append('; ' + str(line))
          target.append(';;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;')
        target.extend(self.termToRules(t))

    target += [")\n  (RComb (fa accept deny))", ")"]

   # print '    '+str(self.vips) + '\n    ' +str(self.vports) + '\n    '+ str(self.vportranges)  + '\n    '+str(self.vpros)
    
    return '\n'.join(target)
