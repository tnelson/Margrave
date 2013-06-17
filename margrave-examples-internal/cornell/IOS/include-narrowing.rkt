#lang margrave

// backed up here: don't try to run it.

///////////////////////////////////////////////////////
// The arity of returning-changes is too large for Kodkod: 8-ary with a universe size 18.
// So *narrow* the idb to include, since some of the fields are held constant by the query:

let narrowed-permit[
                      sa : IPAddress,
                      da : IPAddress,
                      sp : Port,  
                      dp : Port,                       
                      paf : PayloadAndFlags] be
  exists hostname : Hostname (
    exists entry : Interface (
      exists protocol : Protocol-any (
            
        InboundACL1:permit(hostname, entry, sa, da, sp, dp, protocol, paf))));
                              
show returning-changes
  include narrowed-permit(sa, da, sp, dp, paf),
          narrowed-permit(sa, da, sp, dp, paf);
///////////////////////////////////////////////////
  
  