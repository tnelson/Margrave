(Policy
 LocalSwitching
 uses
 IOS-vocab
 (Target)
 (Rules
  (int-in_dmz-primary
   =
   (Forward hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-int hostname)
   (10.1.1.0/255.255.255.0 dest-addr-in)
   (= next-hop dest-addr-out)
   (IPAddress next-hop)
   (in_dmz exit-interface))
  (int-in_dmz-drop
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-int hostname)
   (10.1.1.0/255.255.255.0 dest-addr-in))
  (int-in_lan-primary
   =
   (Forward hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-int hostname)
   (192.168.0.0/255.255.0.0 dest-addr-in)
   (= next-hop dest-addr-out)
   (IPAddress next-hop)
   (in_lan exit-interface))
  (int-in_lan-drop
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-int hostname)
   (192.168.0.0/255.255.0.0 dest-addr-in))
  (int-default-route
   =
   (Pass hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-int hostname))
  (ext-out_inet-primary
   =
   (Forward hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-ext hostname)
   (10.200.0.0/255.255.0.0 dest-addr-in)
   (= next-hop dest-addr-out)
   (IPAddress next-hop)
   (out_inet exit-interface))
  (ext-out_inet-drop
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-ext hostname)
   (10.200.0.0/255.255.0.0 dest-addr-in))
  (ext-out_dmz-primary
   =
   (Forward hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-ext hostname)
   (10.1.1.0/255.255.255.0 dest-addr-in)
   (= next-hop dest-addr-out)
   (IPAddress next-hop)
   (out_dmz exit-interface))
  (ext-out_dmz-drop
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-ext hostname)
   (10.1.1.0/255.255.255.0 dest-addr-in))
  (ext-default-route
   =
   (Pass hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-ext hostname)))
 (RComb FAC)
 (PComb FAC)
 (Children))
