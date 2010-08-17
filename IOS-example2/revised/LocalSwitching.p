(Policy
 LocalSwitching
 uses
 IOS-vocab
 (Target)
 (Rules
  (local-switch-primary-GigabitEthernet0/0-g8938
   =
   (Forward hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-tas hostname)
   (ip-10-232-0-0/ip-255-255-252-0 dest-addr-in)
   (= next-hop dest-addr-out)
   (IPAddress next-hop)
   (GigabitEthernet0/0 exit-interface))
  (local-switch-primary-drop-GigabitEthernet0/0-g8939
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-tas hostname)
   (ip-10-232-0-0/ip-255-255-252-0 dest-addr-in))
  (local-switch-primary-GigabitEthernet0/1-g8940
   =
   (Forward hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-tas hostname)
   (ip-10-232-8-0/ip-255-255-252-0 dest-addr-in)
   (= next-hop dest-addr-out)
   (IPAddress next-hop)
   (GigabitEthernet0/1 exit-interface))
  (local-switch-primary-drop-GigabitEthernet0/1-g8941
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-tas hostname)
   (ip-10-232-8-0/ip-255-255-252-0 dest-addr-in))
  (local-switch-primary-Serial0/3/0:0-g8942
   =
   (Forward hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-tas hostname)
   (ip-10-254-1-128/ip-255-255-255-252 dest-addr-in)
   (= next-hop dest-addr-out)
   (IPAddress next-hop)
   (Serial0/3/0:0 exit-interface))
  (local-switch-primary-drop-Serial0/3/0:0-g8943
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-tas hostname)
   (ip-10-254-1-128/ip-255-255-255-252 dest-addr-in))
  (local-switch-secondary-GigabitEthernet0/0-g8944
   =
   (Forward hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-tas hostname)
   (= next-hop dest-addr-out)
   (ip-10-232-4-0/ip-255-255-252-0 dest-addr-in)
   (IPAddress next-hop)
   (GigabitEthernet0/0 exit-interface))
  (local-switch-secondary-drop-GigabitEthernet0/0-g8945
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-tas hostname)
   (ip-10-232-4-0/ip-255-255-252-0 dest-addr-in))
  (default-route-g8891
   =
   (Pass hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   true)
  (local-switch-primary-GigabitEthernet0/0-g8946
   =
   (Forward hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (ip-10-232-100-0/ip-255-255-252-0 dest-addr-in)
   (= next-hop dest-addr-out)
   (IPAddress next-hop)
   (GigabitEthernet0/0 exit-interface))
  (local-switch-primary-drop-GigabitEthernet0/0-g8947
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (ip-10-232-100-0/ip-255-255-252-0 dest-addr-in))
  (local-switch-primary-Serial0/3/0:0-g8948
   =
   (Forward hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (ip-10-254-1-128/ip-255-255-255-252 dest-addr-in)
   (= next-hop dest-addr-out)
   (IPAddress next-hop)
   (Serial0/3/0:0 exit-interface))
  (local-switch-primary-drop-Serial0/3/0:0-g8949
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (ip-10-254-1-128/ip-255-255-255-252 dest-addr-in))
  (local-switch-secondary-GigabitEthernet0/0-g8950
   =
   (Forward hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (= next-hop dest-addr-out)
   (ip-10-232-104-0/ip-255-255-252-0 dest-addr-in)
   (IPAddress next-hop)
   (GigabitEthernet0/0 exit-interface))
  (local-switch-secondary-drop-GigabitEthernet0/0-g8951
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (ip-10-232-104-0/ip-255-255-252-0 dest-addr-in))
  (default-route-g8891
   =
   (Pass hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   true))
 (RComb FAC)
 (PComb FAC)
 (Children))
