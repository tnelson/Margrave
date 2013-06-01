module maclearning_cornell

pred nLearned[st: ControllerState, pkt: LocPacket, sw: Switch, pt: PhysicalPort, mac: MacAddr] {
	sw = pkt.loc.switch 
	mac = pkt.packet.dlsrc	
}

pred pLearned[st: ControllerState, pkt: LocPacket, sw: Switch, pt: PhysicalPort, mac: MacAddr] {
	sw = pkt.loc.switch 
	mac = pkt.packet.dlsrc
	pt = pkt.loc.port	
}

pred fwdRule1[st: ControllerState, pkt: LocPacket, pkt' : LocPacket] {
	// R1 (know which)
	pkt.packet = pkt'.packet and // no modification of header
	pkt.loc.switch = pkt'.loc.switch and // no teleportation
	(pkt.loc.switch -> pkt'.loc.port -> pkt.packet.dldst) in st.learned
}

pred fwdRule2[st: ControllerState, pkt: LocPacket, pkt' : LocPacket] {
	// R2 (flood)
	pkt.packet = pkt'.packet and // no modification of header
	pkt.loc.switch = pkt'.loc.switch and // no teleportation
	not (pkt.loc.port = pkt'.loc.port) and // no backflow
    //  provided that this mac/switch pair is unknown.
	not (some sendto : PhysicalPort | (pkt.loc.switch -> sendto -> pkt.packet.dldst) in st.learned)
}

// Emit was ambiguous, forward is a better verb.
pred forward[st: ControllerState, pkt: LocPacket, pkt': LocPacket] {
	fwdRule1[st, pkt, pkt'] or 
	fwdRule2[st, pkt, pkt']
}

pred forceEmit[st: ControllerState, pkt: LocPacket, pkt': LocPacket] {
	// Elements of a pred are conjunctive, so the empty pred is true. Instead:
	some none
}


// No host mobility yet
one sig Topology { wires: set (PhysicalPort + MacAddr) -> (PhysicalPort + MacAddr), 
	  					  	    externalPorts: set PhysicalPort,
								switchToPort: set (PhysicalPort -> Switch)}
{
	externalPorts = wires.MacAddr

	// Constrain switch IDs and ports in non-endpoint locs
	all loc: Loc |
         (loc.switch not in MacAddr) implies 
 		 loc.port -> loc.switch in switchToPort
}


pred transitionFunction [st: ControllerState, pkt: LocPacket, st': ControllerState] {
	// For each relation in the state, produce a construct like this:
	st'.learned = st.learned
		- {  sw: Switch, pt: PhysicalPort, mac: MacAddr | nLearned[st, pkt, sw, pt, mac] }
		+{  sw: Switch, pt: PhysicalPort, mac: MacAddr | pLearned[st, pkt, sw, pt, mac] }
} 

/////////////////////////////////////////
// Constant pieces of the model:
/////////////////////////////////////////

sig LocPacket { packet: one Header,
    			             loc: one Loc }

fact packetAndHeaderAndLocEquality {
	// Packet is fully defined by header and loc:
	all p1, p2 : LocPacket | 
		(p1.loc = p2.loc and p1.packet = p2.packet) implies p1 = p2
	all l1, l2 : Loc |
		(l1.port = l2.port and l1.switch = l2.switch) implies l1 = l2

	// !!! VITAL: CHANGE THIS IF WE ADD NEW FIELDS TO HEADER
	all h1, h2 : Header |
		(h1.dlsrc = h2.dlsrc and h1.dldst = h2.dldst) implies h1 = h2
}

// !!! VITAL: if adding new fields here, change packetAndHeaderAndLocEquality
sig Header { dlsrc: one MacAddr,
                     dldst: one MacAddr}

// Policies only care about switches and switch ports. But for analysis, we need 
// the notion of a packet arriving at its final location. That's represented by a 
// mac address in each field.
sig Loc {
	switch: one (Switch + MacAddr),
     port: one (PhysicalPort + MacAddr)
} {
	(switch in MacAddr iff port in MacAddr)
	(switch in MacAddr implies switch = port)
}

abstract sig PseudoPort {}
abstract sig PhysicalPort extends PseudoPort {} 

abstract sig Switch {}
abstract sig MacAddr {}


// If starting in state [st], reception of [pkt] causes the production of [pkt]
// Note that the result is an OUTGOING packet.
fun packetResults[st: ControllerState, pkt: LocPacket] : set LocPacket {
	{ newpkt : LocPacket | forward[st, pkt, newpkt] or forceEmit[st, pkt, newpkt] }
}

// Account for topology
// Results are INCOMING packets (and thus can be located at a host)
fun packetResultsTopo [st: ControllerState, pkt: LocPacket] : set LocPacket {
	{ newpkt : LocPacket |
       	 some p : packetResults[st, pkt] | (p.loc.port -> newpkt.loc.port) in Topology.wires and
															newpkt.packet = p.packet // no mutation by topology
    }
}



////////////////////////////////////////////////////////////////////////////////////////////////
// STATE TRANSITIONS
////////////////////////////////////////////////////////////////////////////////////////////////

// State for MAC learning 
abstract sig ControllerState { learned: set Switch -> PhysicalPort -> MacAddr }



run {some disj st1,st2 : ControllerState | some pkt,newpkt : LocPacket | 
          no st1.learned and
          transitionFunction[st1, pkt, st2] and
         newpkt in  packetResultsTopo[st1, pkt] } for 4 but exactly 2 ControllerState
