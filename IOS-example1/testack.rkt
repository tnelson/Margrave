#lang margrave
LOAD IOS "config-ack/config.txt" WITH "" "ack";

// Test part 1
EXPLORE inboundaclack:permit(<inboundaclack:req>) and not ack(flags) and inboundaclack:router-fastethernet0-line32_applies(<inboundaclack:req>) tupling;
is possible?; // should be false

EXPLORE inboundaclack:permit(<inboundaclack:req>) and not rst(flags) and inboundaclack:router-fastethernet0-line32-2_applies(<inboundaclack:req>) tupling;
is possible?; // should be false
