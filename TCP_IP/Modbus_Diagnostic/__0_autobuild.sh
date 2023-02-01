#!/bin/bash
# This script tests if cc exists
# if it exists it will be started,
# otherwise it will be build and then started.
clear
FILE="/sda5/sda5/Tools/Projects/Sample/TCP_IP/Modbus_Diagnostic/Modbus_Diagnostic"
if [ -f $FILE ]; then
   $FILE &
else
   lazbuild -B $FILE".lpi"
   $FILE &
fi


