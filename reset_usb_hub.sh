#!/bin/bash

# Source https://askubuntu.com/questions/927134/solving-usb-hub-failure-issues-without-reboot

[ `id -u` -ne 0 ] && exec sudo "$0"
cd /sys/bus/pci/drivers/xhci_hcd
BUS="0000:02:00.0"
echo "$BUS" > unbind
sleep 5
echo "$BUS" > bind
sleep 5
find "$BUS/" -name control -exec /bin/sh -c "echo on > {}" \;
