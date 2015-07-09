% Using Gigabyte GA-990FXA-UD3 AM3+ with Linux
% Ricardo Catalinas Jiménez
% 8 Dec 2013


  In case you are an owner of the motherboard **Gigabyte GA-990FXA-UD3
AM3+**, depending on your kernel and BIOS versions, it is possible that
you encounter the very same issues as I did.  Don't worry, it was just
a little bit tricky to find the fix but after that everything works
flawlessly.

  I currently run Arch Linux with a kernel version *3.12.3*. The issues
with this motherboard were that USB ports didn't work properly nor
Ethernet.  Many error messages related to **IOMMU** were also thrown to
`dmesg`.

  The solution is quite simple. Enable from the BIOS the `IOMMU` device
and append to your kernel boot parameters `iommu=pt`.

  One more issue was the overheating of the **VRM** heat sink. Just make
sure that the heat sink that covers this component is ventilated enough.
If not, you will see how the temperature of this heat sink rises and
under heavy load the CPU frequency starts jumping between its maximum
and minimum. Once the appropriate ventilation is in place, the CPU can
work at its maximum frequency for long periods of time.
