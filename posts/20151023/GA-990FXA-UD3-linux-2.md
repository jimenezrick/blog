% Using Gigabyte GA-990FXA-UD3 AM3+ with Linux (4.2.3)
% Ricardo Catalinas Jiménez
% 23 Oct 2015


  As I already explained in this
[other post](http://r.untroubled.be/post/20131208/GA-990FXA-UD3-linux),
the **IOMMU** of this motherboard requires a bit of tuning to keep the
kernel happy. Now it seems that with Linux 4.2.3 I need to tweak the
kernel boot parameters again. Keep the **IOMMU** enabled in the BIOS and
use now in the bootloader this parameter: `iommu=soft`.

With this, the motherboard should keep working perfectly fine.
