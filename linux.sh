#1. 设置环境变量
work=$(pwd)
export RISCV=/nfs/home/share/riscv/
export RISCV_ROOTFS_HOME=$(work)/riscv-rootfs

# 2. 配置linux
cd $(work)/riscv-linux/
# 删除这一行 -YYLTYPE yylloc;
git apply /nfs/home/share/chengguanghui/patch/linux-yylloc.patch



cd $(work)/riscv-pk/
git apply /nfs-nvme/home/tanghaojin/riscv-pk.patch
make -j 100