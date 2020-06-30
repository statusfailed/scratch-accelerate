# Accelerate Scratchpad

I was having trouble getting [accelerate][accelerate] projects to build from
Hackage, so this is a cabal project which pins working versions of all the
dependencies necessary to...

* run on the CPU with [Accelerate LLVM Native][accelerate-llvm-native]
* run on the GPU with [CUDA][accelerate-llvm-ptx]

[accelerate-llvm-native]: https://github.com/AccelerateHS/accelerate-llvm/tree/master/accelerate-llvm-native
[accelerate-llvm-ptx]: https://github.com/AccelerateHS/accelerate-llvm/tree/master/accelerate-llvm-ptx

`Main.hs` shows how to run the `dotp` example from `Data.Array.Accelerate` on
the CPU and GPU.

# Arch Linux

I built this on arch, and needed two non-haskell dependencies:

* [llvm9](https://www.archlinux.org/packages/extra/x86_64/llvm9/)
* [cuda](https://www.archlinux.org/packages/community/x86_64/cuda/)
