# ZDPlasKin with Cklib
プラズマの反応と燃焼の反応を同時に解くためのコード．ZDPlasKinのメインルーチンの中でCHEMKINパッケージのCklibのサブルーチンを呼ぶ

### 元ファイル
`example1` from ZDPlasKin Web page

### 実行方法 (Linux)
```
$ export LD_LIBRARY_PATH=.:$LD_LIBRARY_PATH
$ gfortran dvode_f90_m.F90 zdplaskin_m.F90 driver.F90 bolsig_x86_64.so
$ ./a.out
```