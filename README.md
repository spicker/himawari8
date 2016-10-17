# himawari8

![](https://i.imgur.com/xYdadCV.jpg)

A small program to download pictures taken by the Himawari-8 satellite.
Useful for setting your desktop background to current images.

This is a small personal project to learn Haskell. The program is usable but nowhere near perfect.

Pictures taken from [here](http://himawari8.nict.go.jp/).

### Installation

```bash
git clone https://github.com/spicker/himawari8.git
cd himawari8
stack install 
```

### Usage

```bash
himawari8
``` 

For more information use `himawari8 --help`.

Use `cron` to automatically execute the program every ten minutes.

**Warning:** Running this program removes all .png files from the output directory (for now).