# SIXEL Library for Haskell

SIXEL, short for "six pixels", is a bitmap graphics format supported by terminals and printers from DEC. 
It can show graphics in a terminal emulator.

[libsixel](https://saitoha.github.io/libsixel/) provides various demos using SIXEL.

This library is developed for displaying images on ghci.

# Usage

This library provides "Show-intances of Sixel-Commands" and "ToSixel type class to change image data into Sixel-Cmmands".
To render image data on ghci, just run 'toSixel image-data'.
'putImage' is a wrapper of "readImage 'image-file' >>= putStr.show.toSixel".

See following demo.

![demo](https://raw.githubusercontent.com/junjihashimoto/sixel/master/demo.png)


# References

* https://en.wikipedia.org/wiki/Sixel
* https://saitoha.github.io/libsixel

