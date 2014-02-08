chbspasswd
==========

A password generator inspired by XKCD 936: Password Strength and xkpasswd.net

To use chbspasswd download and install the latest [release](https://github.com/solarturtle/chbspasswd/releases).

If you use Homebrew, installation is even easier:
> brew tap solarturtle/solarturtle
> brew install chbspasswd

Next, you should read (or at least skim) the documentation:
> man chbspasswd

The password generator is very customizable, you use command line flags to specify:
* the number of words to use 
* a seperator to be used between words
* a pad to be placed before, inside between words, and/or after the words
* a minimum and maximum word length
* the case to use within the words
* a count of passwords to generate
* and a couple of other options

Copyright (C) 2013-2014, Charles H. Leggett

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
