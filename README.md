chbspasswd
==========

## A password generator inspired by XKCD 936: Password Strength and xkpasswd.net

### Install
To use chbspasswd download and install the latest [release](https://github.com/solarturtle/chbspasswd/releases).

If you use Homebrew, installation is even easier:
> $ brew tap solarturtle/solarturtle  
> $ brew install chbspasswd  

### Learn

Next, you should read (or at least skim) the documentation:
> $ man chbspasswd  

The password generator is very customizable, you use command line flags to specify:

* the number of words to use 
* a seperator to be used between words 
* a pad to be placed before, inside between words, and/or after the words 
* a minimum and maximum word length 
* the case to use within the words 
* a count of passwords to generate 
* and a couple of other options 

The default is 4 words:
> $ chbspasswd  
> ThirdNounForestSwim  

Now lets add some options:

> $ chbspasswd -w 3 -b d,1 -a s,2  
> 9LearnRemainDon*$  

To seperate the words with a dot:

> $ chbspasswd -w 4 -s .,1  
> West.Twenty.Cookies.Trade  

To seperate the words with random characters:

> $ chbspasswd -s r,1  
> Shoe-Toward=Jack+Water  

To generate several passwords at a time:

> $ chbspasswd -w 3 -b d,1 -a s,1 -n 5  
> 8ChairOrbitVictory&  
> 7EasyGradeCreature*  
> 1ServiceTownBeing*  
> 1LabelHistoryGermany?  
> 4CannotStuckOld$  

### Use

There are, of course, many other combinations that can be used to
customize the password to your preference.

If you catch a bug or have a feature request, file an
[issue](https://github.com/solarturtle/chbspasswd/issues).

---

> Copyright (C) 2013-2014, Charles H. Leggett
> 
> This program is free software: you can redistribute it and/or modify
> it under the terms of the GNU General Public License as published by
> the Free Software Foundation, either version 3 of the License, or
> (at your option) any later version.
> 
> This program is distributed in the hope that it will be useful,
> but WITHOUT ANY WARRANTY; without even the implied warranty of
> MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
> GNU General Public License for more details.
> 
> You should have received a copy of the GNU General Public License
> along with this program.  If not, see <http://www.gnu.org/licenses/>.
