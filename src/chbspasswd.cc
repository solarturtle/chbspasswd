// chbspasswd v0.1
// 
// A password generator inspired by XKCD 936: Password Strength and
// xkpasswd by Bart Busschots
// 
// Copyright (C) 2013-2014, Charles H. Leggett
// 
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

#include <iostream>
#include <GetOpt.h>

int showHelp();
int showVersion();
int chbs();

int main(int argc, char **argv) {

  int flag;

  while ((flag = getopt(argc, argv, "a:b:c:e:hi:l:n:s:u:vw:x")) != EOF) {
    switch (flag) {
      case 'a':
        // after - Add string of digits, special characters, or a combination to the end of the password.
        std::cout << optarg << std::endl;
        break;
      case 'b':
        // before - Add string of digits, special characters, or a combination to the beginning of the password.
        break;
      case 'c':
        // case - Modify the words to be upper, lower, initial, or mixed case 
        break;
      case 'e':
        // eleet - Make 1337sp3@k substitutions: a=@, e=3, i=!, l=1, o=0, and t=7.
        break;
      case 'h':
        // help - Display a message with usage information 
        showHelp();
        break;
      case 'i':
        // inside - Add string of digits, special characters, or a combination between the words inside the password.
        break;
      case 'l':
        // length - Set minimum and maximum word length.
        break;
      case 'n':
        // number - Set number of passwords to create.
        break;
      case 's':
        // seperator - Set the preferences and count of seperator characters between words and other strings.
        break;
      case 'u':
        // use - Specify a configuration file to use instead of the default .chbspasswdrc
        break;
      case 'v':
        // version - Display a message with version information 
        showVersion();
        break;
      case 'w':
        // words - Set the number of words to use.
        break;
      case 'x':
        // xkcd - Override all other options and return a well known password that you already have memorized. ;-)
        chbs();
        break;
      case '?':
        break;
    }
  }

  return 0;
}

int showHelp() {

  std::cout << "Here is a helpful message." << std::endl;

  return 0;
}

int showVersion() {

  std::cout << "chbspassed v0.1" << std::endl;

  return 0;
}

int chbs() {

  std::cout << "CorrectHorseBatteryStaple" << std::endl;

  return 0;
}
