// chbspasswd v0.2 - main.cpp
//
// A password generator inspired by XKCD 936: Password Strength and xkpasswd.net
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

#include "chbspasswd.hpp"

#include <iostream>
#include <sstream>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>

void showHelp();
void showVersion();
void showCHBS();

std::vector<std::string> tokenize ( std::string, char );

int main ( int argc, char **argv ) {

  // Enable or disable debugging called later with showDEBUG();
  bool DEBUG = false;

  int passwordCount = 1;
  CHBSPassword thisPassword;

  // Setup the dictionary to be used later.
  // Also sets min and max word length variables.
  thisPassword.buildDictionary();

  // Use GNU getopts to accept command line arguments.
  int flag;
  std::vector<std::string> arguments;

  while ( ( flag = getopt ( argc, argv, "a:b:c:hi:l:n:s:u:vw:x" ) ) != EOF ) {

    if ( flag == 'a' ) {

      // after - Add string of digits, special characters, or a combination to
      // the end of the password.

      // Split command line arguments on comma delimiter.
      arguments = tokenize ( optarg, ',' );

      // Set pad variables
      thisPassword.setPad ( "after", arguments[0], arguments[1] );

    }

    else if ( flag == 'b' ) {

      // before - Add string of digits, special characters, or a combination to
      // the beginning of the password.

      // Split command line arguments on comma delimiter.
      arguments = tokenize ( optarg, ',' );

      // Set pad variables
      thisPassword.setPad ( "before", arguments[0], arguments[1] );

    }

    else if ( flag == 'c' ) {

      // case - Modify the words to be upper, lower, initial, or mixed case

      thisPassword.setWordCase ( optarg );

    }

// Planned implementation in v0.2
//    else if ( flag == 'e' ) {
//
//      // eleet - Make 1337sp3@k substitutions: a=@, e=3, i=!, l=1, o=0, and t=7.
//
//      // thisPassword.eleetEnabled = true;
//
//    }

    else if ( flag == 'h' ) {

      // help - Display a message with usage information

      showHelp();

      return 0;

    }

    else if ( flag == 'i' ) {

      // inside - Add string of digits, special characters, or a combination
      // between the words inside the password.

      // Split command line arguments on comma delimiter.
      arguments = tokenize ( optarg, ',' );

      // Set pad variables
      thisPassword.setPad ( "inside", arguments[0], arguments[1] );

    }

    else if ( flag == 'l' ) {

      // length - Set minimum and maximum word length.

      // Split command line arguments on comma delimiter.
      arguments = tokenize ( optarg, ',' );

      // Set word length variables
      thisPassword.setWordLength ( arguments[0], arguments[1] );

    }

    else if ( flag == 'n' ) {

      // number - Set number of passwords to create.

      // This argument implemented here in main instead of within CHBSPasswd
      // class function.
      std::string count = optarg;

      passwordCount = atoi(count.c_str());

      if ( passwordCount <= 0 ) {

        std::cout << "./chbspasswd: unexpected argument \"" << optarg << "\" for option -- n" << std::endl;
        std::cout << "./chbspasswd: argument must be a number with a value greater than or equal to 1" << std::endl;
        std::cout << std::endl;

        return -1;
      }

    }

    else if ( flag == 's' ) {

      // seperator - Set the preferences and count of seperator characters
      // between words.

      // Split command line arguments on comma delimiter.
      arguments = tokenize ( optarg, ',' );

      // Set separator variables
      thisPassword.setSeparator ( arguments[0], arguments[1] );
    }

// Planned implementation in v0.2
//    else if ( flag == 'u' ) {
//
//      // use - Specify a configuration file to use instead of the default
//      // .chbspasswdrc
//
//    }

    else if ( flag == 'v' ) {

      // version - Display a message with version information

      showVersion();

      return 0;

    }

    else if ( flag == 'w' ) {

      // words - Set the number of words to use.

      thisPassword.setWordCount ( optarg );

    }

    else if ( flag == 'x' ) {

      // xkcd - Override all other options and return a well known password
      // that you have already memorized. ;-)

      showCHBS();

      return 0;

    }

    else {

      // help - Display the usage information.

      showHelp();

      return -1;

    }

  }

  if ( DEBUG ) {

    // DEBUG - Show debug information if enabled at top of main.

    thisPassword.showDEBUG();
    std::cout << "passwordCount: " << passwordCount << std::endl;
    std::cout << std::endl;

  }

  // Generate requested number of passwords.

  for ( int i = 1; i <= passwordCount; i++ ) {
    std::cout << thisPassword.getPassword() << std::endl;
  }

}

void showVersion() {

  // version - Display a message with version information

  std::cout << "chbspasswd v0.2" << std::endl;

}

void showHelp() {

  // help - Display a message with usage information

  std::cout << std::endl;
  std::cout << "Usage: chbspasswd [ -w numberOfWords ]" << std::endl;
  std::cout << "                  [ -l minimumWordLength,maximumWordLength ]" << std::endl;
  std::cout << "                  [ -c {UPPER|LOWER|INITIAL|MIXED} ]" << std::endl;
  std::cout << "                  [ -s {SAME|RANDOM|validSeparator},numberOfCharacters ]" << std::endl;
  std::cout << "                  [ -{a|b|i} {DIGITS|SPECIAL|MIXED},numberOfCharacters ]" << std::endl;
  std::cout << "                  [ -n numberOfPasswordsToGenerate ]" << std::endl;
  std::cout << std::endl;
  std::cout << "For more detailed information: man chbspasswd" << std::endl;
  std::cout << std::endl;

}

void showCHBS() {

  // xkcd - Override all other options and return a well
  // known password that you have already memorized. ;-)

  std::cout << "CorrectHorseBatteryStaple" << std::endl;

}

std::vector<std::string> tokenize ( std::string delimiterSeparatedString, char delimiter ) {

  // Accepts a delimiter separated string and the
  // specified delimiter.

  std::istringstream iss ( delimiterSeparatedString );

  // Tokenizes the string and puts the tokens into a
  // vector

  std::string token;
  std::vector<std::string> tokens;

  while ( std::getline ( iss, token, delimiter ) ) {
    tokens.push_back(token);
  }

  // Returns the vector.
  return tokens;

}

