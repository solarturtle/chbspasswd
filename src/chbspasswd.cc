// chbspasswd v0.1
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

#include <iostream>
#include <sstream>
#include <string>
#include <vector>
#include <GetOpt.h>

class CHBSPassword {

  public:

    CHBSPassword();
    
    std::string  getPassword();

    std::string  validSpecialCharacters;
    std::string  validSeparators;

    std::string  getWord();
    int          wordCount;
    int          wordMinimumLength;
    int          wordMaximumLength;

    std::string  wordCase;

    std::string  getSeparator();
    std::string  separatorType;
    int          separatorCount;

    std::string  getPadString ( std::string, std::string );
    int          padMaxLength;

    std::string  getBeforeString();
    std::string  beforeType;
    int          beforeCount;

    std::string  getAfterString();
    std::string  afterType;
    int          afterCount;

    std::string  getInsideString();
    std::string  insideType;
    int          insideCount;

    bool         eleetEnabled;

    static bool  isValidPadType ( std::string );
    static bool  isValidPadCount ( int );
    
};

int showHelp();
int showVersion();
int showCHBS();

std::vector<std::string> tokenize ( std::string, char );

int main(int argc, char **argv) {

  int passwordCount;
  CHBSPassword thisPassword;

  int flag;
  std::vector<std::string> arguments;

  while ((flag = getopt(argc, argv, "a:b:c:e:hi:l:n:s:u:vw:x")) != EOF) {

    if ( flag == 'a' ) {

      // after - Add string of digits, special characters, or a combination to the end of the password.

      arguments = tokenize ( optarg, ',' );

      thisPassword.afterType = arguments[0];
      thisPassword.afterCount = atoi(arguments[1].c_str());

      return 0;

    }

    else if ( flag == 'b' ) {

      // before - Add string of digits, special characters, or a combination to the beginning of the password.

      arguments = tokenize ( optarg, ',' );

      thisPassword.beforeType = arguments[0];
      thisPassword.beforeCount = atoi(arguments[1].c_str());

      return 0;

    }

    else if ( flag == 'c' ) {

      // case - Modify the words to be upper, lower, initial, or mixed case 

      std::string caseType = optarg;

      thisPassword.wordCase = caseType;

      return 0;

    }
    
    else if ( flag == 'e' ) {

      // eleet - Make 1337sp3@k substitutions: a=@, e=3, i=!, l=1, o=0, and t=7.

      thisPassword.eleetEnabled = true;

      return 0;

    }

    else if ( flag == 'h' ) {

      // help - Display a message with usage information 

      showHelp();

      return 0;

    }

    else if ( flag == 'i' ) {

      // inside - Add string of digits, special characters, or a combination between the words inside the password.

      arguments = tokenize ( optarg, ',' );

      thisPassword.insideType = arguments[0];
      thisPassword.insideCount = atoi(arguments[1].c_str());

      return 0;

    }

    else if ( flag == 'l' ) {

      // length - Set minimum and maximum word length.

      arguments = tokenize ( optarg, ',' );

      thisPassword.wordMinimumLength = atoi(arguments[0].c_str());
      thisPassword.wordMinimumLength = atoi(arguments[1].c_str());

      return 0;

    }

    else if ( flag == 'n' ) {

      // number - Set number of passwords to create.

      std::string count = optarg;

      passwordCount = atoi(count.c_str());

      return 0;

    }

    else if ( flag == 's' ) {

      // seperator - Set the preferences and count of seperator characters between words and other strings.

      arguments = tokenize ( optarg, ',' );

      thisPassword.separatorType = arguments[0];
      thisPassword.separatorCount = atoi(arguments[1].c_str());

      return 0;

    }

    else if ( flag == 'u' ) {

      // use - Specify a configuration file to use instead of the default .chbspasswdrc

      return 0;

    }

    else if ( flag == 'v' ) {

      // version - Display a message with version information 

      showVersion();

      return 0;

    }

    else if ( flag == 'w' ) {

      // words - Set the number of words to use.

      std::string count = optarg;

      thisPassword.wordCount = atoi(count.c_str());

      return 0;
    }

    else if ( flag == 'x' ) {

      // xkcd - Override all other options and return a well known password that you have already memorized. ;-)

      showCHBS();

      return 0;

    }

  }

  thisPassword.getPassword();
}

CHBSPassword::CHBSPassword() {

  wordCount = 3;
  //separatorEnabled = true;
  //separatorCharacterCount = 1;
  padMaxLength = 5;

}

std::string CHBSPassword::getPassword(){

  // Build and return password based on the defaults in the configuration file and modifying switches.

}

static bool isValidPadType ( std::string padType ) {

  //std::transform(padType.begin(), padType.end(), padType.begin(), ::tolower);

  if ( padType == "d" || padType == "digits") {
    // Using Pad of Digits
    std::cout << "DIGITS" << std::endl; // debug message
    
    return 1;
  }
  else if ( padType == "s" || padType == "special") {
    // Using Pad of Special Characters
    std::cout << "SPECIAL" << std::endl; // debug message
    
    return true;
  }
  else if ( padType == "m" || padType == "mixed") {
    // Using Pad of Mixed Digits and Special Characters
    std::cout << "MIXED" << std::endl; // debug message
    
    return true;
  }
  else {
    // invalid argument
    std::cout << "./chbspasswd: unexpected argument -- " << padType << std::endl;
    showHelp();

    return false;
  }

  std::cout << "HERE" << std::endl; // debug message
}

bool isValidPadCount ( int padCount, int padMaxLength ) {

  if ( padCount <= padMaxLength ) {
    return true;
  }
  else {
    return false;
  }

}

std::string CHBSPassword::getBeforeString() {

  // Return string for ...
  // before - Add string of digits, special characters, or a combination to the beginning of the password.
  
}

std::string CHBSPassword::getAfterString() {

  // Return string for ...
  // after - Add string of digits, special characters, or a combination to the end of the password.

}

std::string CHBSPassword::getInsideString() {

  // Return string for ...
  // inside - Add string of digits, special characters, or a combination between the words inside the password.

}

std::string CHBSPassword::getPadString ( std::string padType, std::string count ){

  // Called by get{Before,After,Inside} to use variables to build and return string.
  
}

int showHelp() {

  // help - Display a message with usage information 

  std::cout << std::endl;
  std::cout << "Usage: chbspasswd [ -w numberOfWords ]" << std::endl;
  std::cout << "                  [ -l minimumWordLength,maximumWordLength ]" << std::endl;
  std::cout << "                  [ -c {UPPER|LOWER|INITIAL|MIXED} ]" << std::endl;
  std::cout << "                  [ -s typeOfSeparator,count ]" << std::endl;
  std::cout << "                  [ -{a|b|c} {DIGITS|SPECIAL|MIXED},count ]" << std::endl;
  std::cout << "                  [ -n numberOfPasswordsToGenerate ]" << std::endl;
  std::cout << std::endl;
  std::cout << "For more detailed information: man chbspasswd" << std::endl;
  std::cout << std::endl;

  return 0;
  
}

int showVersion() {

  // version - Display a message with version information 

  std::cout << "chbspasswd v0.1" << std::endl;

  return 0;

}

int showCHBS() {

  // xkcd - Override all other options and return a well known password that you have already memorized. ;-)

  std::cout << "CorrectHorseBatteryStaple" << std::endl;

  return 0;

}

std::vector<std::string> tokenize(std::string delimiterSeparatedString, char delimiter) {

  // Accepts a delimiter separated string and the specified delimiter.
  
  std::istringstream iss(delimiterSeparatedString);

  // Tokenizes the string and puts the tokens into a vector
  
  std::string token;
  std::vector<std::string> tokens;
  
  while (std::getline(iss, token, delimiter)) {
    tokens.push_back(token);
  }

  // Returns the vector.

  return tokens;

};

