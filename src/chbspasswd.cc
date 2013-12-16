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
#include <string>
#include <GetOpt.h>

class CHBSPassword {

  public:
    CHBSPassword();
    std::string getPassword();
    void setWordCount();
    void setWordLength();
    void setWordCase();
    void setSeperator();
    void setBefore();
    void setAfter();
    void setInside();
    void enableEleet();

  private:
    std::string  validSpecialCharacters;
    std::string  validSeperators;

    std::string  getWord();
    int          wordCount;
    int          wordMinLength;
    int          wordMaxLength;

    enum class   caseType {UPPER, LOWER, INITIAL, MIXED};

    std::string  getSeperator();
    bool         separatorEnabled;
    std::string  separatorCharacterCount;
    std::string  separatorSaved;
    enum class   separatorType {SAME, MIXED};

    std::string  getBefore();
    bool         beforeEnabled;
    enum class   beforeType {DIGITS, SPECIAL, MIXED};

    std::string  getInside();
    bool         insideEnabled;
    enum class   insideType {DIGITS, SPECIAL, MIXED};

    std::string  getAfter();
    bool         afterEnabled;
    enum class   afterType {DIGITS, SPECIAL, MIXED};

};

int showHelp();
int showVersion();
int showChbs();

int main(int argc, char **argv) {

  CHBSPassword thisPassword;

  int flag;

  while ((flag = getopt(argc, argv, "a:b:c:e:hi:l:n:s:u:vw:x")) != EOF) {
    switch (flag) {
      case 'a':
        // after - Add string of digits, special characters, or a combination to the end of the password.
        std::cout << optarg << std::endl;
        thisPassword.setAfter();
        break;
      case 'b':
        // before - Add string of digits, special characters, or a combination to the beginning of the password.
        thisPassword.setBefore();
        break;
      case 'c':
        // case - Modify the words to be upper, lower, initial, or mixed case 
        thisPassword.setWordCase();
        break;
      case 'e':
        // eleet - Make 1337sp3@k substitutions: a=@, e=3, i=!, l=1, o=0, and t=7.
        thisPassword.enableEleet();
        break;
      case 'h':
        // help - Display a message with usage information 
        showHelp();
        break;
      case 'i':
        // inside - Add string of digits, special characters, or a combination between the words inside the password.
        thisPassword.setInside();
        break;
      case 'l':
        // length - Set minimum and maximum word length.
        thisPassword.setWordLength();
        break;
      case 'n':
        // number - Set number of passwords to create.
        break;
      case 's':
        // seperator - Set the preferences and count of seperator characters between words and other strings.
        thisPassword.setSeperator();
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
        thisPassword.setWordCount();
        break;
      case 'x':
        // xkcd - Override all other options and return a well known password that you have already memorized. ;-)
        showChbs();
        break;
      case '?':
        break;
    }
  }

  return 0;
}

CHBSPassword::CHBSPassword() {

  wordCount = 3;
  separatorEnabled = true;
  // figure out how to use the separatorType enum. 
  separatorCharacterCount = 1;

}

std::string CHBSPassword::getPassword(){
  // Build and return password based on the defaults in the configuration file and modifying switches.
}

void CHBSPassword::setWordCount(){
  // words - Set the number of words to use.

}

void CHBSPassword::setWordLength(){
  // length - Set minimum and maximum word length.

}

void CHBSPassword::setWordCase(){
  // case - Modify the words to be upper, lower, initial, or mixed case 

}

void CHBSPassword::setSeperator(){
  // seperator - Set the preferences and count of seperator characters between words and other strings.

}

void CHBSPassword::setBefore(){
  // before - Add string of digits, special characters, or a combination to the beginning of the password.

}

void CHBSPassword::setAfter(){
  // after - Add string of digits, special characters, or a combination to the end of the password.

}

void CHBSPassword::setInside(){
  // inside - Add string of digits, special characters, or a combination between the words inside the password.

}

void CHBSPassword::enableEleet(){
  // eleet - Make 1337sp3@k substitutions: a=@, e=3, i=!, l=1, o=0, and t=7.

}

int showHelp() {
  // help - Display a message with usage information 

  std::cout << "Here is a helpful message." << std::endl;

  return 0;
}

int showVersion() {
  // version - Display a message with version information 

  std::cout << "chbspassed v0.1" << std::endl;

  return 0;
}

int showChbs() {
  // xkcd - Override all other options and return a well known password that you have already memorized. ;-)

  std::cout << "CorrectHorseBatteryStaple" << std::endl;

  return 0;
}

