// chbspasswd v0.1.2 - chbspasswd.hpp
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
#include <vector>

class CHBSPassword {

  public:

    CHBSPassword();

    int          showDEBUG();

    void         buildDictionary();

    std::string  getPassword();

    void         setWordCount ( std::string CountString );
    void         setWordCase ( std::string Type );
    void         setWordLength ( std::string minimumString, std::string maximumString );

    void         setSeparator( std::string Type, std::string CountString );

    void         setPad ( std::string Position, std::string Type, std::string CountString );

  private:

    std::vector<std::string> words;

    std::string  validPadDigits;
    std::string  validPadSpecialCharacters;
    std::string  validSeparators;

    int          validWordMinimumLength;
    int          validWordMaximumLength;

    std::string  separatorSaved;

    bool         padDefaultsOverridden;

    int          wordCount;
    int          wordMinimumLength;
    int          wordMaximumLength;
    std::string  wordCase;

    std::string  getWord();
    std::string  applyWordCase ( std::string Word );

    bool         eleetEnabled;
    std::string  applyEleet ( std::string Word );

    bool         separatorEnabled;
    std::string  separatorType;
    int          separatorCount;

    std::string  getSeparator();

    std::string  getPad ( std::string Position );

    bool         beforeEnabled;
    std::string  beforeType;
    int          beforeCount;

    std::string  getBefore();

    bool         insideEnabled;
    std::string  insideType;
    int          insideCount;

    std::string  getInside();

    bool         afterEnabled;
    std::string  afterType;
    int          afterCount;

    std::string  getAfter();

    bool         isValidWordCount ( int wordCount );
    bool         isValidWordCase ( std::string caseType );

    bool         isValidSeparatorType ( std::string separatorType );
    bool         isValidSeparatorCount ( int separatorCount );

    bool         isValidPadType ( std::string padType );
    bool         isValidPadCount ( int padCount );

    std::string  convertType ( std::string Type );
    int          convertNumber ( std::string numberString );

};

