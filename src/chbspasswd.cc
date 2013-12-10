// chbspasswd
// 
// A password generator inspired by XKCD 936: Password Strength
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

int main()
{
  std::string password;

  password = "CorrectHorseBatteryStaple";

  std::cout << password;
  std::cout << std::endl;

  return 0;
}
