struct foo {
  signed char a : 3;
  signed char b : 3;
  signed char   : 0;  // Next field must be in next storage unit
  signed char c : 2;
};
