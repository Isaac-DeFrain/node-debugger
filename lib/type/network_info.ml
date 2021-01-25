open Basic

type t = {
  mutable active  : chain -> id list array;
  mutable blocks  : chain -> branch -> blocks;
  mutable branch  : chain -> branch option;
  mutable chain   : chain;
  mutable height  : chain -> branch -> int option;
  mutable sent    : chain -> node -> messages option;
  mutable sysmsgs : chain -> messages;
}