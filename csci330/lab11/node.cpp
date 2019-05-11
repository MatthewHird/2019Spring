#include "node.h"

node::node()
{
    key = "";
    value = "";
}

node::~node()
{
    next.reset();
}

void node::setnext(nodePtr n)
{
   next = n;
}

void node::setdata(std::string k, std::string v)
{
   key = k;
   value = v;
}

nodePtr node::getnext()
{
   return next;
}

void node::getdata(std::string &k, std::string &v)
{
   k = key;
   v = value;
}

