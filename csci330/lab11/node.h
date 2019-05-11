#ifndef NODE_H
#define NODE_H

#include <string>
#include <tr1/memory>

class node {
   public:
      node();
      ~node();
      void setnext(std::tr1::shared_ptr<node> n);
      void setdata(std::string k, std::string v);
      std::tr1::shared_ptr<node> getnext();
      void getdata(std::string &k, std::string &v);
   protected:
      std::string key, value;
      std::tr1::shared_ptr<node> next;
};

typedef std::tr1::shared_ptr<node> nodePtr;
typedef std::tr1::weak_ptr<node> wnodePtr;

#endif
