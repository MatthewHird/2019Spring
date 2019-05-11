#include "list.h"
#include <iostream>

void print(wnodePtr p) {
    if (auto observe = p.lock())
    {
        std::string k;
        std::string v;
        observe->getdata(k, v);

        std::cout << "key: " << k << " => value: " << v << "\n\n";
    }

}


void insert(nodePtr &front, std::string k, std::string v) {
    nodePtr newNode;
    newNode.reset(new node());
    newNode->setdata(k, v);
    if (front != nullptr) {
        newNode->setnext(front);
    }
    front = newNode;
}


void remove(nodePtr &front, std::string &k, std::string &v) {
    if (front != nullptr) {
        nodePtr temp = front;
        front->getdata(k, v);
        front = front->getnext();
        temp.reset();
    }
}


void deallocate(nodePtr &front) {
    while (front != nullptr) {
        std::string k;
        std::string v;
        nodePtr temp = front;
        front->getdata(k, v);
        std::cout << "key: " << k << " => value: " << v << "\n\n";
        front = front->getnext();
        temp.reset();
    }
}
