
lab11x: node.o list.o tester.o
	g++ -Wall -Wextra list.o node.o tester.o -o lab11x

node.o: node.h node.cpp
	g++ -Wall -Wextra -c node.cpp

list.o: list.h node.h list.cpp
	g++ -g -Wall -Wextra -c list.cpp

tester.o: list.h node.h tester.cpp
	g++ -g -Wall -Wextra -c tester.cpp

