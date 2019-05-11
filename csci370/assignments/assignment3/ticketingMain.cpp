#include <iostream>
#include "ticketingService.h"

int main(int argc, char const *argv[])
{   
    if (argc != 3) {
        std::cout << "Error: Invalid number of arguments\n";
        return 1;
    }

    TicketingService ticketingService;
    ticketingService.runTicketer(argv[1], argv[2]);
    return 0;
}