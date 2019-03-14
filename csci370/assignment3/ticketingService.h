#ifndef ASSIGNMENT_3_TICKETING_SERVICE_H
#define ASSIGNMENT_3_TICKETING_SERVICE_H

#include <occi.h>
#include <termios.h>
#include <unistd.h>
#include <iostream>

class TicketingService {
public:
    TicketingService();
    ~TicketingService();
    
    void runTicketer(std::string westParkId, std::string currentDate);
private:

    std::string readPassword();

    std::string getUsername();

    std::string getPassword();

    bool hasValidPermit(oracle::occi::Connection* conn, std::string plate, 
            std::string currentDate);

    void writeTicket(oracle::occi::Connection* conn, 
            std::string plate, std::string currentDate, std::string westParkId);
};

#endif // ASSIGNMENT_3_TICKETING_SERVICE_H