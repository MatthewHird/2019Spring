#include "ticketingService.h"


#include "reportService.h"
#include <iostream>
#include <occi.h>
#include <termios.h>
#include <unistd.h>
#include <vector>
#include <fstream>
#include <sstream>
#include <regex>


TicketingService::TicketingService() {}

TicketingService::~TicketingService() {}


void TicketingService::runTicketer(std::string westParkId, std::string currentDate) {
    const std::string connectString = "sunfire.csci.viu.ca";

    // max length 40 char
    // yyyy-mm-dd

    if (std::regex_search(westParkId, std::regex("^\\s*$"))) {
        std::cout << "Invalid West Park ID: ID must not be left blank\n";
        return;
    }

    if (westParkId.size() > 40) {
        std::cout << "Invalid West Park ID: ID must be 40 characters or less\n";
        return;   
    }

    std::cout << "\n**** Welcome to the WestPark Ticketing Service ****\n";

    try {
        std::string username = getUsername();
        std::string password = getPassword(); 

        oracle::occi::Environment* env = oracle::occi::Environment::createEnvironment();
        oracle::occi::Connection* conn = env->createConnection(
                username, password, connectString);
        
        std::string plate;

        while(true) {
            std::cout << "Please enter a license plate #\n>>>  ";

            std::getline(std::cin, plate);

            if (plate == "000000") {
                std::cout << "\nExit code entered ... exiting ticketing service \n\n";
                break;
            }

            if(hasValidPermit(conn, plate, currentDate)) {
                std::cout << "Plate# " << plate << " is registered to a valid permit\n\n";
            } else {
                std::cout << "No valid permit: Plate# " << plate << " has been issued a ticket\n\n";
                writeTicket(conn, plate, currentDate, westParkId);
            }
        }
        
        env->terminateConnection(conn);
        oracle::occi::Environment::terminateEnvironment(env);
    } catch (oracle::occi::SQLException &e) {
        std::cout << e.what();
        return;
    }
}


std::string TicketingService::readPassword() {
    struct termios settings;
    
    tcgetattr( STDIN_FILENO, &settings );
    settings.c_lflag =  (settings.c_lflag & ~(ECHO));
    tcsetattr( STDIN_FILENO, TCSANOW, &settings );

    std::string password = "";
    std::getline(std::cin, password);

    settings.c_lflag = (settings.c_lflag |   ECHO );
    tcsetattr(STDIN_FILENO, TCSANOW, &settings );
    
    return password;
}


std::string TicketingService::getUsername() {
    std::string username;
    std::cout << "Your username: ";
    std::getline(std::cin, username);
    return username;
}


std::string TicketingService::getPassword() {
    std::cout << "Your password: ";
    std::string password = readPassword();
    std::cout << "\n\n";
    return password;
}


bool TicketingService::hasValidPermit(oracle::occi::Connection* conn, std::string plate, std::string currentDate) {

    std::string queryStr = "";
    queryStr = queryStr 
        + "SELECT DISTINCT r.plate "
        + "FROM Registration r "
        + "INNER JOIN Permits p "
        + "ON r.pid = p.pid "
        + "WHERE r.plate = :1 "
        + "AND p.expiretime >= to_timestamp(:2, 'yyyy-mm-dd') "
        + "AND p.starttime <= to_timestamp(:3, 'yyyy-mm-dd')";
    
    oracle::occi::Statement *stmt = conn->createStatement(queryStr);
    stmt->setString(1, plate);
    stmt->setString(2, currentDate);
    stmt->setString(3, currentDate);

    oracle::occi::ResultSet* resultSet = stmt->executeQuery();

    bool validPermit = false;
    if (resultSet->next()) {
        validPermit = true;
    }

    stmt->closeResultSet(resultSet);
    conn->terminateStatement(stmt);

    return validPermit;
}


void TicketingService::writeTicket(
        oracle::occi::Connection* conn, std::string plate, std::string currentDate, std::string westParkId) {
    
    std::string queryStr = "";
    queryStr = queryStr 
        + "INSERT INTO Tickets (tickettime, violation, fine, plate, issuer, tid) "
        + "SELECT to_timestamp(:1, 'yyyy-mm-dd'), 'Parking without permit', 45.00, :2, :3, 1 + MAX(tid) "
        + "FROM Tickets";

    oracle::occi::Statement *stmt = conn->createStatement(queryStr);
    stmt->setString(1, currentDate);
    stmt->setString(2, plate);
    stmt->setString(3, westParkId);

    stmt->executeQuery();

    conn->terminateStatement(stmt);

    return;
}


