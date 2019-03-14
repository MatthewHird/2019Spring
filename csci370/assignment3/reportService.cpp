#include "reportService.h"
#include <iostream>
#include <occi.h>
#include <termios.h>
#include <unistd.h>
#include <vector>
#include <fstream>
#include <sstream>


ReportService::ReportService() {}

ReportService::~ReportService() {}


void ReportService::generateReport(std::string customerId) {
    const std::string connectString = "sunfire.csci.viu.ca";

    try {
        // establish database connection
        std::string username = getUsername();
        std::string password = getPassword(); 

        oracle::occi::Environment* env = oracle::occi::Environment::createEnvironment();
        oracle::occi::Connection* conn = env->createConnection(
                username, password, connectString);

        if (isValidCid(conn, customerId)) {
            std::vector<std::string> permitData = getMostRecentPermit(conn, customerId);
            
            if (permitData.empty()) {
                std::cout << "Customer with ID " << customerId << " has never owned a permit\n";
            } else {
                std::string platesString = getPlatesFromPermit(conn, permitData.at(2));

                std::string reportString = "PURCHASE/REGISTRATION HISTORY REPORT\n\nCUSTOMER ID: " 
                    + permitData.at(0) + "\nCUSTOMER NAME: " + permitData.at(1) 
                    + "\n\nMOST RECENT PERMIT\nPID             START TIME      EXPIRE TIME     PURCHASE TIME" 
                    + "\n----------      ----------      -----------     -------------\n" 
                    + permitData.at(2) + "      " + permitData.at(3) + "      " + permitData.at(4) + "      " 
                    + permitData.at(5) + "\n\nREGISTERED VEHICLE(S)\n" + platesString;

                std::string reportName = "Report-" + permitData.at(0) + ".txt";
                writeStringToFile(reportString, reportName);
            }
        } else {
            std::cout << "Invalid customer ID: " << customerId << "\n";
        }
        
        
        env->terminateConnection(conn);
        oracle::occi::Environment::terminateEnvironment(env);
    } catch (oracle::occi::SQLException &e) {
        std::cout << e.what();
        return;
    }
}


std::string ReportService::readPassword() {
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


std::string ReportService::getUsername() {
    std::string username;
    std::cout << "Your username: ";
    std::getline(std::cin, username);
    return username;
}


std::string ReportService::getPassword() {
    std::cout << "Your password: ";
    std::string password = readPassword();
    std::cout << "\n";
    return password;
}


bool ReportService::isValidCid(oracle::occi::Connection* conn, std::string customerId) {

    std::string queryStr = "";
    queryStr = queryStr 
        + "SELECT cid "
        + "FROM Customers "
        + "WHERE cid = :1";
    
    oracle::occi::Statement *stmt = conn->createStatement(queryStr);
    stmt->setString(1, customerId);

    oracle::occi::ResultSet* resultSet = stmt->executeQuery();

    bool validCid = false;
    if (resultSet->next()) {
        validCid = true;
    }

    stmt->closeResultSet(resultSet);
    conn->terminateStatement(stmt);

    return validCid;
}

std::vector<std::string> ReportService::getMostRecentPermit(oracle::occi::Connection* conn, std::string customerId) {
    std::vector<std::string> retVec;

    std::string queryStr = "";
    queryStr = queryStr 
        + "SELECT cid, name, pid, to_char(startTime, 'yyyy-mm-dd'), to_char(expireTime, 'yyyy-mm-dd'), to_char(purchaseTime, 'yyyy-mm-dd') "
        + "FROM Customers "
        + "INNER JOIN Permits "
        + "ON cid = owner "
        + "WHERE cid = :1 "
        + "ORDER BY purchaseTime, expireTime, startTime DESC ";
        //+ "fetch first row only";

    oracle::occi::Statement *stmt = conn->createStatement(queryStr);
    stmt->setString(1, customerId);

    oracle::occi::ResultSet* resultSet = stmt->executeQuery();

    if (resultSet->next()) {
        retVec.emplace_back(resultSet->getString(1));
        retVec.emplace_back(resultSet->getString(2));
        retVec.emplace_back(resultSet->getString(3));
        retVec.emplace_back(resultSet->getString(4));
        retVec.emplace_back(resultSet->getString(5));
        retVec.emplace_back(resultSet->getString(6));
    }

    stmt->closeResultSet(resultSet);
    conn->terminateStatement(stmt);

    return retVec;
}

std::string ReportService::getPlatesFromPermit(oracle::occi::Connection* conn, std::string permitId) {
    std::string retStr = "";
    std::string queryStr = "";
    queryStr = queryStr 
        + "SELECT v.plate "
        + "FROM Vehicles v "
        + "INNER JOIN Registration r "
        + "ON v.plate = r.plate "
        + "WHERE r.pid = :1";

    oracle::occi::Statement *stmt = conn->createStatement(queryStr);
    stmt->setString(1, permitId);

    oracle::occi::ResultSet* resultSet = stmt->executeQuery();

    while (resultSet->next()) {
        retStr += resultSet->getString(1) + "\n";
    }

    stmt->closeResultSet(resultSet);
    conn->terminateStatement(stmt);

    return retStr;
}


void ReportService::writeStringToFile(std::string writeString, std::string filePath) {
    std::stringstream stringStream;
    stringStream << writeString;
    
    std::string saveString = stringStream.str();

    std::ofstream outFile;
    outFile.open(filePath.c_str());
    
    if (outFile.fail()) {
        std::cout << "Failed to open " << filePath << " aborting program without saving\n";
        outFile.open("batch_queue_backup.txt");
    } else {
        outFile << saveString;
        std::cout << "The report has been saved to " << filePath << "\n";
    }
}


