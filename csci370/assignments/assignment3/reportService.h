#ifndef ASSIGNMENT_3_REPORT_SERVICE_H
#define ASSIGNMENT_3_REPORT_SERVICE_H

#include <occi.h>
#include <iostream>

class ReportService {
public:
    ReportService();
    ~ReportService();

    void generateReport(std::string customerId);
    

    
    
private:
    std::string readPassword();

    std::string getUsername();

    std::string getPassword();
    
    bool isValidCid(oracle::occi::Connection* conn, std::string customerId);

    std::vector<std::string> getMostRecentPermit(oracle::occi::Connection* conn, std::string customerId);

    std::string getPlatesFromPermit(oracle::occi::Connection* conn, std::string permitId);

    void writeStringToFile(std::string writeString, std::string filePath);
};

#endif // ASSIGNMENT_3_REPORT_SERVICE_H