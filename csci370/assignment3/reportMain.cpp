#include <iostream>
#include "reportService.h"

int main(int argc, char const *argv[])
{
    if (argc != 2) {
        std::cout << "Error: Invalid number of arguments\n";
        return 1;
    }

    ReportService reportService;
        reportService.generateReport(argv[1]);   
    
    return 0;
}