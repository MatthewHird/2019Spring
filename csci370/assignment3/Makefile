G = g++-4.9
CFLAGS = -I/usr/local/lib/Oracle/instantclient_11_2/sdk/include -std=c++11
LFLAGS = -L/usr/local/lib/Oracle/instantclient_11_2 -locci -lociei -std=c++11


all: reporting ticketing

reporting: reportMain.o reportService.o
	$(G) $(LFLAGS) -o reporting reportMain.o reportService.o

ticketing: ticketingMain.o ticketingService.o
	$(G) $(LFLAGS) -o ticketing ticketingMain.o ticketingService.o

reportMain.o: reportMain.cpp reportService.h
	$(G) -c $(CFLAGS) reportMain.cpp

reportService.o: reportService.cpp reportService.h
	$(G) -c $(CFLAGS) reportService.cpp

ticketingMain.o: ticketingMain.cpp ticketingService.h
	$(G) -c $(CFLAGS) ticketingMain.cpp

ticketingService.o: ticketingService.cpp ticketingService.h
	$(G) -c $(CFLAGS) ticketingService.cpp

clean:
	rm -f *.o

cleanAll:
	rm -f *.o reporting ticketing