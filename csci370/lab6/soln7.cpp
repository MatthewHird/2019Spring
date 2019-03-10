#include <iostream>
#include <iomanip>
#include <string.h>
#include <occi.h>
#include <termios.h>
#include <unistd.h>
using namespace std;
using namespace oracle::occi;

// read database password from user input
// without showing the password on the screen
string readPassword()
{
    struct termios settings;
    tcgetattr( STDIN_FILENO, &settings );
    settings.c_lflag =  (settings.c_lflag & ~(ECHO));
    tcsetattr( STDIN_FILENO, TCSANOW, &settings );

    string password = "";
    getline(cin, password);

    settings.c_lflag = (settings.c_lflag |   ECHO );
    tcsetattr( STDIN_FILENO, TCSANOW, &settings );
    return password;
}

string desensitize(char *s)
{
    string ds = "";
    for(int i = 0; i < strlen(s); i++) {
        if (s[i] == '\'') {
            ds = ds + "\\";
        }
        ds = ds + s[i];
    }
    return ds;
}

int main(int argc, char *argv[])
{
    string userName;
    string password;
    // address of the Oracle server
    const string connectString = "sunfire.csci.viu.ca";

    if (argc <= 1) {
        cout << "usage: " << argv[0] << " Department Name" << endl;
        return 0;
    }

    cout << "Your user name: ";
    getline(cin, userName);

    cout << "Your password: ";
    password = readPassword();
    cout << endl;
    try {
        // establish database connection
        Environment *env = Environment::createEnvironment();
        Connection *conn = env->createConnection
                          (userName, password, connectString);

        // form a query string using literal string
        // and user input
        string queryStr = "";
        queryStr = queryStr + "select employee_id, first_name || ' ' || last_name "
                            + "as full_name, phone_number "
                            + "from hr.employees E, hr.departments D "
                            + "where E.department_id = D.department_id "
                            + "  and D.department_name = '";
        queryStr = queryStr + desensitize(argv[1]);
        for(int i = 2; i < argc; i++)
            queryStr = queryStr + " " + desensitize(argv[i]);
        queryStr = queryStr + "'";

        // create a statment object
        Statement *stmt = conn->createStatement();

        // using the statement object to execute a query
        ResultSet *rs = stmt->executeQuery(queryStr);

        if (rs->next()) {
            // process result
            do {
                cout << "EID: " << rs->getString(1);
                cout << " Name: " << left << setw(20) << rs->getString(2);
                cout << " Phone: " << rs->getString(3);
                cout << endl;
            } while (rs->next());
        } else {
            cout << "Nobody works in this department.\n";
        }

        stmt->closeResultSet(rs);
        conn->terminateStatement(stmt);
        env->terminateConnection(conn);
        Environment::terminateEnvironment(env);
    } catch (SQLException & e) {
        cout << e.what();
        return 0;
    }

    return 0;
}

