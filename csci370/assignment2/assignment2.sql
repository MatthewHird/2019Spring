-- P1 Q1
SELECT *
  FROM Students
       NATURAL JOIN Customers;

-- P1 Q2
SELECT c.name, e.empno, c.address
  FROM Employees e
       INNER JOIN Customers c
           ON e.cid = c.cid
 WHERE local IS NULL;

-- P1 Q3
SELECT p.startTime, p.expireTime, c.name
  FROM Permits p
       INNER JOIN Customers c
           ON p.owner = c.cid;

-- P1 Q4
SELECT DISTINCT c.cid, c.name
  FROM Customers c
       INNER JOIN Permits p
           ON c.cid = p.owner;

-- P1 Q5
SELECT p.startTime, p.purchaseTime, c.name
  FROM Permits p
       INNER JOIN Customers c
           ON p.owner = c.cid
 WHERE p.expireTime >= TO_TIMESTAMP('2019-06-30', 'yyyy-mm-dd');

-- P1 Q6
-- I was working with MySQL at home (which doesn't support MINUS)
SELECT c.name, c.email
  FROM Customers c
       LEFT JOIN Permits p
           ON c.cid = p.owner
 WHERE p.pid IS NULL;

-- Alternate way to do P1 Q6 with Set Difference
-- SELECT c.name, c.email
--   FROM Customers c
--     MINUS
--    SELECT name, email
--      FROM Customers
--           LEFT JOIN Permits
--               ON Customers.cid = Permits.owner;

-- P1 Q7
-- I was working with MySQL at home (which doesn't support MINUS)
SELECT DISTINCT c.name, c.email
  FROM Customers c
       INNER JOIN Permits p
           ON c.cid = p.owner
       LEFT JOIN Registration r
           ON p.pid = r.pid
 WHERE r.pid IS NULL;

-- Alternate way to do P1 Q7 with Set Difference
-- SELECT name, email
-- FROM Customers
-- INNER JOIN Permits
--     ON Customers.cid = Permits.owner
-- MINUS
-- SELECT name, email
-- FROM ((Customers
-- INNER JOIN Permits
--     ON Customers.cid = Permits.owner)
-- INNER JOIN Registration
--     ON Permits.pid = Registration.pid);

-- P1 Q8
SELECT plate, ticketTime, violation
  FROM Tickets
 WHERE fine > paidAmount
    OR fine > 0 AND paidAmount IS NULL;

-- P1 Q9
SELECT name
  FROM Customers
 WHERE cid NOT IN
       (SELECT cid
          FROM Tickets t
               INNER JOIN Registration r
                   ON t.plate = r.plate
               INNER JOIN Permits p
                   ON r.pid = p.pid
                        AND ticketTime BETWEEN startTime AND expireTime
               INNER JOIN Customers c
                   ON p.owner = c.cid);

-- P1 Q10
SELECT name
  FROM Customers
 WHERE cid NOT IN
       (SELECT DISTINCT p1.owner AS cid
          FROM Permits p1
               JOIN
                   (SELECT p2.pid
                      FROM Permits p2
                           INNER JOIN Registration r1
                               ON p2.pid = r1.pid
                     GROUP BY p2.pid
                    HAVING COUNT(p2.pid) > 1) p3
                   ON p1.pid = p3.pid
        UNION
        SELECT DISTINCT p4.owner
          FROM Permits p4
               LEFT JOIN Registration r2
                   ON p4.pid = r2.pid
         WHERE r2.pid IS NULL);

-- P2 Q1
SELECT SUM(fine)
  FROM Tickets
 WHERE ticketTime >= TO_TIMESTAMP('2019-01-01', 'yyyy-mm-dd');

-- P2 Q2
SELECT v1.plate AS plate_number,
       COUNT(t1.plate) AS num_of_tickets,
       CAST(COALESCE(AVG(t1.fine), 0) AS NUMBER(8, 2)) AS avg_fine
  FROM Vehicles v1
       LEFT JOIN Tickets t1
           ON v1.plate = t1.plate
 GROUP BY v1.plate;

-- P2 Q3
SELECT c.name, c.email
  FROM Customers c
       INNER JOIN Permits p
           ON c.cid = p.owner
 GROUP BY c.cid, c.name, c.email
HAVING COUNT(p.pid) > 5
 ORDER BY COUNT(p.pid) DESC;

-- P2 Q4
SELECT c1.name
  FROM Customers c1
       INNER JOIN Permits p1
           ON c1.cid = p1.owner
 GROUP BY c1.cid, c1.name
HAVING COUNT(p1.pid) >= ALL
       (SELECT COUNT(p2.pid)
          FROM Customers c2
               INNER JOIN Permits p2
                   ON c2.cid = p2.owner
         GROUP BY c2.cid);

-- P2 Q5
SELECT v0.plate, p.pid
  FROM Vehicles v0
       INNER JOIN Registration r
           ON v0.plate = r.plate
       INNER JOIN Permits p
           ON r.pid = p.pid
 WHERE v0.plate IN
       (SELECT v1.plate
          FROM Vehicles v1
               INNER JOIN Tickets t1
                   ON v1.plate = t1.plate
         GROUP BY v1.plate
        HAVING SUM(t1.fine) >= ALL
               (SELECT SUM(t2.fine)
                  FROM Vehicles v2
                       INNER JOIN Tickets t2
                           ON v2.plate = t2.plate
                 GROUP BY v2.plate));
