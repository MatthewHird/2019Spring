-- P1 Q1
SELECT *
  FROM Students
       NATURAL JOIN Customers;

-- P1 Q2
SELECT name, empno, address
  FROM Employees AS e
       INNER JOIN Customers AS c
           ON e.cid = c.cid
 WHERE local IS NULL;

-- P1 Q3
SELECT startTime, expireTime, name
  FROM Permits AS p
       INNER JOIN Customers AS c
           ON p.owner = c.cid;

-- P1 Q4
SELECT DISTINCT cid, name
  FROM Customers AS c
       INNER JOIN Permits AS p
           ON c.cid = p.owner;

-- P1 Q5
SELECT p.startTime, p.purchaseTime, c.name
  FROM Permits AS p
    INNER JOIN Customers AS c
    ON p.owner = c.cid
 WHERE p.expireTime >= '2019-06-30';

-- P1 Q6
SELECT name, email
  FROM Customers AS c -- MINUS
           -- SELECT name, email
           -- FROM Customers
           -- LEFT JOIN Permits
           --      ON Customers.cid = Permits.owner;
       LEFT JOIN Permits AS p
           ON c.cid = p.owner
 WHERE p.pid IS NULL;

-- P1 Q7
SELECT DISTINCT name, email
  FROM Customers AS c
       INNER JOIN Permits AS p
           ON c.cid = p.owner
       LEFT JOIN Registration AS r
           ON p.pid = r.pid
 WHERE r.pid IS NULL;

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
          FROM Tickets AS t
               INNER JOIN Registration AS r
                   ON t.plate = r.plate
               INNER JOIN Permits AS p
                   ON r.pid = p.pid
                        AND ticketTime BETWEEN startTime AND expireTime
               INNER JOIN Customers AS c
                   ON p.owner = c.cid);

-- P1 Q10
SELECT c1.name
  FROM Customers AS c1
 WHERE c1.cid NOT IN
       (SELECT DISTINCT p1.owner AS cid
          FROM Permits AS p1
               JOIN
                   (SELECT p2.pid
                      FROM Permits AS p2
                           INNER JOIN Registration AS r1
                               ON p2.pid = r1.pid
                     GROUP BY p2.pid
                    HAVING COUNT(p2.pid) > 1) AS p3
                   ON p1.pid = p3.pid
        UNION DISTINCT
        SELECT DISTINCT p4.owner
          FROM Permits AS p4
               LEFT JOIN Registration AS r2
                   ON p4.pid = r2.pid
         WHERE r2.pid IS NULL);


SELECT c1.name
  FROM Customers AS c1
 WHERE c1.cid NOT IN
       (SELECT DISTINCT p1.owner AS cid
          FROM Permits AS p1
               INNER JOIN Registration AS r1
                   ON p1.pid = r1.pid
         GROUP BY p1.pid
        HAVING COUNT(p1.pid) > 1
        UNION DISTINCT
        SELECT DISTINCT p4.owner
          FROM Permits AS p4
               LEFT JOIN Registration AS r2
                   ON p4.pid = r2.pid
         WHERE r2.pid IS NULL);

-- P2 Q1
SELECT SUM(fine)
  FROM Tickets
 WHERE ticketTime >= '2019-01-01';

-- P2 Q2
SELECT v1.plate AS "Plate Number",
       COUNT(t1.plate) AS "# of Tickets",
       CAST(COALESCE(AVG(t1.fine), 0) AS DECIMAL(8, 2)) AS "Avg Fine"
    -- SELECT v1.plate as 'Plate Number', count(t1.plate) as '# of Tickets', cast(coalesce(avg(t1.fine), 0) AS NUMBER(8,2)) as 'Avg Fine'
  FROM Vehicles AS v1
       LEFT JOIN Tickets AS t1
           ON v1.plate = t1.plate
 GROUP BY v1.plate;

-- P2 Q3
SELECT c.name, c.email
  FROM Customers AS c
       INNER JOIN Permits AS p
           ON c.cid = p.owner
 GROUP BY c.cid
HAVING COUNT(p.pid) > 5
 ORDER BY COUNT(p.pid) DESC;

-- P2 Q4
SELECT c1.name
  FROM Customers AS c1
       INNER JOIN Permits AS p1
           ON c1.cid = p1.owner
 GROUP BY c1.cid
HAVING COUNT(p1.pid) >= ALL
       (SELECT COUNT(p2.pid)
          FROM Customers AS c2
               INNER JOIN Permits AS p2
                   ON c2.cid = p2.owner
         GROUP BY c2.cid);

-- P2 Q5
SELECT v0.plate, p.pid
  FROM Vehicles AS v0
       INNER JOIN Registration AS r
           ON v0.plate = r.plate
       INNER JOIN Permits AS p
           ON r.pid = p.pid
 WHERE v0.plate IN
       (SELECT v1.plate
          FROM Vehicles AS v1
               INNER JOIN Tickets AS t1
                   ON v1.plate = t1.plate
         GROUP BY v1.plate
        HAVING SUM(t1.fine) >= ALL
               (SELECT SUM(t2.fine)
                  FROM Vehicles AS v2
                       INNER JOIN Tickets AS t2
                           ON v2.plate = t2.plate
                 GROUP BY v2.plate));
