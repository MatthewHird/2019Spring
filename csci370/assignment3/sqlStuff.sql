-- part 1 statement 1
SELECT cid
FROM Customers
WHERE cid = :1 MYCID;

-- part 1 statement 3
SELECT cid, name, pid, starttime, expiretime, purchasetime
FROM Customers
INNER JOIN Permits
ON cid = owner
WHERE cid = :1 MYCID
ORDER BY purchasetime, expiretime, starttime DESC
fetch first row only;

-- part 1 statement 4
SELECT v.plate
FROM Vehicles v
INNER JOIN Registration r
ON v.plate = r.plate
WHERE r.pid = part3.pid;

-- part 2 statement 1
SELECT DISTINCT r.plate
FROM Registration r 
INNER JOIN Permits p
ON r.pid = p.pid
WHERE r.plate = :1 MYPLATE
AND p.expiretime >= :2 CURRENTDATEVALUE
AND p.starttime <= :3 CURRENTDATEVALUE;

-- part 2 statement 2
INSERT INTO Tickets (tickettime, violation, fine, plate, issuer, tid)
SELECT :1 CURRENTDATE, "Parking without permit", 45.00, :2 MYPLATE, :3 WESTPARKIDNAME,
1 + MAX(tid)
FROM Tickets;