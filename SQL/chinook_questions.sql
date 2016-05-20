-- Question 1: Find customers who purchased 'Aersosmith' tracks
SELECT album.title, artist.name, track.name, customer.firstname, customer.lastname
FROM album 
  INNER JOIN artist
    ON album.artistid = artist.artistid
      INNER JOIN track
        ON album.albumid = track.albumid
          INNER JOIN invoiceline
            ON track.trackid = invoiceline.trackid
              INNER JOIN invoice
                ON invoiceline.invoiceid = invoice.invoiceid
                  INNER JOIN customer
                    ON invoice.customerid = customer.customerid
WHERE artist.name = 'Aerosmith';                    
select name from artist;

--Question 2: Which states have purchases for 'Metallica' tracks
SELECT DISTINCT customer.state, customer.country
FROM customer
  INNER JOIN invoice
    ON customer.customerid = invoice.customerid
      INNER JOIN invoiceline
        ON invoice.invoiceid = invoiceline.invoiceid
          INNER JOIN track
            ON invoiceline.trackid = track.trackid
              INNER JOIN album
                ON track.albumid = album.albumid
                  INNER JOIN artist
                    ON album.artistid = artist.artistid
WHERE artist.name = 'Metallica';

--Question 3: Which month had the higest number of orders or any tracks?
SELECT EXTRACT(MONTH FROM invoicedate) as OrderMonth, Count(*)
FROM invoice
GROUP BY EXTRACT(MONTH FROM invoicedate)
order by EXTRACT(MONTH FROM invoicedate) ASC;

--Question 4: Which year had the higest number of orders or any tracks?
SELECT EXTRACT(YEAR FROM invoicedate) as OrderYear, Count(*)
FROM invoice
GROUP BY EXTRACT(YEAR FROM invoicedate)
order by EXTRACT(YEAR FROM invoicedate) ASC;

--Question 5: What are the top trending media types
SELECT mediatype.name, Count(*)
FROM mediatype
  INNER JOIN track
    ON mediatype.mediatypeid = track.mediatypeid
      INNER JOIN invoiceline
        ON track.trackid = invoiceline.trackid
          INNER JOIN invoice
            ON invoiceline.invoiceid = invoice.invoiceid
GROUP BY mediatype.name
ORDER BY Count(*) DESC;

--Question 6: What are the top 'Genre'
SELECT genre.name, Count(*)
FROM genre
  INNER JOIN track
    ON genre.genreid = track.genreid
      INNER JOIN invoiceline
        ON track.trackid = invoiceline.trackid
          INNER JOIN invoice
            ON invoiceline.invoiceid = invoice.invoiceid
GROUP BY genre.name
ORDER BY Count(*) DESC;


-- Question 7: What is the lowest priced media type?
SELECT DISTINCT mediatype.name, invoiceline.unitprice
FROM invoiceline
  INNER JOIN track
    ON invoiceline.trackid = track.trackid
      INNER JOIN mediatype
        ON track.mediatypeid = mediatype.mediatypeid
WHERE invoiceline.unitprice = (SELECT MIN(unitprice) FROM invoiceline)
ORDER BY mediatype.name ASC;


-- Question 8: What are the highest buying customers?
SELECT CONCAT(customer.firstname, CONCAT(' ', customer.lastname)) AS buyer, SUM(invoice.total) AS purchases
FROM customer
  INNER JOIN invoice
    ON customer.customerid = invoice.customerid
GROUP BY CONCAT(customer.firstname, CONCAT(' ', customer.lastname))
ORDER BY purchases DESC;

-- Question 9: Which are the top earning tracks?
SELECT track.name, SUM(invoiceline.unitprice)
FROM track
  INNER JOIN invoiceline
    ON track.trackid = invoiceline.trackid
GROUP BY track.name
ORDER  BY SUM(invoiceline.unitprice) DESC;


-- Question 10: Which are the top grossing artists?
SELECT artist.name, SUM(invoiceline.unitprice)
FROM artist
  INNER JOIN album
    ON artist.artistid = album.artistid
      INNER JOIN track
        ON album.albumid = track.albumid
          INNER JOIN invoiceline
            ON track.trackid = invoiceline.trackid
GROUP BY artist.name
ORDER  BY SUM(invoiceline.unitprice) DESC;

-- Question 11: Which are the top grossing album?
SELECT album.title, SUM(invoiceline.unitprice)
FROM artist
  INNER JOIN album
    ON artist.artistid = album.artistid
      INNER JOIN track
        ON album.albumid = track.albumid
          INNER JOIN invoiceline
            ON track.trackid = invoiceline.trackid
GROUP BY album.title
ORDER  BY SUM(invoiceline.unitprice) DESC; 

-- Question 12: What country did the employees primarily help most customers?
SELECT CONCAT(employee.firstname, CONCAT(' ', employee.lastname)) AS employee_name, 
customer.country, Count(*)
FROM employee
  INNER JOIN customer
    ON employee.employeeid = customer.supportrepid
      INNER JOIN INVOICE
        ON customer.customerid = invoice.customerid
GROUP BY CONCAT(employee.firstname, CONCAT(' ', employee.lastname)), customer.country 
ORDER BY employee_name, Count(*) DESC;









