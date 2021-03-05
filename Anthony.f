c************************************************************************************
c*          CSC 407/507 Programming Assignment #2: The OLDE Language Assignment
c*                                 Fortran Group Project   
c*               written by: Anthony J. Bosch, Shawn C. Huesman, Wie L. Sie 
c************************************************************************************
c*
c*  This program will process customer transaction
c*  The program will input data from 3 files: inventory, customers, and transactions
c*  It will compute each transaction in transcation file: if there is no customer or
c*  product found then generate error report
c*  if not, compute: the gross cost (cost of inventory item * number ordered),
c*  the discount based on the sale code (if any), and the net cost (gross – discount).
c*  Then update the customer debt amount, also update number in product availability
c*  There are 3 output files will be generated: 
c*  transactions processed, inventory orders, errors
c*
c************************************************************************************

c************************************************************************************
c*      This is the main part of the program
c*      Declares the program variables
c*      and call subroutines that will do the computations for processing transactions
c*      such as discount, reorder amount, etc
c*      and the subroutines will output error report, ordering report,
c*      and transactions processed report

        PROGRAM ANTHONY
        INTEGER       PID, STOCK, REORDERPOINT, I, PIDARR(24)
        INTEGER       STOCKARR(24)
        INTEGER       REORDERPOINTARR(24)
        CHARACTER*24  PNAME, PNAMEARR(24)
        REAL          PRICE, PRICEARR(24)

        INTEGER       CID, J, CIDARR(10)
        CHARACTER*24  CNAME, STREET, CITY, STATECOUNTRY
        CHARACTER*24  CNAMEARR(10), STREETARR(10), CITYARR(10)
        CHARACTER*24  STATECOUNTRYARR(10)
        REAL	      DEBT, DEBTARR(10)

        INTEGER       ORDERED
        CHARACTER     SALECODE
        REAL          GROSS, NET, DISCOUNT
        INTEGER       REORDERAMOUNT, TEMPSTOCKVAR         

        CALL GETINVENTORY(PIDARR, STOCKARR, REORDERPOINTARR, 
     +  PNAMEARR, PRICEARR)
        CALL GETCUSTOMERS(CIDARR, CNAMEARR, STREETARR, CITYARR, 
     +  STATECOUNTRYARR, DEBTARR)
        CALL PROCESSTRANSACTIONS(PIDARR, CIDARR, STOCKARR,
     +  REORDERPOINTARR, PRICEARR, DEBTARR, CNAMEARR,
     +  STREETARR, CITYARR, PNAMEARR, STATECOUNTRYARR)
        END PROGRAM
c************************************************************************************

c************************************************************************************
c*      GETINVENTORY subroutine that will read inventory file input and store data 
c*      into separate arrays :
c*      Input product information from inventory.dat to arrays of product id, 
c*      product name, stock availability, reorder point, and price
        SUBROUTINE GETINVENTORY(PIDARR, STOCKARR, REORDERPOINTARR, 
     +  PNAMEARR, PRICEARR)
        INTEGER       PIDARR(24), STOCKARR(24), REORDERPOINTARR(24)
        CHARACTER*24  PNAMEARR(24)
        REAL          PRICEARR(24)

        INTEGER       PID, STOCK, REORDERPOINT, I
        CHARACTER*24  PNAME
        REAL          PRICE
        OPEN(UNIT=1, FILE='inventory.dat', STATUS='OLD')
        I = 1
100     READ(1, 120, IOSTAT=IVAL)PID, PNAME, STOCK, REORDERPOINT, PRICE
        IF(IVAL) 199, 110, 199
110         PIDARR(I) = PID
            PNAMEARR(I) = PNAME
            STOCKARR(I) = STOCK
            REORDERPOINTARR(I) = REORDERPOINT
            PRICEARR(I) = PRICE
            I = I + 1

120     FORMAT(I10, A25, I7, I7, F6.2)

        GO TO 100
199     I = 1
        CLOSE(UNIT=1, STATUS='KEEP')
        END SUBROUTINE
c************************************************************************************

c************************************************************************************
c*      GETCUSTOMERS subroutine that will read customer file input and store data 
c*      into separate arrays :
c*      Input customer information from customer.dat to arrays of customer id,
c*      customer name, customer street address, customer city,
c*      customer state or country, debt
        SUBROUTINE GETCUSTOMERS(CIDARR, CNAMEARR, STREETARR, CITYARR, 
     +  STATECOUNTRYARR, DEBTARR)
        INTEGER       CIDARR(10)
        CHARACTER*24  CNAMEARR(10), STREETARR(10), CITYARR(10)
        CHARACTER*24  STATECOUNTRYARR(10)
        REAL	      DEBTARR(10)

        INTEGER       CID, J
        CHARACTER*24  CNAME, STREET, CITY, STATECOUNTRY
        REAL          DEBT
        OPEN(UNIT=2, FILE='customers.dat', STATUS='OLD')

        J = 1
200     READ(2, 220, IOSTAT=IVAL)CID,CNAME,STREET,CITY,STATECOUNTRY,DEBT
        IF(IVAL) 299, 210, 299
210         CIDARR(J) = CID
            CNAMEARR(J) = CNAME
            STREETARR(J) = STREET
            CITYARR(J) = CITY
            STATECOUNTRYARR(J) = STATECOUNTRY
            DEBTARR(J) = DEBT
            J = J + 1

220     FORMAT(I10, A23, A23, A13, A12, F6.2)

        GO TO 200
299     J = 1     
        CLOSE(UNIT=2, STATUS='KEEP')
        END SUBROUTINE
c************************************************************************************

c************************************************************************************
c*      PROCESSTRANSACTIONS subroutine that will read transaction file input,
c*      store data into separate arrays, and perform computations :
c*
c*      Compute error file if product id or customer id is invalid.
c*         
c*      and call CALCULATEDISCOUNT subroutine if product id or customer id is valid

        SUBROUTINE PROCESSTRANSACTIONS(PIDARR, CIDARR, STOCKARR,
     +  REORDERPOINTARR, PRICEARR, DEBTARR, CNAMEARR,
     +  STREETARR, CITYARR, PNAMEARR, STATECOUNTRYARR)

        INTEGER       PIDARR(24), CIDARR(10), STOCKARR(24)
        INTEGER       REORDERPOINTARR(24)
        REAL          PRICEARR(24), DEBTARR(10)
        CHARACTER*24  CNAMEARR(10), STREETARR(10), CITYARR(10)
        CHARACTER*24  PNAMEARR(24)
        CHARACTER*24  STATECOUNTRYARR(10)

        INTEGER       CID, PID, ORDERED, K, L
        CHARACTER     SALECODE
        
        OPEN(UNIT=3, FILE='transactions.dat', STATUS='OLD')
        OPEN(UNIT=10, FILE='ERROR.DAT', STATUS='NEW')
        OPEN(UNIT=11, FILE='TransactionsProcessed.dat', STATUS='NEW')
        OPEN(UNIT=12, FILE='InventoryOrders.dat', STATUS='NEW')
     
300     READ(3, 320, IOSTAT=IVAL)CID,PID,ORDERED,SALECODE
        IF(IVAL)399,301,399
        
301         DO 302 K = 1, size(CIDARR)
                IF(CID .EQ. CIDARR(K)) Then
c*                  Valid customer id, check if product id is valid
                    GO TO 303
                END IF
302         CONTINUE

305     WRITE (10, 330)CID, PID, ORDERED
        GO TO 300

303         DO 304 L = 1, size(PIDARR)
                IF(PID .EQ. PIDARR(L)) Then
c*                  Valid product id, continue transaction
                    CALL CALCULATEDISCOUNT(PIDARR, STOCKARR,
     +              REORDERPOINTARR, PRICEARR, DEBTARR, CNAMEARR,
     +              STREETARR, CITYARR, PNAMEARR, STATECOUNTRYARR,
     +              ORDERED, SALECODE, K, L)        
                    GO TO 300
                END IF       
304         CONTINUE

315     WRITE (10, 340)CID, PID, ORDERED
        GO TO 300        
            
320     FORMAT(I10, I12, I6, A1)
330     FORMAT(I5.5, 4x, I6.6, 4x, I2, 2x, 'INVALID CUSTOMER NUMBER')
340     FORMAT(I5.5, 4x, I6.6, 4x, I2, 2x, 'INVALID PRODUCT NUMBER')
399     CLOSE(UNIT=3, STATUS='KEEP')
        CLOSE(UNIT=10, STATUS='KEEP')
        CLOSE(UNIT=11, STATUS='KEEP')
        CLOSE(UNIT=12, STATUS='KEEP')        
        END SUBROUTINE
c************************************************************************************

c************************************************************************************
c*      CALCULATEDISCOUNT subroutine perform computations :
c*
c*      Process transactions and compute gross sale amount, net sale
c*      amount, discount applied, and debt owed after each transaction.
c*     
c*      and output results to the transactions processed file
c*      also call OUTPUTORDER subroutine that will
c*      find amount of inventory to reorder after transactions using
c*      stock left and reorder point,


        SUBROUTINE CALCULATEDISCOUNT(PIDARR, STOCKARR,
     +  REORDERPOINTARR, PRICEARR, DEBTARR, CNAMEARR,
     +  STREETARR, CITYARR, PNAMEARR, STATECOUNTRYARR,
     +  ORDERED, SALECODE, K, L)

        INTEGER       PIDARR(24), STOCKARR(24)
        INTEGER       REORDERPOINTARR(24)        
        REAL          PRICEARR(24), DEBTARR(10)
        CHARACTER*24  CNAMEARR(10), STREETARR(10), CITYARR(10)
        CHARACTER*24  PNAMEARR(24)
        CHARACTER*24  STATECOUNTRYARR(10)
        INTEGER       K, L  

        INTEGER       ORDERED, REORDERAMOUNT
        CHARACTER     SALECODE
        REAL          NET, DISCOUNT
   
405     GROSS = ORDERED * PRICEARR(L)

        IF (SALECODE .EQ. 'A') Then
            NET = GROSS * 90/100
        ELSE IF (SALECODE .EQ. 'B') Then
            NET = GROSS * 80/100
        ELSE IF (SALECODE .EQ. 'C') Then
            NET = GROSS * 75/100
        ELSE IF (SALECODE .EQ. 'D') Then
            NET = (ORDERED - INT (ORDERED/3)) * PRICEARR(L)
        ELSE IF (SALECODE .EQ. 'E') Then
            IF (ORDERED .EQ. 1) Then
                NET = PRICEARR(L)
            ELSE                
                NET = (ORDERED - INT (ORDERED/2)) * PRICEARR(L)
            END IF     
        ELSE IF (SALECODE .EQ. 'Z') Then
            NET = GROSS    
        END IF
        DISCOUNT = GROSS - NET
          
        DEBTARR(K) = DEBTARR(K) + NET

        WRITE(11,410)CNAMEARR(K),STREETARR(K), CITYARR(K),
     +  STATECOUNTRYARR(K),PNAMEARR(L), ORDERED, GROSS,
     +  DISCOUNT, NET, DEBTARR(K)

410     FORMAT(A23, 1x, A23, 1x, A13, 1x, A12, 1x, A25, 1x, I2,
     +  1x, F6.2, 1x, F6.2, 1x, F6.2, 1x, F6.2)

        CALL OUTPUTORDER (PIDARR, STOCKARR,
     +  REORDERPOINTARR, ORDERED, L)        
        END SUBROUTINE
c************************************************************************************
        
c************************************************************************************
c*      OUTPUTORDER subroutine perform computations :
c*      
c*      Calculate the quantity that need to be reordered based on
c       reorderpoint after process transaction 
c*      
        SUBROUTINE OUTPUTORDER(PIDARR, STOCKARR,
     +  REORDERPOINTARR, ORDERED, L)

        INTEGER       PIDARR(24), STOCKARR(24)
        INTEGER       REORDERPOINTARR(24)        
        INTEGER       ORDERED, L

        INTEGER       REORDERAMOUNT, TEMPSTOCKVAR
        INTEGER       TEMPREORDVAR    
        TEMPSTOCKVAR = STOCKARR(L)        
        STOCKARR(L) = TEMPSTOCKVAR - ORDERED
        IF(STOCKARR(L) .LE. REORDERPOINTARR(L)) Then
            TEMPREORDVAR = REORDERPOINTARR(L)    
c*          If reorder point is 1,    have 3 in stock
c*          '                ' 2-5,   have 6 in stock
c*          '                ' 6-10,  have 12 in stock
c*          '                ' 11-20, have 25 in stock
c*          otherwise,                have 30 in stock
            IF (TEMPREORDVAR .EQ. 1) THEN
                REORDERAMOUNT = 3 - STOCKARR(L)
            ELSE IF (TEMPREORDVAR .LE. 5) THEN
                REORDERAMOUNT = 6 - STOCKARR(L)
            ELSE IF (TEMPREORDVAR .LE. 10) THEN
                REORDERAMOUNT = 12 - STOCKARR(L)
            ELSE IF (TEMPREORDVAR .LE. 20) THEN
                REORDERAMOUNT = 25 - STOCKARR(L)
            ELSE
                REORDERAMOUNT = 30 - STOCKARR(L)
            END IF
            IF (REORDERAMOUNT .GT. 0) THEN
                WRITE(12,420)PIDARR(L), REORDERAMOUNT
            END IF
        END IF
420     FORMAT(I6.6, 1x, I2)
        END SUBROUTINE
c************************************************************************************
