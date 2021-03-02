c************************************************************************************
c*          CSC 407/507 Programming Assignment #2: The OLDE Language Assignment
c*                                 Fortran Group Project   
c*                written by: Anthony J. Bosch, Shawn C. Huesman, Wie L. Sie 
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
c*	                               Declare Variables
PROGRAM X
        INTEGER       PID, STOCK, REORDERPOINT, I, PIDARR(24)
        INTEGER       STOCKARR(24)
        INTEGER       REORDERPOINTARR(24)
        CHARACTER*24  PNAME, PNAMEARR(24)
        REAL		  PRICE, PRICEARR(24)

        INTEGER       CID, J, CIDARR(10)
        CHARACTER*24  CNAME, STREET, CITY, STATECOUNTRY
        CHARACTER*24  CNAMEARR(10), STREETARR(10), CITYARR(10)
        CHARACTER*24  STATECOUNTRYARR(10)
        REAL		  DEBT, DEBTARR(10)
     
        INTEGER       ORDERED
        CHARACTER     SALECODE         
c************************************************************************************

c************************************************************************************
c*  Input product information from inventory.dat to arrays of product id, 
c*  product name, stock availability, reorder point, and price
        OPEN(UNIT=1, FILE='inventory.dat', STATUS='OLD')
        OPEN(UNIT=10, FILE='inventoryInput.dat', STATUS='OLD')
        I = 1
100     READ(1, 120, IOSTAT=IVAL)PID, PNAME, STOCK, REORDERPOINT, PRICE
        IF(IVAL) 199, 110, 199
110         WRITE(*,130)PID, PNAME, STOCK, REORDERPOINT, PRICE
            PIDARR(I) = PID
            PNAMEARR(I) = PNAME
            STOCKARR(I) = STOCK
            REORDERPOINTARR(I) = REORDERPOINT
            PRICEARR(I) = PRICE
            I = I + 1

120     FORMAT(I10, A25, I7, I7, F5.2)
130     FORMAT(I6.6, 4x, A25, 1x, I2.2, 5x, I2.2, 5x, F5.2)
        GO TO 100
199     I = 1
        CLOSE(UNIT=1, STATUS='KEEP')
c************************************************************************************

c************************************************************************************
c*  Input customer information from customer.dat to arrays of customer id,
c*  customer name, customer street address, customer city,
c*  customer state or country, debt
        OPEN(UNIT=2, FILE='customers.dat', STATUS='OLD')
        OPEN(UNIT=20, FILE='customersInput.dat', STATUS='OLD')
        J = 1
200     READ(2, 220, IOSTAT=IVAL)CID,CNAME,STREET,CITY,STATECOUNTRY,DEBT
        IF(IVAL) 299, 210, 299
210         WRITE(*,230)CID,CNAME,STREET,CITY,
     &          STATECOUNTRY,DEBT
            CIDARR(J) = CID
            CNAMEARR(J) = CNAME
            STREETARR(J) = STREET
            CITYARR(J) = CITY
            STATECOUNTRYARR(J) = STATECOUNTRY
            DEBTARR(J) = DEBT
            J = J + 1

220     FORMAT(I10, A23, A23, A13, A12, F6.2)
230     FORMAT(I5.5, 4x, A23, 1x, A23, 1x, A13, 1x, A12, 1x, F6.2)
        GO TO 200
299     J = 1     
        CLOSE(UNIT=2, STATUS='KEEP')
c************************************************************************************


c************************************************************************************
c*  Processing transaction
        OPEN(UNIT=3, FILE='transaction.dat', STATUS='OLD')
        OPEN(UNIT=10, FILE='ERROR.DAT', STATUS='NEW')
        K = 1
300     READ(3, 320, IOSTAT=IVAL)CID,PID,ORDERED,SALECODE
        IF(IVAL)399,389,399
389         DO 500 I = 1, 10
c*            IF(CIDARG .EQ. CIDARR(I)) Then
c*                GO TO 599
c*            ENDIF
            CALL ISVALIDPRODUCT(CID, PID, ORDERED)
320     FORMAT(I10, I12, A6, A1)
399     CLOSE(UNIT=3, STATUS='KEEP')

500     CONTINUE
c************************************************************************************


c************************************************************************************
c* check if acustomer number is valid or not
c* if not valid, write error report consisting of:  customer number, product number,
c* number ordered, and type of error (invalid customer number).
c*        SUBROUTINE ISVALIDCUSTOMER(CIDARG, PIDARG, ORDEREDARG)
c*        INTEGER CIDARG
c*        INTEGER PIDARG
c*        INTEGER ORDEREDARG
c*        DO 500 I = 1, 10
c*            IF(CIDARG .EQ. CIDARR(I)) Then
c*                GO TO 599
c*            ENDIF
c*        GO TO 505  
c*500     CONTINUE
c*505     WRITE (10, 510)CIDARG, PIDARG, ORDEREDARG
c*510     FORMAT (I5.5, 4x, I6.6, 4x, I2, 2x)
c*599     RETURN
c*        END
c************************************************************************************


c********************************* PROGRAM END **************************************

        STOP
        END