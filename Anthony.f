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
c*	                               Declare Variables

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

        INTEGER       REORDERAMOUNT, TEMPSTOCKVAR, TEMPREORDVAR           
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

120     FORMAT(I10, A25, I7, I7, F6.2)
130     FORMAT(I6.6, 4x, A25, 1x, I2.2, 5x, I2.2, 5x, F6.2)
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
c**************************  Processing transaction  ********************************

c**************************GENERATE ERROR REPORT
        OPEN(UNIT=3, FILE='transactions.dat', STATUS='OLD')
        OPEN(UNIT=10, FILE='ERROR.DAT', STATUS='OLD')

        K = 1
300     READ(3, 320, IOSTAT=IVAL)CID,PID,ORDERED,SALECODE
        
        IF(IVAL)399,301,399

301         DO 302 K = 1, 10
                IF(CID .EQ. CIDARR(K)) Then
                    GO TO 303
                END IF
302         CONTINUE

305     WRITE (10, 330)CID, PID, ORDERED
        GO TO 300

303         DO 304 L = 1, 24
                IF(PID .EQ. PIDARR(L)) Then
                    GO TO 310
                END IF       
304         CONTINUE
        WRITE (10, 340)CID, PID, ORDERED
        GO TO 300        
            
320     FORMAT(I10, I12, I6, A1)
330     FORMAT (I5.5, 4x, I6.6, 4x, I2, 2x, 'INVALID CUSTOMER NUMBER')
340     FORMAT (I5.5, 4x, I6.6, 4x, I2, 2x, 'INVALID PRODUCT NUMBER')   
c**************************GENERATE ERROR REPORT COMPLETED

310     WRITE(*,*)'CALCULATING GROSS COST, DISCOUNT, NET COST, ETC'

*************************COMPUTE GROSS COST, DISCOUNT, NET COST
        GROSS = ORDERED * PRICEARR(L)
  
370     FORMAT(I3, 1x, F6.2, 1x, F6.2)

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
        WRITE(*,*)ORDERED, PRICEARR(L), GROSS, DISCOUNT, NET,
     +  DEBTARR(K)    
c********************COMPUTE GROSS COST, DISCOUNT, NET COST COMPLETED


c****************************************COMPUTE REORDER
c*        OPEN(UNIT=11, FILE='INVENTORYORDERS.DAT', STATUS='OLD')
c*        TEMPSTOCKVAR = STOCKARR(K)        
c*        STOCKARR(K) = TEMPSTOCKVAR - ORDERED
c*        IF(STCKARR(K) .LE. REORDERPOINTARR(K)) Then
c*            TEMPREORDVAR = REORDERPOINTARR(K)    
c*            IF(TEMPREORDVAR .EQ. 1)    
c*                REORDERAMOUNT = REORDERPOINTARR(K) - STOCKARR(K)
c*        WRITE(11,410)CNAMEARR(K),STREETARR(K), CITYARR(K),
c*     &  STATECOUNTRYARR(K),PNAMEARR(L), ORDERED, GROSS,
c*     &  DISCOUNT, NET, DEBTARR(K)            
c*410     FORMAT(A23, 1x, A23, 1x, A13, 1x, A12, 1x, A25, 1x, A3,
c*     &  1x, F6.2, 1x, F6.2, 1x, F6.2, 1x, F6.2, 1x,)

c*******************************************COMPUTE REORDER COMPLETED
        GO TO 300
399     CLOSE(UNIT=3, STATUS='KEEP')



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
