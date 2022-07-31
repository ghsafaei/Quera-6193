Program my_8
 implicit none
 INTEGER, PARAMETER :: LEN_STR = 80
 INTEGER :: i ,id,A(10),X(10),j,k,y(10)
 CHARACTER(len=LEN_STR) :: string1
  CHARACTER(len=LEN_STR) :: string2
  WRITE (*,*) 'Please enter string of input letters:'
  READ (*,*) string1
 j=0
  do i=1,LEN_STR  
    IF ( string1(i:i) .ne. ' ') THEN
     j=j+1
    END IF
  end do

    DO i = 1, j
	 IF ( string1(i:i) >= 'a' .AND. string1(i:i) <= 'z' ) THEN
       A(i) = IACHAR ( string1(i:i) ) - 97
	  ELSE IF  ( string1(i:i) >= 'A' .AND. string1(i:i) <= 'Z' ) THEN
	   A(i) = IACHAR ( string1(i:i) ) - 65  
     END IF 
    END DO

	   do i=1,j
		  X(i)=0
	   end do

	    DO i = 1, j
	  		  	    DO k = 1, j
	  				   IF ( A(i) .eq. A(k)) THEN
                          X(i)=X(i)+1
                       END IF
                    END DO
        END DO

               do i=1,j
			     y(i)=mod(((A(i)*X(i))+1),26)
               end do


	                Do i=1,j
							 IF ( string1(i:i) >= 'a' .AND. string1(i:i) <= 'z' ) THEN
                                 string2(i:i)= ACHAR (y(i) +97)
	                          ELSE IF  ( string1(i:i) >= 'A' .AND. string1(i:i) <= 'Z' ) THEN
	                             string2(i:i)= ACHAR (y(i) +65)  
                              END IF
                    END DO
 WRITE(*,*) string2
End program
