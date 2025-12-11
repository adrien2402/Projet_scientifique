! projet_sci_etape7.f90
! Adrien PICHON & Hugo REVEILLERE

!--------- MODULE ----------------------------------------------
module global
	implicit none
	! on déclare les variables que nous allons lire dans 'input.dat' et les maillages u et v dont nous ne connaissons pas encore la taille
	integer :: N,frequence,Nx,Ny,necriture
	real :: pinit,dx,dy,L,rtol,nu,Fo,CFL,tf,dt,Vmax,t,dtFo,rho,Re
	real, dimension (:,:), allocatable :: p,mat,u,v,us,vs,termedroitepoisson,uetoile,vetoile,dV
	real, dimension (:), allocatable ::pj,psj,x,y ,b
	character (len = 4 ) :: type_res
	
contains
!-------------------------------------------------------------------------------------------
	! on va lire les données qui caractérisent notre problème
	subroutine lecture()
	implicit none
		character(len=6) :: nom
		
		open(10,file='input.dat')
	
		read(10,'(5X,I3)') Nx
		read(10,'(5X,I3)') Ny
		read(10,'(6X,F3.1)') CFL
		read(10,'(5X,F3.1)') Fo
		read(10,'(5X,F3.1)') tf
		read(10,'(4X,F3.1)') L
		read(10,'(7X,F3.1)') Vmax
		read(10,'(7X,F3.1)') pinit
		read(10,'(7X,F8.6)') rtol
		read(10,'(7X,I3)') frequence
		read(10,'(5X,F8.2)') Re
		read(10,'(19X,A4)')  type_res
		close(10)
		rho = 1
	return
	end subroutine lecture

!-------------------------------------------------------------------------------------------------

	! on allocate tous nos tableaux et on definit le maillage
	subroutine def_maill()  
	implicit none
	 
		INTEGER :: i,j
		real :: deltax,deltay
		
		N = Nx*Ny
		dx = L/real(Nx-1)
		dy = L/real(Ny-1)
		nu = L*Vmax/Re
		
		allocate( p(0:Nx-1,0:Ny-1) )
		allocate( mat(0:N-1,0:N-1) )
		allocate(u(0:Nx-1,0:Ny-1))
		allocate(us(0:Nx-1,0:Ny-1))
		allocate(v(0:Nx-1,0:Ny-1))
		allocate(vs(0:Nx-1,0:Ny-1))
		allocate(termedroitepoisson(0:Nx-1,0:Ny-1))
		allocate(uetoile(0:Nx-1,0:Ny-1))
		allocate(vetoile(0:Nx-1,0:Ny-1))
		allocate(dV(0:Nx-1,0:Ny-1))
		
		allocate( x(0:Nx-1) )
		allocate( y(0:Ny-1) )
		allocate( b(0:N-1) )
		allocate( pj(0:N-1) )
		allocate( psj(0:N-1) )
		
		do i = 0,Nx-1
			x(i) = real(i) * dx
		end do
		
		do i = 0,Ny-1
			y(i) = real(i) * dy
		end do
		
		do j = 0, Ny-1
			do i = 0, Nx-1
				deltax=dx
				deltay=dy
				if ((i==0) .or. (i==Nx-1)) then
			    		deltax=0.5*dx
			 	end if
			 	if ((j==0) .or. (j==Ny-1)) then
			    		deltay=0.5*dy
			 	end if
				
				dV(i,j) = deltax*deltay
		    	
			end do
	    	end do
		
	return
	end subroutine def_maill
	
!-----------------------------------------------------------------------------------------------
	
	! on utilise conditions initiales et aux limites de l'énoncé pour remplir p et l'ecrire dans le fichier 'file0.tec'
	subroutine cond_init()  
	implicit none  
		
		p(0:Nx-1,0:Ny-1) = 0.0
		u(0:Nx-1,0:Ny-2) = 0.0
		u(0:Nx-1,Ny-1) = Vmax
		v(0:Nx-1,0:Ny-1) = 0.0
		
		necriture = 0
		
		call ecriture()
		
	return
	end subroutine cond_init
	
	
!-----------------------------------------------------------------------------------------------
    
    	! on rempli pj avec les conditions aux limites et mat de telle sorte que mat*psj=pj
    	subroutine remplissagematrice()
    	implicit none
    		INTEGER :: i,j,k
          
    		mat(0:N-1,0:N-1) = 0.0
    
    		DO j = 0,Ny-1
   	        	DO i = 0,Nx-1
   			        k=i+j*Nx
   		           
   		             	
                        	IF (j==0) THEN
   			        	mat(k,k) = 1.0
   			        	mat(k,k+Nx) = -1.0
   			       
   			        else if (j==Ny-1)then
   			        	mat(k,k) = 1.0
   			        	mat(k,k-Nx) = -1.0
   			        
   			        else if (i==0) then
   			        	mat(k,k) = 1.0
   			        	mat(k,k+1) = -1.0
   			        
   			        
   			        else if (i==Nx-1) then
   			        	mat(k,k) = 1.0
   			        	mat(k,k-1) = -1.0
   			    
   		        	ELSE	
   			        	mat(k,k) =-2.0*(1/dx**2 + 1/dy**2)
   			        	
   			        	if (k-1>=0) then
   			            		mat(k,k-1) =1.0/dx**2
   			        	end if
   			        
   			        	if (k+1<=N-1) then
   			            		mat(k,k+1) =1.0/dx**2
   			        	end if    
   			        
   			        	if (k-Nx>=0) then
   			            		mat(k,k-Nx) =1.0/dy**2
   			        	end if
   			        
   			        	if (k+Nx<=N-1) then
   			            		mat(k,k+Nx) =1.0/dy**2
   			        	end if
   			        
   		        	END IF
		        
   	        	END DO
   	        
        	END DO
        
        return
	end subroutine
	
!-----------------------------------------------------------------------------------------------
	!discrétisation CDS-4
	subroutine CDS4()
		implicit none
	
		integer :: i,j
			
		DO j = 1,Ny-2        		
   	     		DO i = 1,Nx-2
				if ( (j==1) .OR. (j==Ny-2) .OR. (i==1) .OR. (i==Nx-2)) then
					
					uetoile(i,j) = u(i,j) -u(i,j)*dt/(2*dx)*(u(i+1,j)-u(i-1,j)) &
						-v(i,j)*dt/(2*dy)*(u(i,j+1)-u(i,j-1)) &
					     	+ nu*dt*((u(i+1,j)-2.0*u(i,j)+u(i-1,j))/dx**2 + &
					     	(u(i,j+1)-2.0*u(i,j)+u(i,j-1))/dy**2)
					
					vetoile(i,j) = v(i,j) -u(i,j)*dt/(2*dx)*(v(i+1,j)-v(i-1,j)) &
						-v(i,j)*dt/(2*dy)*(v(i,j+1)-v(i,j-1)) &
					     	+ nu*dt*((v(i+1,j)-2.0*v(i,j)+v(i-1,j))/dx**2 + &
					     	(v(i,j+1)-2.0*v(i,j)+v(i,j-1))/dy**2)
				else
				
					uetoile(i,j) = u(i,j) -u(i,j)*dt/(6*dx)*(4*(u(i+1,j)-u(i-1,j))-0.5*(u(i+2,j)-u(i-2,j))) &
						-v(i,j)*dt/(6*dx)*(4*(u(i,j+1)-u(i,j-1))-0.5*(u(i,j+2)-u(i,j-2))) &
					     	+ nu*dt*((u(i+1,j)-2.0*u(i,j)+u(i-1,j))/dx**2 + &
					     	(u(i,j+1)-2.0*u(i,j)+u(i,j-1))/dy**2)
					
					vetoile(i,j) = v(i,j) -u(i,j)*dt/(6*dx)*(4*(v(i+1,j)-v(i-1,j))-0.5*(v(i+2,j)-v(i-2,j))) &
						-v(i,j)*dt/(6*dx)*(4*(v(i,j+1)-v(i,j-1))-0.5*(v(i,j+2)-v(i,j-2))) &
					     	+ nu*dt*((v(i+1,j)-2.0*v(i,j)+v(i-1,j))/dx**2 + &
					     	(v(i,j+1)-2.0*v(i,j)+v(i,j-1))/dy**2)
				endif

			ENDDO				
		ENDDO		
		
		uetoile(0,0:Ny-2) = 0.0
		uetoile(Nx-1,0:Ny-2) = 0.0
		uetoile(0:Nx-1,Ny-1) = Vmax
		uetoile(0:Nx-1,0) = 0.0
		
		vetoile(0,0:Ny-1) = 0.0
		vetoile(Nx-1,0:Ny-1) = 0.0
		vetoile(0:Nx-1,Ny-1) = 0.0
		vetoile(0:Nx-1,0) = 0.0	
	return	
	end subroutine
		
!-----------------------------------------------------------------------------------------------
	!discrétisation CDS-2
	subroutine CDS2()
		implicit none
	
		integer :: i,j
		
			
		DO j = 1,Ny-2  		
   	        	DO i = 1,Nx-2
				uetoile(i,j) = u(i,j) -u(i,j)*dt/(2*dx)*(u(i+1,j)-u(i-1,j)) &
						-v(i,j)*dt/(2*dy)*(u(i,j+1)-u(i,j-1)) &
					     + nu*dt*((u(i+1,j)-2.0*u(i,j)+u(i-1,j))/dx**2 + &
					     (u(i,j+1)-2.0*u(i,j)+u(i,j-1))/dy**2)
					
				vetoile(i,j) = v(i,j) -u(i,j)*dt/(2*dx)*(v(i+1,j)-v(i-1,j)) &
					-v(i,j)*dt/(2*dy)*(v(i,j+1)-v(i,j-1)) &
					     + nu*dt*((v(i+1,j)-2.0*v(i,j)+v(i-1,j))/dx**2 + &
					     (v(i,j+1)-2.0*v(i,j)+v(i,j-1))/dy**2)
			ENDDO				
		ENDDO			

		uetoile(0,0:Ny-2) = 0.0
		uetoile(Nx-1,0:Ny-2) = 0.0
		uetoile(0:Nx-1,Ny-1) = Vmax
		uetoile(0:Nx-1,0) = 0.0
			
		vetoile(0,0:Ny-1) = 0.0
		vetoile(Nx-1,0:Ny-1) = 0.0
		vetoile(0:Nx-1,Ny-1) = 0.0
		vetoile(0:Nx-1,0) = 0.0
	return	
	end subroutine
		
!-----------------------------------------------------------------------------------------------
	!discrétisation upwind
	subroutine upwind()
		implicit none
	
		integer :: i,j
		
			
		DO j = 1,Ny-2  		
   	        	DO i = 1,Nx-2
				uetoile(i,j) = u(i,j) -u(i,j)*dt/dx*(u(i,j)-u(i-1,j)) -v(i,j)*dt/dy*(u(i,j)-u(i,j-1)) &
					     + nu*dt*((u(i+1,j)-2.0*u(i,j)+u(i-1,j))/dx**2 + &
					     (u(i,j+1)-2.0*u(i,j)+u(i,j-1))/dy**2)
					
				vetoile(i,j) = v(i,j) -u(i,j)*dt/dx*(v(i,j)-v(i-1,j)) -v(i,j)*dt/dy*(v(i,j)-v(i,j-1)) &
					     + nu*dt*((v(i+1,j)-2.0*v(i,j)+v(i-1,j))/dx**2 + &
					     (v(i,j+1)-2.0*v(i,j)+v(i,j-1))/dy**2)	
			ENDDO				
		ENDDO			
		
		uetoile(0,0:Ny-2) = 0.0
		uetoile(Nx-1,0:Ny-2) = 0.0
		uetoile(0:Nx-1,Ny-1) = Vmax
		uetoile(0:Nx-1,0) = 0.0
		
		vetoile(0,0:Ny-1) = 0.0
		vetoile(Nx-1,0:Ny-1) = 0.0
		vetoile(0:Nx-1,Ny-1) = 0.0
		vetoile(0:Nx-1,0) = 0.0
	return	
	end subroutine
!----------------------------------------------------------------------------------------------

	subroutine contrainteP()
		implicit none
		
		integer :: i,j
		real :: moyennep
		
		!reshape a partir de pj calcule par jacobi
		do j=0,Ny-1
			do i=0,Nx-1
				p(i,j) = pj(i+j*Nx)
			end do
		end do
		
		!on calcule la moyenne
	    
	 	moyennep=0.0
	    	do j = 0, Ny-1
			do i = 0, Nx-1
		
				moyennep = moyennep + p(i,j)*dV(i,j)
			end do
	    	end do
	
		!on enleve la valeur de la moyenne calcule a chaque point pour mettre la moyenne a 0
		
		p(0:Nx-1,0:Ny-1) = p(0:Nx-1,0:Ny-1) - moyennep
		
			
	
	end subroutine contrainteP


!-----------------------------------------------------------------------------------------------

	subroutine termepoisson()
		integer :: i,j,k
		DO j = 1,Ny-2
         		DO i = 1,Nx-2
				termedroitepoisson(i,j) = rho/dt *((uetoile(i+1,j)-uetoile(i-1,j))/(2.0*dx) &
				+(vetoile(i,j+1)-vetoile(i,j-1))/(2.0*dy))
									
			ENDDO
		ENDDO
			
		termedroitepoisson(0,0:Ny-1) = 0.0
		termedroitepoisson(Nx-1,0:Ny-1) = 0.0
		termedroitepoisson(0:Nx-1,0) = 0.0
		termedroitepoisson(0:Nx-1,Ny-1) = 0.0
		
		DO j = 0,Ny-1
         		DO i = 0,Nx-1
			k=i+j*Nx
   		        b(k) = termedroitepoisson(i,j)
   		        pj(k) = p(i,j)
   		       end do
   		end do
			
	
	end subroutine
	
!----------------------------------------------------------------------------------------------

	subroutine correction()
		integer :: i,j
		DO j = 1,Ny-2
	       		DO i = 1,Nx-2
				us(i,j) = uetoile(i,j) -dt/rho * (p(i+1,j)-p(i-1,j))/(2.0*dx)	
				vs(i,j) = vetoile(i,j) -dt/rho * (p(i,j+1)-p(i,j-1))/(2.0*dy) 			
			ENDDO
		ENDDO
			
		us(0,0:Ny-2) = 0.0
		us(Nx-1,0:Ny-2) = 0.0
		us(0:Nx-1,Ny-1) = Vmax
		us(0:Nx-1,0) = 0.0
			
		vs(0,0:Ny-1) = 0.0
		vs(Nx-1,0:Ny-1) = 0.0
		vs(0:Nx-1,Ny-1) = 0.0
		vs(0:Nx-1,0) = 0.0
						
		u(0:Nx-1,0:Ny-1)=us(0:Nx-1,0:Ny-1)
		v(0:Nx-1,0:Ny-1)=vs(0:Nx-1,0:Ny-1)
	
	end subroutine
!----------------------------------------------------------------------
	subroutine calculdt()
		implicit none
		
		real :: dtFo_nu_x,dtFo_nu_y,dtCFL_x,dtCFL_y
		integer :: i,j
		
		!initialisation avec le plus grand dt possible pour utiliser les min
		dt=tf-t
	
		!comparaison avec les differents criteres de stabilite pour prendre le plus petit dt possible
		do i=0,Nx-1
				dtFo_nu_x = Fo*dx**2/nu
				dt = min(dt,dtFo_nu_x)
		end do
		
		do j=0,Ny-1
				dtFo_nu_y = Fo*dy **2/nu
				dt = min(dt,dtFo_nu_y)
		end do

		do j=0,Ny-1
			do i=0,Nx-1
				dtCFL_x = CFL*dx/max(abs(u(i,j)),1e-10)
				dtCFL_y = CFL*dy /max(abs(v(i,j)),1e-10)
				
				dt = min(dt,dtCFL_x,dtCFL_y)
			end do
		end do
		
		!pour s'arreter exactement a tf
		if (t+dt>tf) then
			dt=tf-t
		end if
		
		
	end subroutine calculdt
!-----------------------------------------------------------------------------------------------
	!avancement temporel du système
	subroutine temporel()
	implicit none
	
		integer :: i,j,k
		real :: maxu,maxv
		
		t=0.0
		necriture=0
		
		nu = L*Vmax/Re

		CALL remplissagematrice()
		do while (t<tf)
			
			call calculdt()
			t=t + dt
			if (mod(necriture,10) == 0) then
				write(*,"(F5.1,A)") 100.0*t/tf ,' %'
			end if
			
			necriture=necriture+1
			
			if (type_res=='CDS4') then
				CALL CDS4()
			else if (type_res=='CDS2') then
				CALL CDS2()
			else if (type_res=='UPWI') then
				CALL upwind()
			end if

			CALL termepoisson()
			CALL jacobi()
			CALL contrainteP()
			
			CALL correction()	
			
			
			if (mod(necriture,frequence) == 0) then
				call ecriture()
			end if
		
		ENDDO
		write(*,"(F5.1,A)") 100.0*t/tf ,' %'	
	return	
	end subroutine
!-----------------------------------------------------------------------------------------------	
	! methode de jacobi
	subroutine jacobi()
		implicit none
		
		real :: somme,norme_bas,norme_haut,critere
        	integer :: i
				
		critere = 1.0
		do while (critere > rtol)
			
			norme_haut=0.0
			norme_bas=0.0
			
			do i=0,N-1
				somme=0.0
				
				!pas de boucle sur j puisque la plupart des nombres dans la matrice sont des 0
				!on simplifie la somme en allant directement aux valeurs de mat non nulles
				
				if (i-Nx>=0) then
					somme = somme + mat(i,i-Nx)*pj(i-Nx)
				end if	
				if (i-1>=0) then				
					somme = somme + mat(i,i-1)*pj(i-1)
				end if		
				if (i+1<=N-1) then		
					somme = somme + mat(i,i+1)*pj(i+1)
				end if		
				if (i+Nx<=N-1) then	
					somme = somme + mat(i,i+Nx)*pj(i+Nx)
				end if		

				psj(i) = (b(i)-somme)/mat(i,i)
				
				norme_haut = norme_haut + (psj(i)-pj(i))**2
				norme_bas = norme_bas + (psj(i))**2
					
			end do
		
			pj(0:N-1) = psj(0:N-1)
			
			critere = sqrt(norme_haut / norme_bas)
			
		end do
		
	return
	end subroutine
	
!-------------------------------------------------


	subroutine ecriture()
	implicit none
		INTEGER :: i,j,k
		character(50) :: filename
		
		!on ecrit dans des fichiers differents pour chaque discretisation en .tec
		if (type_res == 'UPWI') then 
			write(filename,'(A,I0,A4)') 'resultat/UPWIND/ite',necriture,'.tec'
		else if (type_res == 'CDS2') then
			write(filename,'(A,I0,A4)') 'resultat/CDS2/ite',necriture,'.tec'
		else if (type_res == 'CDS4') then
			write(filename,'(A,I0,A4)') 'resultat/CDS4/ite',necriture,'.tec'
		end if	
		
     		open(10,file=filename,status='replace')
     				
		write(10,*)'TITLE = "res_etape7"'
  		write(10,*)'VARIABLES = "X", "Y", "U", "V", "P"'
  		write(10,*)'ZONE T = "',t,'seconds", I =',Nx,', J =',Ny,', DATAPACKING = POINT'
  		
  		do j = 0,Ny-1	
			do i = 0,Nx-1
				
				write(10,'(5F20.5)') x(i),y(j),u(i,j),v(i,j),p(i,j)
				
			end do
		end do
		
		close(10)
		
	return
	end subroutine ecriture

end module global

!-------------- PROGRAMME ------------------------------------------------
!-------------------------------------------------------------------------


program main

	use global
	implicit none
	
	integer :: start_time,rate,end_time
	real :: time
	
	call system_clock(start_time,rate)
	
	call lecture()
	call def_maill()
	call cond_init()
	call temporel()	
	call ecriture()
	
	call system_clock(end_time)
	time = real(end_time-start_time)/real(rate)
	open(11,file='output.txt',status='replace')
	write(11,"(I0)") necriture
	write(11,"(F10.2)") time
	close(11)
	
	deallocate( p )
	deallocate( mat )
	deallocate(u)
	deallocate(us)
	deallocate(v)
	deallocate(vs)
	deallocate(termedroitepoisson)
	deallocate(uetoile)
	deallocate(vetoile)
	deallocate( x )
	deallocate( y )
	deallocate( pj )
	deallocate( psj)
				
end program main
