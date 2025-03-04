      !Método de euler-cromer.
      subroutine penduloamortecidoforcado(pi,g,r,n,deltat,Fo,omega,
     &theta,w)
        !Transfomando as variáveis que são real*4 por padrão em real*8.
        implicit real*8 (a-h, o-z)
        !Definindo o valor inical de theta.
        thetai = (theta*2*pi)/360
        !Definindo o valor inical da velocidade angular.
        wi = w
        !Inciando o ciclo que irá calcular o theta para diferentes
        !valores de t.
        icont = 1
        do i = 1, n
          !Utilizando a condicional para manter theta entre -pi e pi.
          if (thetai > pi) then
            thetai = thetai - 2*pi
          else if (thetai < -pi) then
            thetai = thetai + 2*pi
          end if
          t = (i-1)*deltat
          if (abs(t-((icont*pi)/omega)) < deltat/2) then
            !Imprimindo no documento de saída o valor obtido para a
            !velocidade angular em função de theta.
            write(2,*) thetai, wi
            icont = icont + 1
          end if
          !Calculando o valor de w, para t mais deltat.
          wimais1 = wi-((g/r)*dsin(thetai)+(wi/2)-Fo*
     &dsin(omega*i*deltat))*deltat
          !Calculando o valor de theta para t mais deltat.
          thetaimais1 = thetai + wimais1*deltat
          !Definindo o novo valor inicial da velocidade angular.
          wi = wimais1
          !Definindo o novo valor inical de theta.
          thetai = thetaimais1
        end do
      end subroutine penduloamortecidoforcado
!-----------------------------------------------------------------------
      !Iniciando a execução do programa.
      program main
        !Transformando as variáveis que são real*4 por padrão em real*8.
        implicit real*8 (a-h, o-z)
        !Atribuindo o valor de pi a variável "pi".
        pi = dacos(-1.d0)
        !Definindo o valor da aceleração da gravidade.
        g = 9.8d0
        !Definindo o valor do comprimento da haste.
        r = 9.8d0
        !Abrindo o arquivo no qual serão salvos os resultados obtidos.
        open(2, file = 'saída-1-tarefa-E.')
        !Chamando a subroutine que fará todos os cálculo para Fo=0.5.
        call penduloamortecidoforcado(pi,g,r,10000000,0.03d0,
     &0.5d0,2/3.0d0,15.0d0,0.0d0)
        close(2)
        !Abrindo o arquivo no qual serão salvos os resultados obtidos.
        open(2, file = 'saída-2-tarefa-E.')
        !Chamando a subroutine que fará todos os cálculo para Fo=0.5.
        call penduloamortecidoforcado(pi,g,r,10000000,0.03d0,
     &0.5d0,2/3.0d0,15.1d0,0.0d0)
        close(2)
        !Abrindo o arquivo no qual serão salvos os resultados obtidos.
        open(2, file = 'saída-3-tarefa-E.')
        !Chamando a subroutine que fará todos os cálculo para Fo=0.5.
        call penduloamortecidoforcado(pi,g,r,10000000,0.03d0,
     &0.5d0,2/3.0d0,14.9d0,0.001d0)
        close(2)
        !Abrindo o arquivo no qual serão salvos os resultados obtidos.
        open(2, file = 'saída-4-tarefa-E.')
        !Chamando a subroutine que fará todos os cálculo para Fo=1.2.
        call penduloamortecidoforcado(pi,g,r,10000000,0.03d0,
     &1.2d0,2/3.0d0,15.0d0,0.0d0)
        close(2)
        !Abrindo o arquivo no qual serão salvos os resultados obtidos.
        open(2, file = 'saída-5-tarefa-E.')
        !Chamando a subroutine que fará todos os cálculo para Fo=1.2.
        call penduloamortecidoforcado(pi,g,r,10000000,0.03d0,
     &1.2d0,2/3.0d0,15.1d0,0.0d0)
        close(2)
        !Abrindo o arquivo no qual serão salvos os resultados obtidos.
        open(2, file = 'saída-6-tarefa-E.')
        !Chamando a subroutine que fará todos os cálculo para Fo=1.2.
        call penduloamortecidoforcado(pi,g,r,10000000,0.03d0,
     & 1.2d0,2/3.0d0,14.9d0,0.001d0)
        close(2)
      end program main
