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
        do i = 1, n
          !Inserindo a condição para que theta fique sempre entre 0 e
          !pi.
          if (thetai > pi) then
            thetai = thetai - 2*pi
          else if (thetai < -pi) then
            thetai = thetai + 2*pi
          end if
          !Calculando o valor de t anterior.
          t = (i-1)*deltat
          !Imprimindo no documento de saída o valor obtido para theta em
          !cada tempo t.
          write(2,*) t, thetai
          write(3,*) t, wi
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
      !Iniciando o programa que será executado pela máquina.
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
        open(2, file = 'saida-1-tarefa-B4.')
        open(3, file = 'saida-2-tarefa-B4.')
        !Chamando a subroutine que fará todos os cálculos para Fo=0.
        call penduloamortecidoforcado(pi,g,r,10000,0.03d0,0.0d0,
     &2/3.0d0, 15.0d0, 0.0d0)
        close(2)
        close(3)
        !Abrindo o arquivo no qual serão salvos os resultados obtidos.
        open(2, file = 'saida-3-tarefa-B4.')
        open(3, file = 'saida-4-tarefa-B4.')
        !Chamando a subroutine que fará todos os cálculos para Fo=0.5.
        call penduloamortecidoforcado(pi,g,r,10000,0.03d0,0.5d0,
     &2/3.0d0, 15.0d0, 0.0d0)
        close(2)
        close(3)
        !Abrindo o arquivo no qual serão salvos os resultados obtidos.
        open(2, file = 'saida-5-tarefa-B4.')
        open(3, file = 'saida-6-tarefa-B4.')
        !Chamando a subroutine que fará todos os cálculos para Fo=1.2.
        call penduloamortecidoforcado(pi,g,r,10000,0.03d0,1.2d0,
     &2/3.0d0, 15.0d0, 0.0d0)
        close(2)
        close(3)
      end program main
