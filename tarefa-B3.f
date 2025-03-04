      !Método de euler-cromer.
      subroutine penduloamortecido(g, r, n, deltat, theta, w, gama)
        !Transformando todas as variáveis que inicalmente são real*4 em
        !real*8.
        implicit real*8 (a-h, o-z)
        !Definindo o valor inical de theta.
        thetai = theta
        !Definindo o valor inical de omega "w".
        wi = w
        !Definindo o ciclo que irá calcular a posição angular do pêndulo
        !em cada tempo diferente.
        do i = 1, n
          !Calculando o valor de t em segundo para cada iteração.
          t = (i-1)*deltat
          !Imprimindo o valor de theta para cada tempo.
          write(2,*) t, thetai
          !Calculando o valor de w para t mais deltat.
          wimais1 = wi-((g/r)*dsin(thetai)+wi*gama)*deltat
          !Calculando o valor de theta para t mais deltat.
          thetaimais1 = thetai + wimais1*deltat
          !Definindo o novo valor para omega incial, para que se possa
          !ir para o próximo ciclo.
          wi = wimais1
          !Defininfo o novo valor para theta inicial, para que se possa
          !ir para o próximo ciclo.
          thetai = thetaimais1
        end do
      end subroutine penduloamortecido
!-----------------------------------------------------------------------
      !Iniciando o programa que será executado.
      program main
        !Transformando todas as variáveis que inicalmente são real*4 em
        !real*8.
        implicit real*8 (a-h, o-z)
        !Definindo o valor para a aceleração da gravidade.
        g = 9.8d0
        !Definindo o valor para o raio do pêndulo.
        r = 9.8d0
        !Abrindo o arquivo que receberá os dados que foram obtidos pelo
        !programa.
        open(2, file = 'saída-1-tarefa-B3.')
        !Chamando a subroutine que irá fazer todos os cálculos
        call penduloamortecido(g,r,10000,0.01d0,0.261799d0,0.d0,
     &1/2.d0)
        close(2)
      end program main

