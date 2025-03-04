      !Método de euler.
      subroutine penduloeuler(pi, n, deltat, g, r, theta, w)
        !Mudando todas variáveis que são real*4 por padrão como real*8.
        implicit real*8 (a-h,o-z)
        !Definindo o valor inicial de theta e omega.
        thetai = theta
        wi = w
        !Iniciando o looping que calculará cada valor de theta e da
        !energia para n passos de tempo.
        do i = 1, n
          !Calculando o tempo em que o pêndulo está em certa posição
          !angular.
          t = (i-1)*deltat
          !Imprimindo no arquivo de saída o valor do tempo e o valor de
          !theta.
          write(2,*) t, thetai
          !Calculando a energia total do sistema em cada ponto do
          !sistema.
          energia = ((wi*9.8)**2)/2 + r*(1-dcos(thetai))*g
          !Imprimindo na tela o valor da energia que foi obtido, em
          !função do tempo.
          write(3,*) t, energia
          !Calculando o valor da velocidade angular após um incremento
          !de tempo deltat.
          wimais1 = wi - (g/r)*thetai*deltatd
          !Calculando o valor do ângulo que o pêndulo faz com a vertical
          !após um incremento de tempo deltat.
          thetaimais1 = thetai + wi*deltat
          !Reajustando o valor da velocidade inicial que será utilizado
          !no próximo passo.
          wi = wimais1
          !Reajustando o valor de theta inicial que será utilizado no
          !pŕoximo passo.
          thetai = thetaimais1
        end do
      end subroutine penduloeuler
!-----------------------------------------------------------------------
      !Método de euler-cromer.
      subroutine penduloeulercromer(pi, n, deltat, g, r, theta, w)
        !Mudando todas variáveis que são real*4 por padrão como real*8.
        implicit real*8 (a-h,o-z)
        !Definindo o valor inicial de theta e omega.
        thetai = theta
        wi = w
        !Iniciando o looping que calculará cada valor de theta e da
        !energia para n passos de tempo.
        do i = 1, n
          !Calculando o tempo em que o pêndulo está em certa posição
          !angular.
          t = (i-1)*deltat
          !Imprimindo no arquivo de saída o valor do tempo e o valor
          !de theta.
          write(2,*) t, thetai
          !Calculando a energia total do sistema em cada ponto do
          !sistema.
          energia =((wi*r)**2)/2 + r*(1-dcos(thetai))*g
          !Imprimindo no arquivo de saída o valor da energia que foi
          !obtida, em função do tempo.
          write(3,*) t, energia
          !Calculando o valor da velocidade angular após um incremento
          !de tempo deltat.
          wimais1 = wi - (g/r)*thetai*deltat
          !Calculando o valor do ângulo que o pêndulo faz com a vertical
          !após um intervalo de tempo deltat.
          thetaimais1 = thetai + wimais1*deltat
          !Reajustando o valor da velocidade inicial que será utilizada
          !no próximo passo.
          wi = wimais1
          !Reajustando o valor de theta inicial que será utilizado no
          !próximo passo.
          thetai = thetaimais1
        end do
      end subroutine penduloeulercromer
!-----------------------------------------------------------------------
      program main
        !Mudando todas variáveis que são real*4 por padrão como real*8.
        implicit real*8 (a-h,o-z)
        !Atribuindo o valor de pi a variável "pi"..
        pi = dacos(-1d0)
        !Definindo o valor da aceleração gravitacional.
        g = 9.8d0
        !Definindo o valor do comprimento da haste.
        r = 9.8d0
        !Abrindo os arquivos que irão conter os dados obtidos pelo
        !programa.
        open(2, file = 'saída-1-tarefa-A.')
        open(3, file = 'saída-2-tarefa-A.')
        !Chamando a subroutine que possui o método de euler.
        call penduloeuler(pi,10000,0.01d0,g,r,0.261799d0,0.0d0)
        close(2)
        close(3)
        !Abrindo os arquivos que irão conter os dados obtidos pelo
        !programa.
        open(2, file = 'saída-3-tarefa-A.')
        open(3, file = 'saída-4-tarefa-A.')
        !Chamando a subroutine que possui o método de euler-cromer.
        call penduloeulercromer(pi,10000,0.01d0,g,r,0.261799d0,0.0d0)
        close(2)
        close(3)
      end program main
