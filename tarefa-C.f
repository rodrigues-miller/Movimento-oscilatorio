      !Método de euler-cromer.
      subroutine pendulo(pi, g, r, deltat, n, theta, w, omega, Fo)
        !Transformando todas as variáveis que são real*4 por padrão em
        !real*8.
        implicit real*8 (a-h, o-z)
        !Definindo o valor inical para theta1.
        thetai = (theta*2*pi)/360
        !Definindo o valor inical para theta2.
        theta2i = thetai + 0.001d0
        !Definindo o valor inicial para a velocidade angular 1.
        wi = w
        !Definindo o valor inicial para a velocidade angular 2.
        w2i = w
        !Iniciando o ciclo que irá percorrer ao longo do tempo no
        !programa.
        do i = 1, n
          !Calculando o tempo em que a posição angular de cada pêndulo
          !se encontra.
          t = (i-1)*deltat
          !Calculando a diferença deltatheta em função do tempo.
          deltatheta = abs(theta2i - thetai)
          !Imprimindo a diferença deltatheta ao longo do tempo.
          write(2,*) t, deltatheta
          !Início do cálculo para theta1.
          !Calculando a velocidade angular para t + detat.
          wimais1 = wi-((g/r)*dsin(thetai)+(wi/2)-
     &Fo*dsin(omega*i*deltat))*deltat
          !Calculando a posição angular para t + deltat.
          thetaimais1 = thetai + wimais1*deltat
          !Definindo o novo valor da velocidade angular inicial.
          wi = wimais1
          !Definindo o novo valor da posição angular inicial.
          thetai = thetaimais1
          !Início do cálculo para theta2.
          !Calculando a velocidade angular para t + deltat.
          w2imais1 = w2i-((g/r)*dsin(theta2i)+(w2i/2)-
     &Fo*dsin(omega*i*deltat))*deltat
          !Calculando a posição angular para t + deltat.
          theta2imais1 = theta2i + w2imais1*deltat
          !Definindo o novo valor para a velocidade angular inicial.
          w2i = w2imais1
          !Definindo o novo valor para a posição angular inicial.
          theta2i = theta2imais1
        end do
      end subroutine pendulo
!-----------------------------------------------------------------------
      !Iniciando a execução do programa.
      program main
        !Transformando todas as variáveis que são real*4 pro padrão em
        !real*8.
        implicit real*8 (a-h, o-z)
        !Atribuindo o valor de pi a variável "pi".
        pi = dacos(-1d0)
        !Definindo o valor para a aceleração da gravidade.
        g = 9.8d0
        !Definindo o valor para o comprimento da haste.
        r = 9.8d0
        !Abrindo o arquivo onde serão armazenados os dados obtidos.
        open(2, file = 'saida-1-tarefa-C.')
        !Chamando a subroutine que fará todos os cálculos.
        call pendulo(pi,g,r,0.03d0,10000,15.0d0,0.0d0,2/3.0d0,
     &0.5d0)
        close(2)
        !Abrindo o arquivo onde serão armazenados os dados obtidos.
        open(2, file = 'saida-2-tarefa-C.')
        !Chamando a subroutine que fará todos os cálculos.
        call pendulo(pi,g,r,0.03d0,100000,15.0d0,0.d0,2/3d0,
     &1.2d0)
        close(2)
      end program
