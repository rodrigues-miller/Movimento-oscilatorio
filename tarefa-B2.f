      !Definindo a função que calculará o período de pequenas oscilações
      !oscilações do pêndulo utilizando o método de euler.
      function periodopendulo(pi,g,r,n,deltat,theta,w)
        !Transformando todas as variáveis que são real*4 por padrão, em
        !real*8.
        implicit real*8 (a-h, o-z)
        !Definindo o valor inicial de theta, transformando de graus para
        !radianos.
        thetai = (theta*2*pi)/360
        !Definindo o valor inicial da velocidade angular.
        wi = w
        !Definindo o valor incial do contador.
        icont = 0
        !Definindo o ciclo que calculará cada passo de tempo.
        do i = 1, n
          !Calculando a velocidade âgular (omega) , após um acréssimo
          !deltat ao tempo.
          wimais1 = wi - (g/r)*dsin(thetai)*deltat
          !Calculando o ângulo theta formado entre o pêndulo e a
          !vertical, após um acreśsimo deltat ao tempo.
          thetaimais1 = thetai + wimais1*deltat
          !Colocando a condição para se calcular o período do pênculo,
          !nas extremidades da oscilação, que é onde ocorre a inversão
          !do sinal da velocidade angular.
          if (wi*wimais1 < 0) then
            !Adicionando mais um ao valor do contador para se ter o
            !controle de quantas vezes o pêndulo atingiu a extremidade
            !após ser solto.
            icont = icont + 1
            !Condição que informará quando o pêndulo atingiu duas vezes
            !a extremidade da oscilação, obtendo assim um período.
            if (icont == 2) then
              !Atribuindo o valor do período à função.
              periodopendulo = i*deltat
              !Dando o comando para retornar o valor da função pois o
              !resultado desejado já foi obtido.
              return
            end if
          !Condição para quando wimais1 é igual a 0. (Caso especial).
          else if (wi*wimais1 == 0) then
            !Adicionando meio a variável cont pois wi irá ser 0 também.
            cont = cont + 1/2.d0
            !Quando cont atinge o valor 1 mais 1 a variável icont.
            if (if cont == 1 .or. cont == 2) then
              icont = icont + 1
            end if
            !Condição que informará quando o pêndulo atingiu duas vezes
            !a extremidade da oscilação, obtendo assim um período.
            if (icont == 2) then
              !Atribuindo o valor do período a função.
              periodopendulo = i*deltat
              !Dando o comando para retornar o valor da função pois o
              !resultado desejado já foi obtido.
              return
            end if
          end if
          !Definindo o novo valor da velocidade ângular inicial.
          !incial
          wi = wimais1
          !Definindo o novo valor do ângulo inicial.
          thetai = thetaimais1
        end do
      end function periodopendulo
!-----------------------------------------------------------------------
      !Definindo a função que calculará os períodos de pequenas
      !oscilações de forma analítica.
      function periodoanalitico(pi,g,r,theta)
        !Transformando as variáveis que são real*4 por padrão, em
        !real*8.
        implicit real*8 (a-h,o-z)
        !Definindo o valor de theta em radianos.
        thetai = (2*pi*theta)/360
        !Calculando o período para ângulos pequenos.
        periodoanalitico = 2*pi*sqrt(r/g)*(1+(thetai**2)/16)
        return
      end function periodoanalitico
!-----------------------------------------------------------------------
      !Iniciando o programa que será executado.
      program main
        !Transformando as variáveis que são real*4 por padrão, em
        !real*8.
        implicit real*8 (a-h,o-z)
        !Atribuindo o valor de pi a variável "pi".
        pi = dacos(-1.d0)
        !Definindo o valor da aceleração da gravidade.
        g = 9.8d0
        !Definindo o valor do comprimento da haste.
        r = 9.8d0
        !Abrindo o arquivo onde serão salvos os dados obtidos.
        open(2, file = 'saída-1-tarefa-B2.')
        !Imprimindo os dados no arquivo em formato de tabela.
        write(2,101)
        write(2,102)
        write(2,103)
        write(2,101)
        !Chamando as funções que calcularão os períodos e imprimirão no
        !arquivo de saída.
        write(2,100) 15d0, 
     &periodopendulo(pi,g,r,10000,0.001d0,15d0,0.0d0),
     &periodoanalitico(pi,g,r,15d0)
        write(2,100) 10d0,
     &periodopendulo(pi,g,r,10000,0.001d0,10.0d0,0.0d0),
     &periodoanalitico(pi,g,r,10.0d0)
        write(2,100) 5d0,
     &periodopendulo(pi,g,r,10000,0.001d0,5d0,0.0d0),
     &periodoanalitico(pi,g,r,5.0d0)
        write(2,101)
        close(2)
        !Definindo os formatos utilizados no programa.
100     format('|',f7.3,'|',f15.9,'|',f15.9,'|')
101     format('-----------------------------------------')
102     format('|Âmgulo |  Período da   |  Período      |')
103     format('|Inicial|     Simulação |     Calculado |')
      end program main
