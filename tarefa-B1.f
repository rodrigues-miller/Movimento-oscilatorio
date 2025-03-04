      !Método de euler-cromer.
      subroutine pendulo(pi, g, r, n, deltat, theta, w)
        !Transformando todas as variáveis que são real*4 por padrão em
        !real*8.
        implicit real*8 (a-h, o-z)
        !Definindo o valor inicial de theta, transformando de graus
        !para radianos.
        thetai = (theta*2*pi)/360
        !Definindo o valor inicial da velocidade angular.
        wi = w
        !Definindo o valor incial de um contador.
        icont = 0
        !A variável "cont" irá auxiliar a variável "icont" para um
        !caso especial.
        cont = 0.d0
        !Definindo o ciclo que calculará cada passo de tempo.
        do i = 1, n
          !Calculando a velocidade âgular (omega) , após um acréssimo
          !deltat ao tempo da velocidade angular calculada
          !anteriormente.
          wimais1 = wi - (g/r)*dsin(thetai)*deltat
          !Calculando o ângulo theta formado entre o pêndulo e a
          !vertical, após um acréssimo deltat ao tempo do ângulo
          !obtido anteriormente.
          thetaimais1 = thetai + wimais1*deltat
          !Colocando a condição para se calcular o período do pêndulo,
          !nas extremidades da oscilação, que é onde ocorre a inversão
          !do sinal da velocidade angular.
          if (wi*wimais1 < 0) then
            !Adicionando mais um ao valor do contador para se ter o
            !controle de quantas vezes o pênculo atingiu a extremidade
            !após ser solto.
            icont = icont + 1
            !Condição que informará quando o pêndulo atingiu duas
            !vezes a extremidade da oscilação, obtendo assim um
            !período.
            if (icont == 2) then
              !Imprimindo o valor do período no arquivo.
              write(2,100) theta, i*deltat
              !Dando o comando para sair do ciclo pois o resultado
              !desejado já foi obtido.
              exit
            end if
          !Condição para quando wimais1 é igual a 0. (Caso
          !especial).
          else if (wi*wimais1 == 0) then
              !Adicionando meio a variável cont pois wi irá ser 0
              !também.
              cont = cont + 1/2.d0
            !Quando cont atinge o valor 1 adiciona-se mais 1 a
            !variável icont.
            if (if cont == 1 .or. cont == 2) then
              icont = icont + 1
            end if
            !Condição que informará quando o pêndulo atingiu duas
            !vezes a extremidade da oscilação, obtendo assim um
            !período.
            if (icont == 2) then
              !Imprimindo o valor do período no arquivo.
              write(2,*) theta, i*deltat
              !Dando o comando para sair do ciclo pois o resultado
              !desejado já foi obtido.
              exit
            end if
          end if
          !Definindo o novo valor da velocidade ângular inicial.
          wi = wimais1
          !Definindo o novo valor do ângulo inicial.
          thetai = thetaimais1
        end do
        !Definindo o formato utilizado, para impressão dos dados no
        !arquivo, pela subroutine.
100     format('|',f7.2,1x,'|',f7.2,1x,'|')
      end subroutine pendulo
!-----------------------------------------------------------------------
      !Iniciando o programa que será executado.
      program main
        !Transformando todas as variáveis que são real*4 por padrão,
        !em real*8.
        implicit real*8 (a-h, o-z)
        !Atribuindo o valor de pi a variável "pi".
        pi = dacos(-1.d0)
        !Definindo o valor da aceleração da gravidade.
        g = 9.8d0
        !Definindo o valor do raio do pêndulo.
        r = 9.8d0
        !Abrindo o arquivo que irá conter os dados calculados.
        open(2, file = 'saída-1-tarefa-B1.')
        !Imprindo os dados no arquivo em formato de tabela.
        write(2,101)
        write(2,102)
        write(2,103)
        write(2,101)
        !Chamando a subroutine que irá fazer todos os cálculos.
        call pendulo(pi,g,r,10000,0.01d0,15.0d0,0.0d0)
        call pendulo(pi,g,r,10000,0.01d0,30.0d0,0.0d0)
        call pendulo(pi,g,r,10000,0.01d0,45.0d0,0.0d0)
        call pendulo(pi,g,r,10000,0.01d0,60.0d0,0.0d0)
        call pendulo(pi,g,r,10000,0.01d0,90.0d0,0.0d0)
        call pendulo(pi,g,r,10000,0.01d0,120.0d0,0.0d0)
        call pendulo(pi,g,r,10000,0.01d0,145.0d0,0.0d0)
        call pendulo(pi,g,r,10000,0.01d0,165.0d0,0.0d0)
        write(2,101)
        close(2)
        !Definindo os formatos utilizados no programa.
101     format('-------------------')
102     format('|Ângulo  |Período |')
103     format('| Inicial|        |')
      end program main
