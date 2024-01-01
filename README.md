# EcoSenha
# EcoSenha.Controller.Senha

Este código Delphi define uma interface `ISenha` e uma classe `TSenha` que a implementa. O propósito é gerar e gerenciar senhas para períodos específicos.

## ISenha Interface

A interface `ISenha` define os seguintes métodos:

- `SenhaAtual`: Retorna a senha atual para o período em vigor.
- `ProximaSenha`: Gera a senha para o próximo período.
- `InicioPeriodoAtual`: Retorna a data e hora de início do período atual.
- `FimPeriodoAtual`: Retorna a data e hora de término do período atual.
- `InicioProximoPeriodo`: Retorna a data e hora de início do próximo período.
- `FimProximoPeriodo`: Retorna a data e hora de término do próximo período.

## TSenha Classe

A classe `TSenha` implementa a interface `ISenha`. Principais métodos:

- `GetMD5`: Método privado para calcular o hash MD5 de uma string.
- `New`: Método de classe para criar uma nova instância da classe.
- `SenhaAtual` e `ProximaSenha`: Métodos para gerar senhas baseadas em um padrão específico.
- Métodos relacionados a datas e horas para manipulação de períodos.

## Uso

Essencialmente, a classe `TSenha` fornece um mecanismo para gerar senhas que mudam de acordo com o tempo. A implementação utiliza funções da biblioteca Delphi, como `DayOfTheMonth`, `StartOfTheMonth`, `EndOfTheMonth`, etc., e a biblioteca Indy para cálculo de hash MD5.

## Exemplo

```delphi
// Exemplo de uso
var
  senhaService: ISenha;
begin
  senhaService := TSenha.New;

  Writeln('Senha Atual: ', senhaService.SenhaAtual);
  Writeln('Próxima Senha: ', senhaService.ProximaSenha);
end;
