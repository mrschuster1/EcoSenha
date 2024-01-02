unit EcoSenha.Controller.Senha;

interface

uses
  System.SysUtils,
  DateUtils,
  IdHashMessageDigest,
  IdGlobal,
  IdHash;

type
  /// <summary>
  /// Interface para geração e gerenciamento de senhas para um período específico.
  /// </summary>
  ISenha = interface
    ['{9EF2FB47-5265-414B-BD18-E978E7E9E73F}']

    /// <summary>
    /// Obtém a senha atual para o período ativo.
    /// </summary>
    function SenhaAtual: string;

    /// <summary>
    /// Gera a próxima senha para o próximo período.
    /// </summary>
    function ProximaSenha: string;

    /// <summary>
    /// Obtém a data e hora de início do período atual da senha.
    /// </summary>
    function InicioPeriodoAtual: TDateTime;

    /// <summary>
    /// Obtém a data e hora de término do período atual da senha.
    /// </summary>
    function FimPeriodoAtual: TDateTime;

    /// <summary>
    /// Obtém a data e hora de início do próximo período da senha.
    /// </summary>
    function InicioProximoPeriodo: TDateTime;

    /// <summary>
    /// Obtém a data e hora de término do próximo período da senha.
    /// </summary>
    function FimProximoPeriodo: TDateTime;
  end;

type
  /// <summary>
  /// Implementação da interface ISenha para geração de senhas.
  /// </summary>
  TSenha = class(TInterfacedObject, ISenha)
  private
    /// <summary>
    /// Calcula o hash MD5 do valor de string fornecido.
    /// </summary>
    class function GetMD5(const AValue: string): string;

  public
    /// <summary>
    /// Cria uma nova instância de ISenha.
    /// </summary>
    class function New: ISenha;

    /// <inheritdoc/>
    function SenhaAtual: string;

    /// <inheritdoc/>
    function ProximaSenha: string;

    /// <inheritdoc/>
    function InicioPeriodoAtual: TDateTime;

    /// <inheritdoc/>
    function FimPeriodoAtual: TDateTime;

    /// <inheritdoc/>
    function InicioProximoPeriodo: TDateTime;

    /// <inheritdoc/>
    function FimProximoPeriodo: TDateTime;
  end;

implementation

{ TSenha }

function TSenha.FimPeriodoAtual: TDateTime;
begin
  if DayOfTheMonth(Now) <= 15 then
    Result := (IncDay(StartOfTheMonth(Now), 15)) - OneHour
  else
    Result := EndOfTheMonth(Now);
end;

function TSenha.FimProximoPeriodo: TDateTime;
begin
  if DayOfTheMonth(Now) <= 15 then
    Result := EndOfTheMonth(Now)
  else
    Result := IncDay(StartOfTheMonth(IncMonth(Now)), 15);
end;

class function TSenha.GetMD5(const AValue: string): string;
begin
  var
  MD5Hash := TIdHashMessageDigest5.Create;
  try
    Result := MD5Hash.HashStringAsHex(AValue);
  finally
    MD5Hash.Free;
  end;
end;

function TSenha.InicioPeriodoAtual: TDateTime;
begin
  if DayOfTheMonth(Now) <= 15 then
    Result := StartOfTheMonth(Now)
  else
    Result := IncDay(StartOfTheMonth(Now), 15);
end;

function TSenha.InicioProximoPeriodo: TDateTime;
begin
  if DayOfTheMonth(Now) <= 15 then
    Result := (IncDay(EndOfTheMonth(Now), -16)) + OneSecond
  else
    Result := StartOfTheMonth(IncMonth(Now));
end;

class function TSenha.New: ISenha;
begin
  Result := Self.Create;
end;

function TSenha.ProximaSenha: string;
begin
  Result := 'ECO' + Copy(GetMD5('supervisor_' + FormatDateTime('yyyyMMdd',
    InicioProximoPeriodo)), 1, 3);
end;

function TSenha.SenhaAtual: string;
begin
  Result := 'ECO' + Copy(GetMD5('supervisor_' + FormatDateTime('yyyyMMdd',
    InicioPeriodoAtual)), 1, 3);
end;

end.
