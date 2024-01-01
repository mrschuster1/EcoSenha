unit EcoSenha.Controller.Senha;

interface

uses
  System.SysUtils,
  DateUtils,
  IdHashMessageDigest,
  IdGlobal,
  IdHash;

type
  ISenha = interface
    ['{9EF2FB47-5265-414B-BD18-E978E7E9E73F}']
    function SenhaAtual: string;
    function ProximaSenha: string;
    function InicioPeriodoAtual: TDateTime;
    function FimPeriodoAtual: TDateTime;
    function InicioProximoPeriodo: TDateTime;
    function FimProximoPeriodo: TDateTime;
  end;

type
  TSenha = class(TInterfacedObject, ISenha)
  private
    class function GetMD5(const AValue: string): string;

  public
    class function New: ISenha;
    function SenhaAtual: string;
    function ProximaSenha: string;
    function InicioPeriodoAtual: TDateTime;
    function FimPeriodoAtual: TDateTime;
    function InicioProximoPeriodo: TDateTime;
    function FimProximoPeriodo: TDateTime;

  end;

implementation

{ TSenha }

function TSenha.FimPeriodoAtual: TDateTime;
begin
  if DayOfTheMonth(Now) <= 15 then
    Result := (IncDay(StartOfTheMonth(Now), 15)) - OneHour
  else
    Result := EndOfTheMonth(Now)
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
  Result := Self.Create
end;

function TSenha.ProximaSenha: string;
begin
  Result := 'ECO' + Copy(GetMD5('supervisor_' +
    FormatDateTime('yyyyMMdd', InicioProximoPeriodo)), 1, 3);
end;

function TSenha.SenhaAtual: string;
begin
  Result := 'ECO' + Copy(GetMD5('supervisor_' +
    FormatDateTime('yyyyMMdd', Now)), 1, 3);
end;

end.
