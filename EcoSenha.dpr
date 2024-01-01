program EcoSenha;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Skia,
  EcoSenha.View.Main in 'src\view\EcoSenha.View.Main.pas' {FormMain},
  EcoSenha.Controller.Senha in 'src\controller\EcoSenha.Controller.Senha.pas';

{$R *.res}


begin
  ReportMemoryLeaksOnShutdown := true;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;

end.
