unit EcoSenha.View.Main;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  System.Skia,
  FMX.Effects,
  FMX.Skia,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  DateUtils,
  FMX.Objects,
  FMX.Ani,
  FMX.Layouts,
  FMX.Platform;

type
  TFormMain = class(TForm)
    lytClient: TLayout;
    rectClient: TRectangle;
    lytTop: TLayout;
    rectTop: TRectangle;
    btnClose: TSpeedButton;
    svgClose: TSkSvg;
    lblAppName: TSkLabel;
    btnConfig: TSpeedButton;
    svgConfig: TSkSvg;
    svgAppIcon: TSkSvg;
    lytPassword: TLayout;
    rectSenhaAtual: TRectangle;
    lblAtual: TSkLabel;
    lblSenhaAtual: TSkLabel;
    lblPeriodoAtual: TSkLabel;
    shadowSenhaAtual: TShadowEffect;
    rectProximaSenha: TRectangle;
    lblProximo: TSkLabel;
    lblProximaSenha: TSkLabel;
    lblProximoPeriodo: TSkLabel;
    shadowProximaSenha: TShadowEffect;
    shadowForm: TShadowEffect;
    rectSenhaOutroPeriodo: TRectangle;
    lbltSenhaOutroPeriodo: TSkLabel;
    shadowSenhaOutroPeriodo: TShadowEffect;
    procedure EscurecerMouseEnter(Sender: TObject);
    procedure ClarearMouseLeave(Sender: TObject);
    procedure EscurecerClick(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure ClarearClick(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure DragMoveForm(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure btnCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure rectSenhaAtualClick(Sender: TObject);
    procedure rectProximaSenhaClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure rectSenhaOutroPeriodoClick(Sender: TObject);
  private
    procedure CopiarSenha(Senha: TSkLabel);
  public

  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}


uses
  EcoSenha.Controller.Senha,
  EcoSenha.View.Periodo;

{ TFormMain }

procedure TFormMain.btnCloseClick(Sender: TObject);
begin
  Close
end;

procedure TFormMain.ClarearClick(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  TAnimator.AnimateFloat(Sender as TFmxObject, 'Opacity', 0.8, 0.2)
end;

procedure TFormMain.ClarearMouseLeave(Sender: TObject);
begin
  TAnimator.AnimateFloat(Sender as TFmxObject, 'Opacity', 1)
end;

procedure TFormMain.CopiarSenha(Senha: TSkLabel);
begin
  var
  ClipboardService := IFMXClipboardService
    (TPlatformServices.Current.GetPlatformService(IFMXClipboardService));
  ClipboardService.SetClipboard(Senha.Text);
end;

procedure TFormMain.DragMoveForm(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  StartWindowDrag
end;

procedure TFormMain.EscurecerClick(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  TAnimator.AnimateFloat(Sender as TFmxObject, 'Opacity', 1, 0.2)
end;

procedure TFormMain.EscurecerMouseEnter(Sender: TObject);
begin
  TAnimator.AnimateFloat(Sender as TFmxObject, 'Opacity', 0.8)
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  var
  SenhaAtual := TSenha.New;

  lblSenhaAtual.Text := SenhaAtual.SenhaAtual;
  lblProximaSenha.Text := SenhaAtual.ProximaSenha;

  lblPeriodoAtual.Text := Format('%s at� %s',
    [Copy(DateToStr(SenhaAtual.InicioPeriodoAtual), 0, 2),
    DateToStr(SenhaAtual.FimPeriodoAtual)]);

  lblProximoPeriodo.Text := Format('%s at� %s',
    [Copy(DateToStr(SenhaAtual.InicioProximoPeriodo), 0, 2),
    DateToStr(SenhaAtual.FimProximoPeriodo)]);

end;

procedure TFormMain.FormKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Key = vkEscape then
    Close
end;

procedure TFormMain.rectProximaSenhaClick(Sender: TObject);
begin
  CopiarSenha(lblProximaSenha);
end;

procedure TFormMain.rectSenhaAtualClick(Sender: TObject);
begin
  CopiarSenha(lblSenhaAtual);
end;

procedure TFormMain.rectSenhaOutroPeriodoClick(Sender: TObject);
begin
  var
  lFormPeriodo := TFormPeriodo.Create(Self);
  try
    lFormPeriodo.ShowModal
  finally
    lFormPeriodo.Free;
  end;
end;

end.
