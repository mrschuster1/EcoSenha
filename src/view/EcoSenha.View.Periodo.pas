unit EcoSenha.View.Periodo;

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
  FMX.Objects,
  FMX.Layouts,
  FMX.TMSFNCTypes,
  FMX.TMSFNCUtils,
  FMX.TMSFNCGraphics,
  FMX.TMSFNCGraphicsTypes,
  FMX.TMSFNCCustomControl,
  FMX.TMSFNCCustomPicker,
  FMX.TMSFNCDatePicker,
  FMX.TMSFNCCalendar,
  FMX.Ani,
  EcoSenha.Components.CustomCalendar;

type
  TFormPeriodo = class(TForm)
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
    lytCalendario: TLayout;
    shadowForm: TShadowEffect;
    layoutNomeMes: TLayout;
    lblNomeMes: TSkLabel;
    btnNext: TSpeedButton;
    svgNext: TSkSvg;
    btnPrev: TSpeedButton;
    svgPrev: TSkSvg;
    lblSenha: TSkLabel;
    rectSenhaOutroPeriodo: TRectangle;
    lblConfirmar: TSkLabel;
    shadowSenhaOutroPeriodo: TShadowEffect;
    rectHoje: TRectangle;
    lblHoje: TSkLabel;
    ShadowEffect1: TShadowEffect;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure btnCloseClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnPrevClick(Sender: TObject);
    procedure EscurecerMouseEnter(Sender: TObject);
    procedure ClarearMouseLeave(Sender: TObject);
    procedure EscurecerClick(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure ClarearClick(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure rectSenhaOutroPeriodoClick(Sender: TObject);
    procedure rectHojeClick(Sender: TObject);
  private
    Calendario: TCustomCalendar;
    procedure DayClick(Sender: TObject);
    procedure MesAnterior;
    procedure ProximoMes;
    procedure AtualizarCalendario;
    procedure AtualizarSenha;
    procedure CriarCalendario;
  public

  end;

var
  FormPeriodo: TFormPeriodo;

implementation

{$R *.fmx}


uses
  EcoSenha.Controller.Senha;

procedure TFormPeriodo.AtualizarCalendario;
begin
  lblNomeMes.Text := Calendario.MonthName;
  AtualizarSenha
end;

procedure TFormPeriodo.AtualizarSenha;
begin
  lblSenha.Text := 'Senha: ' + TSenha.New.Data(Calendario.SelectedDate)
    .SenhaAtual;
end;

procedure TFormPeriodo.CriarCalendario;
begin
  if Assigned(Calendario) then
    Calendario.DisposeOf;
  Calendario := TCustomCalendar.Create(lytCalendario);
  Calendario.OnClick := DayClick;
  Calendario.DayFontSize := 14;
  Calendario.DayFontColor := TAlphaColors.Black;
  Calendario.SelectedDayColor := 4282218228;
  Calendario.BackgroundColor := TAlphaColors.White;
  Calendario.ShowCalendar;
  AtualizarCalendario;
end;

procedure TFormPeriodo.btnCloseClick(Sender: TObject);
begin
  Close
end;

procedure TFormPeriodo.btnNextClick(Sender: TObject);
begin
  ProximoMes
end;

procedure TFormPeriodo.btnPrevClick(Sender: TObject);
begin
  MesAnterior
end;

procedure TFormPeriodo.ClarearClick(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  TAnimator.AnimateFloat(Sender as TFmxObject, 'Opacity', 0.8, 0.2)
end;

procedure TFormPeriodo.ClarearMouseLeave(Sender: TObject);
begin
  TAnimator.AnimateFloat(Sender as TFmxObject, 'Opacity', 1)
end;

procedure TFormPeriodo.DayClick(Sender: TObject);
begin
  AtualizarSenha;
end;

procedure TFormPeriodo.EscurecerClick(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  TAnimator.AnimateFloat(Sender as TFmxObject, 'Opacity', 1, 0.2)
end;

procedure TFormPeriodo.EscurecerMouseEnter(Sender: TObject);
begin
  TAnimator.AnimateFloat(Sender as TFmxObject, 'Opacity', 0.8)
end;

procedure TFormPeriodo.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Calendario.DisposeOf
end;

procedure TFormPeriodo.FormKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Key = vkEscape then
    Close
end;

procedure TFormPeriodo.FormShow(Sender: TObject);
begin
  CriarCalendario;
end;

procedure TFormPeriodo.MesAnterior;
begin
  Calendario.PriorMonth;
  AtualizarCalendario;
end;

procedure TFormPeriodo.ProximoMes;
begin
  Calendario.NextMonth;
  AtualizarCalendario;
end;

procedure TFormPeriodo.rectHojeClick(Sender: TObject);
begin
  CriarCalendario
end;

procedure TFormPeriodo.rectSenhaOutroPeriodoClick(Sender: TObject);
begin
  Close
end;

end.
