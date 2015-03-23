{$I ACBr.inc}

unit ValidadorTeste1;

interface

uses
  SysUtils,
 {$IFDEF Delphi6_UP} Types, Variants, VarConv, {$ELSE} Windows,{$ENDIF}
  Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  ACBrBase, ACBrValidador;

type
  TfrValidador = class(TForm)
    ACBrValidador1: TACBrValidador;
    edDocto: TEdit;
    Label1: TLabel;
    cbTipoDocto: TComboBox;
    Label2: TLabel;
    mMsgErro: TMemo;
    Label3: TLabel;
    bValidar: TButton;
    Label4: TLabel;
    edComple: TEdit;
    Label5: TLabel;
    cbPermiteVazio: TCheckBox;
    cbAjustarTam: TCheckBox;
    bFormatar: TButton;
    cbException: TCheckBox;
    edIgnorar: TEdit;
    cbExibeDigCorreto: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure ACBrValidador1MsgErro(Mensagem: String);
    procedure bValidarClick(Sender: TObject);
    procedure bFormatarClick(Sender: TObject);
    procedure cbPermiteVazioClick(Sender: TObject);
    procedure cbAjustarTamClick(Sender: TObject);
    procedure cbExceptionClick(Sender: TObject);
    procedure cbTipoDoctoChange(Sender: TObject);
    procedure edDoctoChange(Sender: TObject);
    procedure cbExibeDigCorretoClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frValidador: TfrValidador;

implementation

{$R *.dfm}

procedure TfrValidador.FormCreate(Sender: TObject);
begin
  edIgnorar.Text := ACBrValidador1.IgnorarChar ;
  cbPermiteVazio.Checked := ACBrValidador1.PermiteVazio ;
  cbException.Checked    := ACBrValidador1.RaiseExcept ;
  cbAjustarTam.Checked   := ACBrValidador1.AjustarTamanho ;
  cbTipoDocto.ItemIndex  := Integer( ACBrValidador1.TipoDocto ) ;
end;

procedure TfrValidador.ACBrValidador1MsgErro(Mensagem: String);
begin
  mMsgErro.Text := ACBrValidador1.MsgErro ;
end;

procedure TfrValidador.bValidarClick(Sender: TObject);
begin
  bFormatarClick( Sender );
  
  ACBrValidador1.Documento   := edDocto.Text ;
  ACBrValidador1.Complemento := edComple.Text ;
  ACBrValidador1.IgnorarChar := edIgnorar.Text ;

  if ACBrValidador1.Validar then
     mMsgErro.Text := 'Documento OK'
  else
     mMsgErro.Text := ACBrValidador1.MsgErro ;

end;

procedure TfrValidador.bFormatarClick(Sender: TObject);
begin
  ACBrValidador1.Documento   := edDocto.Text ;
  ACBrValidador1.Complemento := edComple.Text ;

  edDocto.Text := ACBrValidador1.Formatar ;
end;

procedure TfrValidador.cbPermiteVazioClick(Sender: TObject);
begin
  ACBrValidador1.PermiteVazio := cbPermiteVazio.Checked ;
end;

procedure TfrValidador.cbAjustarTamClick(Sender: TObject);
begin
  ACBrValidador1.AjustarTamanho := cbAjustarTam.Checked ;
end;

procedure TfrValidador.cbExibeDigCorretoClick(Sender: TObject);
begin
  ACBrValidador1.ExibeDigitoCorreto := cbExibeDigCorreto.Checked ;
end;

procedure TfrValidador.cbExceptionClick(Sender: TObject);
begin
  ACBrValidador1.RaiseExcept := cbException.Checked ;
end;

procedure TfrValidador.cbTipoDoctoChange(Sender: TObject);
begin
  ACBrValidador1.TipoDocto := TACBrValTipoDocto( cbTipoDocto.ItemIndex ) ;
  if ACBrValidador1.TipoDocto <> docInscEst then
     edComple.Text := '' ;
end;

procedure TfrValidador.edDoctoChange(Sender: TObject);
begin
  mMsgErro.Lines.Clear ;
end;

end.
