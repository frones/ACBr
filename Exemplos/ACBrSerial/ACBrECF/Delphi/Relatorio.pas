{$I ACBr.inc}

unit Relatorio;

interface

uses
  SysUtils,
  {$IFDEF Delphi6_UP} Types, {$ELSE} ACBrD5, {$ENDIF}
  Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, Classes;

type
  TfrRelatorio = class(TForm)
    mRelat: TMemo;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    edVias: TEdit;
    OpenDialog1: TOpenDialog;
    lCodFPG: TLabel;
    edFPG: TEdit;
    sbFPG: TSpeedButton;
    lValor: TLabel;
    edValor: TEdit;
    lCupom: TLabel;
    edCupom: TEdit;
    lCodCNF: TLabel;
    edCNF: TEdit;
    sbCNF: TSpeedButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure sbFPGClick(Sender: TObject);
    procedure edValorKeyPress(Sender: TObject; var Key: Char);
    procedure sbCNFClick(Sender: TObject);
  private
    { Private declarations }
  public
    TipoRelatorio : Char ;
    { Public declarations }
  end;

var
  frRelatorio: TfrRelatorio;

implementation

uses ECFTeste1, ACBrECF, ACBrConsts;

{$R *.dfm}

procedure TfrRelatorio.FormCreate(Sender: TObject);
begin
  TipoRelatorio := 'G' ;
end;

procedure TfrRelatorio.Button1Click(Sender: TObject);
begin
if OpenDialog1.Execute then
   mRelat.Lines.LoadFromFile(OpenDialog1.FileName);
end;

procedure TfrRelatorio.Button2Click(Sender: TObject);
begin
  if TipoRelatorio = 'V' then
     Form1.ACBrECF1.CupomVinculado( edCupom.Text, edFPG.Text, edCNF.Text,
        StrToFloatDef( edValor.Text,0 ), mRelat.Lines,
        StrToIntDef(edVias.Text,1) )
  else
     Form1.ACBrECF1.RelatorioGerencial(mRelat.Lines,StrToIntDef(edVias.Text,1), StrToIntDef(edCupom.Text, 0));
end;

procedure TfrRelatorio.FormShow(Sender: TObject);
begin
//  mRelat.WrapAtValue := Form1.ACBrECF1.Colunas ;
  edFPG.Visible   := (TipoRelatorio = 'V') ;
//  lCupom.Visible  := edFPG.Visible ;
//  edCupom.Visible := edFPG.Visible ;
  lCodFPG.Visible := edFPG.Visible ;
  sbFPG.Visible   := edFPG.Visible ;
  lValor.Visible  := edFPG.Visible ;
  edValor.Visible := edFPG.Visible ;

  if edCupom.Visible then
     edCupom.Text := Form1.ACBrECF1.NumCupom ;
     
  edCNF.Text      := '' ;
  edCNF.Visible   := edFPG.Visible and
                    (Form1.ACBrECF1.Modelo in [ecfDaruma, ecfSchalter]) ;
  sbCNF.Visible   := edCNF.Visible ;
  lCodCNF.Visible := edCNF.Visible ;
  if TipoRelatorio = 'V' then
     Caption := 'Cupom NAO Fiscal Vinculado'
  else
  begin
     Caption := 'Relatório Gerêncial' ;
     lCupom.Caption :=  '&Indice Relatório Rerencial';
     edCupom.Text   :=  '01';
  end;
end;

procedure TfrRelatorio.sbFPGClick(Sender: TObject);
begin
  Form1.FormasdePagamento1Click( Sender );
end;

procedure TfrRelatorio.edValorKeyPress(Sender: TObject; var Key: Char);
begin
  if Key in [',','.'] then
     Key := DecimalSeparator ;
end;

procedure TfrRelatorio.sbCNFClick(Sender: TObject);
begin
  Form1.CarregaComprovantesNAOFiscais1Click( Sender );
end;

end.
