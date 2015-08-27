unit Relatorio;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Edit, FMX.ScrollBox, FMX.Memo,
  FMX.Controls.Presentation;

type
  TfrRelatorio = class(TForm)
    Button1: TButton;
    Button2: TButton;
    mRelat: TMemo;
    edVias: TEdit;
    edCupom: TEdit;
    edFPG: TEdit;
    edCNF: TEdit;
    edValor: TEdit;
    lValor: TLabel;
    lCodCNF: TLabel;
    lCodFPG: TLabel;
    lCupom: TLabel;
    Label1: TLabel;
    sbFPG: TButton;
    sbCNF: TButton;
    OpenDialog1: TOpenDialog;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure sbFPGClick(Sender: TObject);
    procedure sbCNFClick(Sender: TObject);
    procedure edValorKeyPress(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
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

{$R *.fmx}


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

procedure TfrRelatorio.edValorKeyPress(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
 if KeyChar in [',','.'] then
    KeyChar := DecimalSeparator ;
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
     lCupom.Text :=  '&Indice Relatório Rerencial';
     edCupom.Text   :=  '01';
  end;
end;

procedure TfrRelatorio.sbFPGClick(Sender: TObject);
begin
  Form1.FormasdePagamento1Click( Sender );
end;

procedure TfrRelatorio.sbCNFClick(Sender: TObject);
begin
  Form1.CarregaComprovantesNAOFiscais1Click( Sender );
end;

end.
