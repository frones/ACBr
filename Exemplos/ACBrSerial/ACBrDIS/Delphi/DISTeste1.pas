{$I ACBr.inc}

unit DISTeste1;

interface

uses
  SysUtils,
 {$IFDEF Delphi6_UP} Types, Variants,{$ELSE} Windows, {$ENDIF}
  Classes,  Graphics, Controls, Forms,
  Dialogs, StdCtrls, ACBrBase, ACBrDIS;

type
  TForm1 = class(TForm)
    edLinha1: TEdit;
    edLinha2: TEdit;
    ACBrDIS1: TACBrDIS;
    cbxPorta: TComboBox;
    Label2: TLabel;
    Label1: TLabel;
    cbxModelo: TComboBox;
    Label3: TLabel;
    edIntervalo: TEdit;
    Label4: TLabel;
    edPassos: TEdit;
    Label5: TLabel;
    Label6: TLabel;
    lLinhas: TLabel;
    lColunas: TLabel;
    lLinha1: TLabel;
    lLinha2: TLabel;
    bDemo: TButton;
    bLimpar: TButton;
    bParar: TButton;
    bContinuar: TButton;
    cbxAlinhamento: TComboBox;
    cbxExibirEfeito: TComboBox;
    cbxRolarEfeito: TComboBox;
    cbLinha1: TCheckBox;
    cbLinha2: TCheckBox;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    bExibir: TButton;
    bExibirEfeito: TButton;
    bRolar: TButton;
    Label7: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure cbLinha1Click(Sender: TObject);
    procedure cbxPortaChange(Sender: TObject);
    procedure cbxAlinhamentoChange(Sender: TObject);
    procedure cbxModeloChange(Sender: TObject);
    procedure edIntervaloChange(Sender: TObject);
    procedure edPassosChange(Sender: TObject);
    procedure bLimparClick(Sender: TObject);
    procedure bContinuarClick(Sender: TObject);
    procedure bPararClick(Sender: TObject);
    procedure bDemoClick(Sender: TObject);
    procedure bExibirClick(Sender: TObject);
    procedure ACBrDIS1Atualiza(Linha: Integer; TextoVisivel: String);
    procedure bExibirEfeitoClick(Sender: TObject);
    procedure bRolarClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
Uses AcbrUtil, typinfo ;

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  I: TACBrDISModelo;
begin
  cbxModelo.Items.Clear ;
  For I := Low(TACBrDISModelo) to High(TACBrDISModelo) do
     cbxModelo.Items.Add( GetEnumName(TypeInfo(TACBrDISModelo), integer(I) ) ) ;

  cbxAlinhamento.ItemIndex  := 2 ;
  cbxExibirEfeito.ItemIndex := 0 ;
  cbxRolarEfeito.ItemIndex  := 4 ;
  cbxModelo.ItemIndex       := 0 ;

  cbxPorta.Text    := ACBrDIS1.Porta ;
  edIntervalo.Text := IntToStr(ACBrDIS1.Intervalo) ;
  edPassos.Text    := IntToStr(ACBrDIS1.Passos) ;
  cbxPortaChange( Sender );
  cbxAlinhamentoChange( Sender ) ;
  cbxModeloChange( Sender );
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if ACBrDIS1.Ativo then
  begin
     ACBrDIS1.Alinhamento := alCentro ;
     ACBrDIS1.Intervalo   := 100 ;
     ACBrDIS1.ExibirLinha(1,
        PadCenter('COMPONENTES ACBr',ACBrDIS1.Colunas,'*'),
        efeEsquerda_Direita);
     ACBrDIS1.ExibirLinha(2,
        PadCenter('acbr.sourceforge.net',ACBrDIS1.Colunas,'*'),
        efeDireita_Esquerda);

     while ACBrDIS1.Trabalhando do
        Application.ProcessMessages ;

     sleep(500) ;
  end ;
  
  CanClose := true ;
end;

procedure TForm1.cbLinha1Click(Sender: TObject);
Var Ligado : Boolean ;
begin
  Ligado := cbLinha1.Checked or cbLinha2.Checked ;

  cbxAlinhamento.Enabled  := Ligado ;
  bExibir.Enabled         := Ligado ;
  cbxExibirEfeito.Enabled := Ligado ;
  bExibirEfeito.Enabled   := Ligado ;
  cbxRolarEfeito.Enabled  := Ligado ;
  bRolar.Enabled          := Ligado ;
end;

procedure TForm1.cbxPortaChange(Sender: TObject);
Var Atv : Boolean ;
begin
  Atv := ACBrDIS1.Ativo ;
  ACBrDIS1.Desativar ;
  ACBrDIS1.Porta := cbxPorta.Text ;
  ACBrDIS1.Ativo := Atv ;
end;

procedure TForm1.cbxModeloChange(Sender: TObject);
Var Atv : Boolean ;
begin
  Atv := ACBrDIS1.Ativo ;
  ACBrDIS1.Desativar ;
  ACBrDIS1.Modelo  := TACBrDISModelo( cbxModelo.ItemIndex ) ;
  ACBrDIS1.Ativo   := Atv ;
  lColunas.Caption := IntToStr( ACBrDIS1.Colunas ) ;
  lLinhas.Caption  := IntToStr( ACBrDIS1.LinhasCount ) ;
end;

procedure TForm1.edIntervaloChange(Sender: TObject);
begin
  ACBrDIS1.Intervalo := StrToIntDef( edIntervalo.Text, 0) ;
end;

procedure TForm1.edPassosChange(Sender: TObject);
begin
  ACBrDIS1.Passos := StrToIntDef( edPassos.Text, 0) ;
end;

procedure TForm1.bDemoClick(Sender: TObject);
begin
  ACBrDIS1.Alinhamento := alCentro ;
  ACBrDIS1.ExibirLinha(1,'COMPONENTES ACBr');
  ACBrDIS1.ExibirLinha(2,'acbr.sourceforge.net');

  ACBrDIS1.RolarLinha(1,rolParaEsquerda_Sempre);
  ACBrDIS1.RolarLinha(2,rolParaDireita_Sempre);
end;

procedure TForm1.bLimparClick(Sender: TObject);
begin
  ACBrDIS1.LimparDisplay ;
end;

procedure TForm1.bPararClick(Sender: TObject);
begin
  ACBrDIS1.Parar ;
end;

procedure TForm1.bContinuarClick(Sender: TObject);
begin
  ACBrDIS1.Continuar ;
end;

procedure TForm1.cbxAlinhamentoChange(Sender: TObject);
begin
  ACBrDIS1.Alinhamento := TACBrDISAlinhamento( cbxAlinhamento.ItemIndex ) ;
end;

procedure TForm1.ACBrDIS1Atualiza(Linha: Integer; TextoVisivel: String);
begin
  if Linha = 1 then
     lLinha1.Caption := TextoVisivel ;

  if Linha = 2 then
     lLinha2.Caption := TextoVisivel ;
end;

procedure TForm1.bExibirClick(Sender: TObject);
begin
  if cbLinha1.Checked then
     ACBrDIS1.ExibirLinha(1,edLinha1.Text,
        TACBrDISAlinhamento( cbxAlinhamento.ItemIndex ) ) ;

  if cbLinha2.Checked then
     ACBrDIS1.ExibirLinha(2,edLinha2.Text,
        TACBrDISAlinhamento( cbxAlinhamento.ItemIndex ) ) ;
end;

procedure TForm1.bExibirEfeitoClick(Sender: TObject);
begin
  if cbLinha1.Checked then
     ACBrDIS1.ExibirLinha(1,edLinha1.Text,
        TACBrDISEfeitoExibir( cbxExibirEfeito.ItemIndex ) ) ;

  if cbLinha2.Checked then
     ACBrDIS1.ExibirLinha(2,edLinha2.Text,
        TACBrDISEfeitoExibir( cbxExibirEfeito.ItemIndex ) ) ;
end;

procedure TForm1.bRolarClick(Sender: TObject);
begin
  if cbLinha1.Checked then
     ACBrDIS1.RolarLinha(1, TACBrDISEfeitoRolar( cbxRolarEfeito.ItemIndex ) ) ;

  if cbLinha2.Checked then
     ACBrDIS1.RolarLinha(2, TACBrDISEfeitoRolar( cbxRolarEfeito.ItemIndex ) ) ;
end;

end.
