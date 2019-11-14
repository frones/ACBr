{$I Report.inc}
unit uDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Mask, {$IFDEF demo_forte} uDMForte, {$ELSE}uDMFast, {$ENDIF}ACBrBase, ACBrBoleto, ACBrUtil;

type
  TfrmDemo = class(TForm)
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    edtLocalPag: TEdit;
    edtEspecieDoc: TEdit;
    edtEspecieMod: TEdit;
    cbxAceite: TComboBox;
    edtCarteira: TEdit;
    edtNossoNro: TEdit;
    edtMoraJuros: TEdit;
    edtValorDesconto: TEdit;
    edtValorAbatimento: TEdit;
    edtMulta: TEdit;
    edtDataMora: TMaskEdit;
    edtDataDesconto: TMaskEdit;
    edtDataAbatimento: TMaskEdit;
    edtDataProtesto: TMaskEdit;
    edtNumeroDoc: TEdit;
    edtValorDoc: TEdit;
    edtDataDoc: TMaskEdit;
    edtVencimento: TMaskEdit;
    memMensagem: TMemo;
    edtInstrucoes1: TEdit;
    edtInstrucoes2: TEdit;
    Panel2: TPanel;
    edtNome: TEdit;
    edtCPFCNPJ: TEdit;
    edtEmail: TEdit;
    edtEndereco: TEdit;
    edtNumero: TEdit;
    edtComplemento: TEdit;
    edtBairro: TEdit;
    edtCidade: TEdit;
    edtCEP: TEdit;
    Label30: TLabel;
    edtUF: TEdit;
    Label31: TLabel;
    cbxLayOut: TComboBox;
    cbxImprimirVersoFatura: TCheckBox;
    btnLerRetorno: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure cbxLayOutChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnLerRetornoClick(Sender: TObject);
  private
{$IFDEF demo_forte}
    dm: TdmForte;
{$ELSE}
    dm: TdmFast;
{$ENDIF}
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmDemo: TfrmDemo;

implementation

Uses TypInfo;

{$R *.dfm}

procedure TfrmDemo.btnLerRetornoClick(Sender: TObject);
begin
  dm.ACBrBoleto.LerRetorno();
end;

procedure TfrmDemo.Button1Click(Sender: TObject);
begin
  dm.ACBrBoleto.ACBrBoletoFC.NomeArquivo := ExtractFilePath(Application.ExeName) + 'teste.html';
  dm.ACBrBoleto.GerarHTML;
end;

procedure TfrmDemo.Button2Click(Sender: TObject);
begin
  dm.ACBrBoleto.GerarPDF;
end;

procedure TfrmDemo.Button4Click(Sender: TObject);
var
  Titulo : TACBrTitulo;
  VQtdeCarcA, VQtdeCarcB, VQtdeCarcC :Integer;
  VLinha, logo : string;
  i: Integer;
begin
  dm.ACBrBoleto.Cedente.FantasiaCedente := 'Nome Fantasia' ;

  dm.ACBrBoleto.Cedente.Nome := 'Nome do edente' ;
  dm.ACBrBoleto.Cedente.Logradouro := 'Logradouro do edente' ;
  dm.ACBrBoleto.Cedente.Bairro := 'Bairro do edente' ;
  dm.ACBrBoleto.Cedente.Cidade := 'Cidade do edente' ;
  dm.ACBrBoleto.Cedente.CEP := 'CEP do edente' ;

  Titulo := dm.ACBrBoleto.CriarTituloNaLista;

  with Titulo do
  begin
    Vencimento        := StrToDate(edtVencimento.Text);
    DataDocumento     := StrToDate(edtDataDoc.Text);
    NumeroDocumento   := edtNumeroDoc.Text;
    EspecieDoc        := edtEspecieDoc.Text;
    if cbxAceite.ItemIndex = 0 then
       Aceite := atSim
    else
       Aceite := atNao;
    DataProcessamento := Now;
    NossoNumero       := edtNossoNro.Text;
    Carteira          := edtCarteira.Text;
    ValorDocumento    := StrToCurr(edtValorDoc.Text);
    Sacado.NomeSacado := edtNome.Text;
    Sacado.CNPJCPF    := OnlyNumber(edtCPFCNPJ.Text);
    Sacado.Logradouro := edtEndereco.Text;
    Sacado.Numero     := edtNumero.Text;
    Sacado.Bairro     := edtBairro.Text;
    Sacado.Cidade     := edtCidade.Text;
    Sacado.UF         := edtUF.Text;
    Sacado.CEP        := OnlyNumber(edtCEP.Text);
    ValorAbatimento   := StrToCurrDef(edtValorAbatimento.Text,0);
    LocalPagamento    := edtLocalPag.Text;
    ValorMoraJuros    := StrToCurrDef(edtMoraJuros.Text,0);
    ValorDesconto     := StrToCurrDef(edtValorDesconto.Text,0);
    ValorAbatimento   := StrToCurrDef(edtValorAbatimento.Text,0);
    DataMoraJuros     := StrToDateDef(edtDataMora.Text, 0);
    DataDesconto      := StrToDateDef(edtDataDesconto.Text, 0);
    DataAbatimento    := StrToDateDef(edtDataAbatimento.Text, 0);
    DataProtesto      := StrToDateDef(edtDataProtesto.Text, 0);
    PercentualMulta   := StrToCurrDef(edtMulta.Text,0);
    Mensagem.Text     := memMensagem.Text;
    OcorrenciaOriginal.Tipo := toRemessaBaixar;
    Instrucao1        := PadRight(trim(edtInstrucoes1.Text),2,'0');
    Instrucao2        := PadRight(trim(edtInstrucoes2.Text),2,'0');

   // dm.ACBrBoleto.AdicionarMensagensPadroes(Titulo,Mensagem);

    if cbxLayOut.ItemIndex = 4 then
    begin
      for i:=0 to 3 do
      begin
        VLinha := '.';

        VQtdeCarcA := length('Descrição Produto/Serviço ' + IntToStr(I)) ;
        VQtdeCarcB := Length('Valor:');
        VQtdeCarcC := 85 - (VQtdeCarcA + VQtdeCarcB);

        VLinha := PadLeft(VLinha,VQtdeCarcC,'.');

        Detalhamento.Add('Descrição Produto/Serviço ' + IntToStr(I) + ' '+ VLinha + ' Valor:   '+  PadRight(FormatCurr('R$ ###,##0.00', StrToCurr(edtValorDoc.Text) * 0.25),18,' ') );
      end;
      Detalhamento.Add('');
      Detalhamento.Add('');
      Detalhamento.Add('');
      Detalhamento.Add('');
      Detalhamento.Add('Desconto ........................................................................... Valor: R$ 0,00' );
    end;

    logo:= ExtractFileDir(ParamStr(0)) + '\acbr_logo.jpg';

    ArquivoLogoEmp := logo ;  // logo da empresa
    //ShowMessage(logo);

    Verso := ((cbxImprimirVersoFatura.Checked) and ( cbxImprimirVersoFatura.Enabled = true ));
  end;

end;

procedure TfrmDemo.Button5Click(Sender: TObject);
var
  Titulo: TACBrTitulo;
  I: Integer;
  NrTitulos: Integer;
  NrTitulosStr: String;
  Convertido: Boolean;
begin
  NrTitulos    := 10;
  NrTitulosStr := '10';
  Convertido   := true;
  dm.ACBrBoleto.Cedente.FantasiaCedente := 'Nome Fantasia do Cliente' ;
  repeat
    InputQuery('ACBrBoleto','Número de Boletos a incluir',NrTitulosStr);
    try
     NrTitulos := StrToInt(NrTitulosStr);
    except
     Convertido:= false;
    end;
  until  Convertido;

  for I := 1 to NrTitulos do
  begin
    Titulo:= dm.ACBrBoleto.CriarTituloNaLista;

    with Titulo do
    begin
      LocalPagamento    := 'Pagar preferêncialmente nas agências do Bradesco'; //MEnsagem exigida pelo bradesco
      Vencimento        := IncMonth(EncodeDate(2010,05,10),I);
      DataDocumento     := EncodeDate(2010,04,10);
      NumeroDocumento   := PadRight(IntToStr(I),6,'0');
      EspecieDoc        := 'DM';
      Aceite            := atSim;
      DataProcessamento := Now;
      NossoNumero       := IntToStrZero(I,11);
      Carteira          := '09';
      ValorDocumento    := 100.35 * (I+0.5);
      Sacado.NomeSacado := 'Jose Luiz Pedroso';
      Sacado.CNPJCPF    := '12345678901';
      Sacado.Logradouro := 'Rua da Consolacao';
      Sacado.Numero     := '100';
      Sacado.Bairro     := 'Vila Esperanca';
      Sacado.Cidade     := 'Tatui';
      Sacado.UF         := 'SP';
      Sacado.CEP        := '18270000';
      ValorAbatimento   := 10;
      DataAbatimento    := Vencimento-5;
      Instrucao1        := '00';
      Instrucao2        := '00';
      NossoNumero       := edtNossoNro.Text;
      dm.ACBrBoleto.AdicionarMensagensPadroes(Titulo,Mensagem);
    end;
  end;
end;

procedure TfrmDemo.Button6Click(Sender: TObject);
begin
  dm.ACBrBoleto.GerarRemessa(1);
end;

procedure TfrmDemo.Button7Click(Sender: TObject);
begin
  dm.ACBrBoleto.Imprimir;
end;

procedure TfrmDemo.FormCreate(Sender: TObject);
var
  I: TACBrBolLayOut;
begin
   edtDataDoc.Text    := DateToStr(Now);
   edtVencimento.Text := DateToStr(IncMonth(StrToDate(edtDataDoc.Text),1));
   edtDataMora.Text   := DateToStr(StrToDate(edtVencimento.Text)+1);

  cbxLayOut.Items.Clear;
  For I := Low(TACBrBolLayOut) to High(TACBrBolLayOut) do
    cbxLayOut.Items.Add(GetEnumName(TypeInfo(TACBrBolLayOut), Integer(I)));
  cbxLayOut.ItemIndex := 0;
end;

procedure TfrmDemo.FormShow(Sender: TObject);
begin
{$IFDEF demo_forte}
  dm := dmForte;
{$ELSE}
  dm := dmFast;
{$ENDIF}
end;

procedure TfrmDemo.Button3Click(Sender: TObject);
begin
  dm.ACBrBoleto.ListadeBoletos.Clear;
end;

procedure TfrmDemo.cbxLayOutChange(Sender: TObject);
begin
  dm.ACBrBoleto.ACBrBoletoFC.LayOut := TACBrBolLayOut( cbxLayOut.ItemIndex );

  cbxImprimirVersoFatura.Enabled := (cbxLayOut.ItemIndex = 4); // lFaturaDetal
  if cbxLayOut.ItemIndex <> 4 then
   cbxImprimirVersoFatura.Checked := false;
end;

end.
