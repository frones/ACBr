{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{																			   }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

{$I Report.inc}
unit uDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Mask, {$IFDEF demo_forte} uDMForte, {$ELSE}uDMFast, {$ENDIF}ACBrBase, ACBrBoleto, ACBrUtil,
  ACBrBoletoConversao;

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
    Button8: TButton;
    btnRegistro: TButton;
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
    procedure Button8Click(Sender: TObject);
    procedure btnRegistroClick(Sender: TObject);
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
//var
  //i: Integer; 
begin
  dm.ACBrBoleto.GerarPDF;

  //Método para geração PDF de forma individual
   {for i:= 0 to ACBrBoleto1.ListadeBoletos.Count -1 do
   begin
     ACBrBoleto1.ListadeBoletos[i].GerarPDF();

   end;}
end;

procedure TfrmDemo.Button4Click(Sender: TObject);
var
  Titulo : TACBrTitulo;
  VQtdeCarcA, VQtdeCarcB, VQtdeCarcC :Integer;
  VLinha, logo : string;
  i: Integer;
begin

  dm.ACBrBoleto.Cedente.FantasiaCedente := 'Nome Fantasia';

  dm.ACBrBoleto.Cedente.Nome := 'Nome do cedente';
  dm.ACBrBoleto.Cedente.Logradouro := 'Logradouro do cedente';
  dm.ACBrBoleto.Cedente.Bairro := 'Bairro do cedente';
  dm.ACBrBoleto.Cedente.Cidade := 'Cidade do cedente';
  dm.ACBrBoleto.Cedente.CEP := 'CEP do cedente';
  dm.ACBrBoleto.Cedente.Telefone := '(xx) 99999-9999';

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
    Carteira          := edtCarteira.Text;
    NossoNumero       := edtNossoNro.Text;
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
    //Mensagem.Text     := memMensagem.Text;
    OcorrenciaOriginal.Tipo := toRemessaBaixar;
    Instrucao1        := PadRight(trim(edtInstrucoes1.Text),2,'0');
    Instrucao2        := PadRight(trim(edtInstrucoes2.Text),2,'0');

    QtdePagamentoParcial:= 1;
    TipoPagamento:= tpNao_Aceita_Valor_Divergente;
    PercentualMinPagamento:= 0;
    PercentualMaxPagamento:= 0;
    ValorMinPagamento:= 0;
    ValorMaxPagamento:= 0;

   // dm.ACBrBoleto.AdicionarMensagensPadroes(Titulo,Mensagem);

    if cbxLayOut.ItemIndex = 6 then
    begin
      for i:=0 to 3 do
      begin
        VLinha := '.';

        VQtdeCarcA := length('Descrição Produto/Serviço ' + IntToStr(I));
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

    ArquivoLogoEmp := logo;  // logo da empresa
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
  dm.ACBrBoleto.Cedente.FantasiaCedente := 'Nome Fantasia do Cliente';
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
      NumeroDocumento   := PadRight(IntToStr(I),8,'0');
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
//var
//  i: Integer; 
begin
  dm.ACBrBoleto.Imprimir;

  //Método para impressao de cada titulo de forma individual
   {for i:= 0 to ACBrBoleto1.ListadeBoletos.Count -1 do
   begin
     ACBrBoleto1.ListadeBoletos[i].Imprimir();

   end; }
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

  cbxImprimirVersoFatura.Enabled := (cbxLayOut.ItemIndex = 6); // lFaturaDetal
  if cbxLayOut.ItemIndex <> 6 then
   cbxImprimirVersoFatura.Checked := false;
end;

procedure TfrmDemo.Button8Click(Sender: TObject);
var
  SL: TStringList;
  //i: Integer;
begin
  SL := TStringList.Create;
  try
    SL.Add('Olá,');
    SL.Add('Atenção, Boleto está em Anexo');
    dm.ACBrBoleto.EnviarEmail(edtEmail.Text ,'Teste de Envio de Email', SL, True);

    //Método para envio e-mail de forma individual para cada título
    {for i := 0 to dm.ACBrBoleto.ListadeBoletos.Count -1 do
    begin
      if (dm.ACBrBoleto.ListadeBoletos[i].Sacado.Email <> '') then
        dm.ACBrBoleto.ListadeBoletos[i].EnviarEmail(dm.ACBrBoleto.ListadeBoletos[i].Sacado.Email ,'Teste de Envio de Email', SL, True);

    end;}

  finally
    SL.Free;
  end;
end;


{
--Utiliza WebService dos Bancos para realizar o Registro dos Boletos--
Até o momento disponível para Caixa Economica e Banco do Brasil
É necessario realizar a configuração previa para acesso ao WebService
No Object Inspector verifique as propriedades: CedenteWS e Configuracoes/WebService
Verifique no arquivo "configWebService.txt" quais as configurações necessárias para cada Banco
}
procedure TfrmDemo.btnRegistroClick(Sender: TObject);
var
  SLRemessa: TStringList;
  i: Integer;
begin
  with dm.ACBrBoleto do
  begin
    //Função de Envio
    EnviarBoleto;

    //Verifica Lista com os retornos
    if ListaRetornoWeb.Count > 0 then
    begin
      SLRemessa := TStringList.Create;
      try
        for i:= 0 to ListaRetornoWeb.Count -1 do
        begin
          //Ler todos os campos da classe Retorno
           SLRemessa.Add('Cod_Retorno='+ ListaRetornoWeb[i].CodRetorno + sLineBreak +
                       'Msg_Retorno='+ ListaRetornoWeb[i].MsgRetorno + sLineBreak +
                       'Ori_Retorno='+ ListaRetornoWeb[i].OriRetorno + sLineBreak +
                       'HEADER' + sLineBreak +
                       'Versao='+ ListaRetornoWeb[i].Header.Versao + sLineBreak +
                       'Autenticacao=' + ListaRetornoWeb[i].Header.Autenticacao + sLineBreak +
                       'Usuario_Servico=' + ListaRetornoWeb[i].Header.Usuario_Servico + sLineBreak +
                       'Usuario=' + ListaRetornoWeb[i].Header.Usuario + sLineBreak +
                       'Operacao='  + TipoOperacaoToStr(ListaRetornoWeb[i].Header.Operacao) + sLineBreak +
                       'Indice=' + IntToStr(ListaRetornoWeb[i].Header.Indice) + sLineBreak +
                       'Sistema_Origem=' + ListaRetornoWeb[i].Header.Sistema_Origem + sLineBreak +
                       'Agencia=' + IntToStr(ListaRetornoWeb[i].Header.Agencia) + sLineBreak +
                       'ID_Origem=' + ListaRetornoWeb[i].Header.Id_Origem + sLineBreak +
                       'Data_Hora=' +FormatDateTime('dd/mm/yyyy hh:nn:ss',ListaRetornoWeb[i].Header.Data_Hora) + sLineBreak +
                       'ID_Processo=' + ListaRetornoWeb[i].Header.Id_Processo + sLineBreak +
                       'DADOS' + sLineBreak +
                       'Excessao=' +ListaRetornoWeb[i].DadosRet.Excecao + sLineBreak +
                       'CONTROLE_NEGOCIAL' + sLineBreak +
                       'Origem_Retorno=' + ListaRetornoWeb[i].DadosRet.ControleNegocial.OriRetorno + sLineBreak +
                       'NSU=' + ListaRetornoWeb[i].DadosRet.ControleNegocial.NSU + sLineBreak +
                       'Cod_Retorno=' + ListaRetornoWeb[i].DadosRet.ControleNegocial.CodRetorno + sLineBreak +
                       'Msg_Retorno=' + ListaRetornoWeb[i].DadosRet.ControleNegocial.Retorno + sLineBreak +
                       'COMPROVANTE' + sLineBreak +
                       'Data=' +  FormatDateTime('dd/mm/yyyy', ListaRetornoWeb[i].DadosRet.Comprovante.Data) + sLineBreak +
                       'Hora=' +  ListaRetornoWeb[i].DadosRet.Comprovante.Hora + sLineBreak +
                       'ID_BOLETO' + sLineBreak +
                       'Codigo_Barras=' + ListaRetornoWeb[i].DadosRet.IDBoleto.CodBarras + sLineBreak +
                       'Linha_Digitavel=' + ListaRetornoWeb[i].DadosRet.IDBoleto.LinhaDig + sLineBreak +
                       'Nosso_Numero=' + ListaRetornoWeb[i].DadosRet.IDBoleto.NossoNum + sLineBreak +
                       'URL=' + ListaRetornoWeb[i].DadosRet.IDBoleto.URL + sLineBreak +
                       'CONSULTA_BOLETO' + sLineBreak +
                       'Numero_Documento=' + ListaRetornoWeb[i].DadosRet.TituloRet.NumeroDocumento + sLineBreak +
                       'Data_Vencimento=' + FormatDateTime('dd/mm/yyyy',ListaRetornoWeb[i].DadosRet.TituloRet.Vencimento) + sLineBreak +
                       'Valor=' + CurrToStr(ListaRetornoWeb[i].DadosRet.TituloRet.ValorDocumento) + sLineBreak
                        );
        end;

        SLRemessa.SaveToFile( PathWithDelim(ExtractFilePath(Application.ExeName))+'RetornoRegistro.txt' );
      finally
        SLRemessa.Free;
      end;
      ShowMessage('Retorno Envio gerado em: '+ PathWithDelim(ExtractFilePath(Application.ExeName))+'RetornoRegistro.txt' );

    end;

  end;

end;

end.
