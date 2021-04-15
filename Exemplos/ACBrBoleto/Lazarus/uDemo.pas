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

unit uDemo; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, EditBtn, ACBrBoleto, ACBrBoletoFCFortesFr,
  ExtCtrls, MaskEdit, Buttons, ACBrUtil, ACBrMail, ACBrBoletoConversao;

type


  { TfrmDemo }

  TfrmDemo = class ( TForm )
     ACBrBoleto1: TACBrBoleto;
     ACBrBoletoFCFortes1 : TACBrBoletoFCFortes ;
     ACBrMail1: TACBrMail;
     btnIncluiBoleto: TButton;
     btnIncluir10Boletos: TButton;
     btnGerarRemessa: TButton;
     btnImprimir: TButton;
     btnZerar: TButton;
     Button1: TButton;
     Button2: TButton;
     Button3: TButton;
     Button4: TButton;
     btnRegistro: TButton;
     cbxAceite: TComboBox;
     cbxLayOut : TComboBox ;
     edtInstrucoes1: TEdit;
     edtInstrucoes2: TEdit;
     edtMulta: TEdit;
     edtCEP: TMaskEdit;
     edtCPFCNPJ: TEdit;
     edtDataAbatimento: TDateEdit;
     edtDataDesconto: TDateEdit;
     edtDataProtesto: TDateEdit;
     edtEmail: TEdit;
     edtNossoNro: TEdit;
     edtUF: TEdit;
     edtCidade: TEdit;
     edtBairro: TEdit;
     edtComplemento: TEdit;
     edtNumero: TEdit;
     edtEndereco: TEdit;
     edtNome: TEdit;
     edtCarteira: TEdit;
     edtDataDoc: TDateEdit;
     edtEspecieDoc: TEdit;
     edtEspecieMod: TEdit;
     edtLocalPag: TEdit;
     edtNumeroDoc: TEdit;
     edtValorDoc: TEdit;
     edtMoraJuros: TEdit;
     edtValorAbatimento: TEdit;
     edtValorDesconto: TEdit;
     edtVencimento: TDateEdit;
     edtDataMora: TDateEdit;
     GroupBox1: TGroupBox;
     GroupBox2: TGroupBox;
     GroupBox3: TGroupBox;
     GroupBox4: TGroupBox;
     GroupBox5: TGroupBox;
     Label1: TLabel;
     Label10: TLabel;
     Label11: TLabel;
     Label12: TLabel;
     Label13: TLabel;
     Label14: TLabel;
     Label15 : TLabel ;
     Label16: TLabel;
     Label17: TLabel;
     Label18: TLabel;
     Label19: TLabel;
     Label2: TLabel;
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
     Label3: TLabel;
     Label30: TLabel;
     Label31: TLabel;
     Label32: TLabel;
     Label4: TLabel;
     Label5: TLabel;
     Label6: TLabel;
     Label7: TLabel;
     Label8: TLabel;
     Label9: TLabel;
     memMensagem: TMemo;
     Panel1: TPanel;
     Panel2: TPanel;
     procedure btnGerarRemessaClick ( Sender: TObject ) ;
     procedure btnIncluiBoletoClick ( Sender: TObject ) ;
     procedure btnIncluir10BoletosClick ( Sender: TObject ) ;
     procedure btnImprimirClick ( Sender: TObject ) ;
     procedure btnRegistroClick(Sender: TObject);
     procedure btnZerarClick ( Sender: TObject ) ;
     procedure Button1Click ( Sender: TObject ) ;
     procedure Button2Click ( Sender: TObject ) ;
     procedure Button3Click(Sender: TObject);
     procedure Button4Click(Sender: TObject);
     procedure cbxLayOutChange(Sender : TObject) ;
     procedure FormCreate ( Sender: TObject ) ;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmDemo: TfrmDemo;

implementation

Uses typinfo;

{$R *.lfm}

{ TfrmDemo }

procedure TfrmDemo.btnIncluir10BoletosClick ( Sender: TObject ) ;
var
  Titulo    : TACBrTitulo;
  I         : Integer;
  NrTitulos : Integer;
  NrTitulosStr :String;
  Convertido: Boolean;
begin
   NrTitulos    := 10;
   NrTitulosStr := '10';
   Convertido   := true;

   repeat
     InputQuery('Número de Boletos a incluir','',False,NrTitulosStr);
     try
       NrTitulos := StrToInt(NrTitulosStr);
     except
       Convertido:= false;
     end;
   until  Convertido;

   for I := 1 to NrTitulos do
   begin
     Titulo:= ACBrBoleto1.CriarTituloNaLista;

     with Titulo do
     begin
       // LocalPagamento    := 'Pagar preferêncialmente nas agências do '+ ACBrBoleto1.Banco.Nome; //MEnsagem exigida pelo bradesco
        Vencimento        := IncMonth(EncodeDate(2010,05,10),I);
        DataDocumento     := EncodeDate(2010,04,10);
        NumeroDocumento   := PadRight(IntToStr(I),6,'0');
        EspecieDoc        := 'DM';
        Aceite            := atSim;
        DataProcessamento := Now;
        Carteira          := 'CSB';
        NossoNumero       := IntToStr(I);//IntToStrZero(I,ACBrBoleto1.Banco.TamanhoMaximoNossoNum);
        ValorDocumento    := 100.10 * (I+0.5);
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

        //ACBrBoleto1.AdicionarMensagensPadroes(Titulo,Mensagem);
     end;
   end;
end;

procedure TfrmDemo.btnImprimirClick ( Sender: TObject ) ;
//var
//  i: Integer;
begin
   ACBrBoleto1.Imprimir;

   //Método para impressao de cada titulo de forma individual
   {for i:= 0 to ACBrBoleto1.ListadeBoletos.Count -1 do
   begin
     ACBrBoleto1.ListadeBoletos[i].Imprimir();

   end; }

end;

{
--Utiliza WebService dos Bancos para realizar o Registro dos Boletos--
Até o momento disponível para Caixa Economica, Banco do Brasil, Itau
É necessario realizar a configuração previa para acesso ao WebService
No Object Inspector verifique as propriedades: CedenteWS e Configuracoes/WebService
Verifique no arquivo "configWebService.txt" quais as configurações necessárias para cada Banco
}
procedure TfrmDemo.btnRegistroClick(Sender: TObject);
var
  SLRemessa: TStringList;
  i,j : Integer;
begin
  with ACBrBoleto1 do
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
           SLRemessa.Add('[Registro'+IntToStr(i)+']' + sLineBreak +
                       'Cod_Retorno='+ ListaRetornoWeb[i].CodRetorno + sLineBreak +
                       'Msg_Retorno='+ ListaRetornoWeb[i].MsgRetorno + sLineBreak +
                       'Ori_Retorno='+ ListaRetornoWeb[i].OriRetorno + sLineBreak );
           for j:= 0 to ListaRetornoWeb[i].ListaRejeicao.Count -1 do
           begin
             SLRemessa.Add('[Rejeicao'+IntToStr(j)+']' + sLineBreak +
                           'Campo=' + ListaRetornoWeb[i].ListaRejeicao[j].Campo + sLineBreak +
                           'Mensagem=' + ListaRetornoWeb[i].ListaRejeicao[j].Mensagem + sLineBreak +
                           'Valor='+ ListaRetornoWeb[i].ListaRejeicao[j].Valor + sLineBreak );
           end;
           SLRemessa.Add('HEADER' + sLineBreak +
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
          if NaoEstaVazio(ListaRetornoWeb[i].DadosRet.TituloRet.CodBarras) then
          begin
            SLRemessa.Add('TITULO_RETORNO' + sLineBreak  +
             'vencimento_titulo='+FormatDateTime('dd/mm/yyyy',ListaRetornoWeb[i].DadosRet.TituloRet.Vencimento)+ sLineBreak +
             'tipo_carteira_titulo='+ListaRetornoWeb[i].DadosRet.TituloRet.Carteira+ sLineBreak +
             'nosso_numero='+ListaRetornoWeb[i].DadosRet.TituloRet.NossoNumero+ sLineBreak +
             'seu_numero='+ListaRetornoWeb[i].DadosRet.TituloRet.SeuNumero+ sLineBreak +
             'especie='+ListaRetornoWeb[i].DadosRet.TituloRet.EspecieDoc+ sLineBreak +
             'codigo_barras='+ListaRetornoWeb[i].DadosRet.TituloRet.CodBarras+ sLineBreak +
             'numero_linha_digitavel='+ListaRetornoWeb[i].DadosRet.TituloRet.LinhaDig+ sLineBreak +
             'local_pagamento='+ListaRetornoWeb[i].DadosRet.TituloRet.Mensagem.Text+ sLineBreak +
             'data_processamento='+FormatDateTime('dd/mm/yyyy',ListaRetornoWeb[i].DadosRet.TituloRet.DataProcessamento)+ sLineBreak +
             'data_emissao='+FormatDateTime('dd/mm/yyyy',ListaRetornoWeb[i].DadosRet.TituloRet.DataDocumento)+ sLineBreak +
             'uso_banco='+ListaRetornoWeb[i].DadosRet.TituloRet.UsoBanco+ sLineBreak +
             'valor_titulo='+CurrToStr(ListaRetornoWeb[i].DadosRet.TituloRet.ValorDocumento)+ sLineBreak +
             'valor_desconto='+CurrToStr(ListaRetornoWeb[i].DadosRet.TituloRet.ValorDesconto)+ sLineBreak +
             'valor_outra_deducao='+CurrToStr(ListaRetornoWeb[i].DadosRet.TituloRet.ValorDespesaCobranca)+ sLineBreak +
             'valor_juro_multa='+CurrToStr(ListaRetornoWeb[i].DadosRet.TituloRet.ValorMoraJuros)+ sLineBreak +
             'valor_outro_acrescimo='+CurrToStr(ListaRetornoWeb[i].DadosRet.TituloRet.ValorOutrosCreditos)+ sLineBreak +
             'valor_total_cobrado='+CurrToStr(ListaRetornoWeb[i].DadosRet.TituloRet.ValorPago) + sLineBreak +
             'texto_informacao_cliente_beneficiario=' +ListaRetornoWeb[i].DadosRet.TituloRet.Informativo.Text  );

          end;
        end;

        SLRemessa.SaveToFile( PathWithDelim(ExtractFilePath(Application.ExeName))+'RetornoRegistro.txt' );
      finally
        SLRemessa.Free;
      end;
      ShowMessage('Retorno Envio gerado em: '+ PathWithDelim(ExtractFilePath(Application.ExeName))+'RetornoRegistro.txt' );

    end;


  end;
end;

procedure TfrmDemo.btnZerarClick ( Sender: TObject ) ;
begin
   ACBrBoleto1.ListadeBoletos.Clear;
end;

procedure TfrmDemo.FormCreate ( Sender: TObject ) ;
var
  I : TACBrBolLayOut ;
begin
   edtDataDoc.Date    := Now;
   edtVencimento.Date := IncMonth(edtDataDoc.Date,1);
   edtDataMora.Date   := edtVencimento.Date+1;

   cbxLayOut.Items.Clear ;
   For I := Low(TACBrBolLayOut) to High(TACBrBolLayOut) do
      cbxLayOut.Items.Add( GetEnumName(TypeInfo(TACBrBolLayOut), integer(I) ) ) ;

   cbxLayOut.ItemIndex := 0;
end;

procedure TfrmDemo.btnIncluiBoletoClick ( Sender: TObject ) ;
var
  Titulo : TACBrTitulo;
  DadosNFe: TACBrDadosNFe;
  I: integer;
begin

     Titulo := ACBrBoleto1.CriarTituloNaLista;

     with Titulo do
     begin
        Vencimento        := edtVencimento.Date;
        DataDocumento     := edtDataDoc.Date;
        NumeroDocumento   := edtNumeroDoc.Text;
        EspecieDoc        := edtEspecieDoc.Text;
        if cbxAceite.ItemIndex = 0 then
           Aceite := atSim
        else
           Aceite := atNao;
        Carteira          := edtCarteira.Text;
        DataProcessamento := Now;
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
        //LocalPagamento    := edtLocalPag.Text+ ' '+ ACBrBoleto1.Banco.Nome;
        ValorMoraJuros    := StrToCurrDef(edtMoraJuros.Text,0);
        ValorDesconto     := StrToCurrDef(edtValorDesconto.Text,0);
        ValorAbatimento   := StrToCurrDef(edtValorAbatimento.Text,0);
        DataMoraJuros     := edtDataMora.Date;
        DataMulta         := edtDataMora.Date;;
        DataDesconto      := edtDataDesconto.Date;
        DataAbatimento    := edtDataAbatimento.Date;
        DataProtesto      := edtDataProtesto.Date;
        PercentualMulta   := StrToCurrDef(edtMulta.Text,0);
        Mensagem.Text     := memMensagem.Text;
        OcorrenciaOriginal.Tipo  := toRemessaRegistrar;
        Instrucao1        := PadRight(trim(edtInstrucoes1.Text),2,'0');
        Instrucao2        := PadRight(trim(edtInstrucoes2.Text),2,'0');

        QtdePagamentoParcial:= 1;
        TipoPagamento:= tpNao_Aceita_Valor_Divergente;
        PercentualMinPagamento:= 0;
        PercentualMaxPagamento:= 0;
        ValorMinPagamento:= 0;
        ValorMaxPagamento:= 0;

        for I:= 0 to 4 do
        begin
         DadosNFe:= Titulo.CriarNFeNaLista;
         DadosNFe.NumNFe:= '123456';
         DadosNFe.EmissaoNFe:= Now;
         DadosNFe.ValorNFe:= 100;
         DadosNFe.ChaveNFe:=  StringOfChar('1' ,44);
        end;


         with Sacado.SacadoAvalista  do
         begin
          Pessoa:= pJuridica;
          NomeAvalista:= 'RIAADE SUPRIMENTOS MEDICOS LTDA';
          CNPJCPF:= '18.760.540.0001-39';
          Logradouro:= 'Rua XI de Agosto';
          Numero:= '100';
          Bairro:= 'Centro';
          Cidade:= 'Tatui';
          UF:= 'SP';
          CEP:= '18270-170';
         end;
        end;
        {Parcela := 1;
        TotalParcelas := 1};

       // ACBrBoleto1.AdicionarMensagensPadroes(Titulo,Mensagem);
end;

procedure TfrmDemo.btnGerarRemessaClick ( Sender: TObject ) ;
begin
   ACBrBoleto1.GerarRemessa( 1 );
end;

procedure TfrmDemo.Button1Click ( Sender: TObject ) ;
//var
  //i: Integer;
begin
   ACBrBoletoFCFortes1.NomeArquivo := './teste.pdf' ;
   ACBrBoleto1.GerarPDF;

   //Método para geração PDF de forma individual
   {for i:= 0 to ACBrBoleto1.ListadeBoletos.Count -1 do
   begin
     ACBrBoleto1.ListadeBoletos[i].GerarPDF();

   end;}

end;

procedure TfrmDemo.Button2Click ( Sender: TObject ) ;
begin
   ACBrBoleto1.GerarHTML;
end;

procedure TfrmDemo.Button3Click(Sender: TObject);
begin
   //ACBrBoleto1.LerRetorno();
   ACBrBoleto1.GetOcorrenciasRemessa();
end;

procedure TfrmDemo.Button4Click(Sender: TObject);
var
  SL: TStringList;
  //i: Integer;
begin
  SL := TStringList.Create;
  try
    SL.Add('Olá,');
    SL.Add('Atenção, Boleto está em Anexo');
    ACBrBoleto1.EnviarEmail(edtEmail.Text ,'Teste de Envio de Email', SL, True);

    //Método para envio e-mail de forma individual para cada título
    {for i := 0 to ACBrBoleto1.ListadeBoletos.Count -1 do
    begin
      if (ACBrBoleto1.ListadeBoletos[i].Sacado.Email <> '') then
        ACBrBoleto1.ListadeBoletos[i].EnviarEmail(ACBrBoleto1.ListadeBoletos[i].Sacado.Email ,'Teste de Envio de Email', SL, True);

    end;}

  finally
    SL.Free;
  end;
end;

procedure TfrmDemo.cbxLayOutChange(Sender : TObject) ;
begin
  ACBrBoleto1.ACBrBoletoFC.LayOut := TACBrBolLayOut( cbxLayOut.ItemIndex );
end;

end.

