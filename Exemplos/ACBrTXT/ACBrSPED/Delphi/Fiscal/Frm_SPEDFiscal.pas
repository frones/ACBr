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

unit Frm_SPEDFiscal;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
{$IFNDEF FPC}
  Windows, Messages,
{$ENDIF}
  SysUtils, Variants, Classes, Graphics, Controls, Forms, ACBrEFDBlocos,
  StrUtils, Dialogs, StdCtrls, ACBrSpedFiscal, ExtCtrls, ComCtrls,
  ACBrUtil, ACBrTXTClass, DateUtils, ACBrBase;

type

  { TFrmSPEDFiscal }

  TFrmSPEDFiscal = class(TForm)
    btnB_0: TButton;
    btnB_1: TButton;
    btnB_C: TButton;
    btnB_D: TButton;
    btnB_E: TButton;
    btnB_H: TButton;
    btnError: TButton;
    btnTXT: TButton;
    btnB_9: TButton;
    cbConcomitante: TCheckBox;
    edBufNotas: TEdit;
    edNotas: TEdit;
    edBufLinhas: TEdit;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    memoError: TMemo;
    edtFile: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    memoTXT: TMemo;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    ProgressBar1: TProgressBar;
    ACBrSPEDFiscal1: TACBrSPEDFiscal;
    btnB_G: TButton;
    btnB_Completo: TButton;
    btnCancelaGeracao: TButton;
    btnB_K: TButton;
    Label9: TLabel;
    DtRef: TDateTimePicker;
    btnB_B: TButton;
    cbEstado: TComboBox;
    Label10: TLabel;
    procedure ACBrSPEDFiscal1Error(const MsnError: string);
    procedure btnB_KClick(Sender: TObject);
    procedure btnB_0Click(Sender: TObject);
    procedure btnB_9Click(Sender: TObject);
    procedure btnTXTClick(Sender: TObject);
    procedure btnB_1Click(Sender: TObject);
    procedure btnB_BClick(Sender: TObject);
    procedure btnB_CClick(Sender: TObject);
    procedure btnB_DClick(Sender: TObject);
    procedure btnB_EClick(Sender: TObject);
    procedure btnB_HClick(Sender: TObject);
    procedure btnErrorClick(Sender: TObject);
    procedure edtFileChange(Sender: TObject);
    procedure cbConcomitanteClick(Sender: TObject);
    procedure btnB_CompletoClick(Sender: TObject);
    procedure btnB_GClick(Sender: TObject);
    procedure btnCancelaGeracaoClick(Sender: TObject);
  private
    procedure LoadToMemo;
    function AnoToVersao: TACBrVersaoLeiauteSPEDFiscal;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmSPEDFiscal: TFrmSPEDFiscal;

implementation

uses ACBrEFDBloco_K_Class, ACBrEFDBloco_K, ACBrEFDBloco_0, ACBrEFDBloco_0_Class;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

const
  strUNID: array [0 .. 4] of string = ('PC', 'UN', 'LT', 'PC', 'MT');
  FCod_Item = '000001';

procedure TFrmSPEDFiscal.ACBrSPEDFiscal1Error(const MsnError: string);
begin
  memoError.Lines.Add(MsnError);
end;

function TFrmSPEDFiscal.AnoToVersao: TACBrVersaoLeiauteSPEDFiscal;
var
  xVer: string;
begin
  if (DtRef.DateTime >= StrToDate('01/01/2009')) and (DtRef.DateTime <= StrToDate('31/12/2009')) then
    xVer := '002'
  else if (DtRef.DateTime >= StrToDate('01/01/2010')) and (DtRef.DateTime <= StrToDate('31/12/2010')) then
    xVer := '003'
  else if (DtRef.DateTime >= StrToDate('01/01/2011')) and (DtRef.DateTime <= StrToDate('31/12/2011')) then
    xVer := '004'
  else if (DtRef.DateTime >= StrToDate('01/01/2012')) and (DtRef.DateTime <= StrToDate('30/06/2012')) then
    xVer := '005'
  else if (DtRef.DateTime >= StrToDate('01/07/2012')) and (DtRef.DateTime <= StrToDate('31/12/2012')) then
    xVer := '006'
  else if (DtRef.DateTime >= StrToDate('01/01/2013')) and (DtRef.DateTime <= StrToDate('31/12/2013')) then
    xVer := '007'
  else if (DtRef.DateTime >= StrToDate('01/01/2014')) and (DtRef.DateTime <= StrToDate('31/12/2014')) then
    xVer := '008'
  else if (DtRef.DateTime >= StrToDate('01/01/2015')) and (DtRef.DateTime <= StrToDate('31/12/2015')) then
    xVer := '009'
  else if (DtRef.DateTime >= StrToDate('01/01/2016')) and (DtRef.DateTime <= StrToDate('31/12/2016')) then
    xVer := '010'
  else if (DtRef.DateTime >= StrToDate('01/01/2017')) and (DtRef.DateTime <= StrToDate('31/12/2017')) then
    xVer := '011'
  else if (DtRef.DateTime >= StrToDate('01/01/2018')) and (DtRef.DateTime <= StrToDate('31/12/2018')) then
    xVer := '012'
  else if (DtRef.DateTime >= StrToDate('01/01/2019')) and (DtRef.DateTime <= StrToDate('31/12/2019')) then
    xVer := '013'
  else if (DtRef.DateTime >= StrToDate('01/01/2020')) and (DtRef.DateTime <= StrToDate('31/12/2020')) then
    xVer := '014'
  else if (DtRef.DateTime >= StrToDate('01/01/2021')) and (DtRef.DateTime <= StrToDate('31/12/2021')) then
    xVer := '015'
  else if (DtRef.DateTime >= StrToDate('01/01/2022')) and (DtRef.DateTime <= StrToDate('31/12/2022')) then
    xVer := '016'
  else if (DtRef.DateTime >= StrToDate('01/01/2023')) then
    xVer := '017';
  Result := StrToCodVer(xVer);
end;

procedure TFrmSPEDFiscal.btnB_0Click(Sender: TObject);

var
  int0150: integer;
//  int0175: integer;
  int0300: integer;
  int0190: integer;
  int0500: Integer;
  int0600: Integer;
  int0200: Integer;
begin
  // Alimenta o componente com informações para gerar todos os registros do
  // Bloco 0.
  cbConcomitante.Enabled := False;
  btnB_0.Enabled := False;
  btnB_B.Enabled := True;

  ACBrSPEDFiscal1.DT_INI := StrToDate('01/11/' + IntToStr(YearOf(DtRef.Date)));
  ACBrSPEDFiscal1.DT_FIN := StrToDate('30/11/' + IntToStr(YearOf(DtRef.Date)));

  if cbConcomitante.Checked then
  begin
    ACBrSPEDFiscal1.LinhasBuffer := StrToIntDef(edBufLinhas.Text, 0);
    ACBrSPEDFiscal1.IniciaGeracao;
    LoadToMemo;
  end;

  with ACBrSPEDFiscal1.Bloco_0 do
  begin
    // Dados da Empresa
    with Registro0000New do
    begin
      COD_VER := AnoToVersao;
      COD_FIN := raOriginal;
      NOME := 'RAZÃO SOCIAL DA EMPRESA EMITENTE';
      CNPJ := '11111111000191';
      CPF := '';
      UF := cbEstado.Text;
      IE := '1111111119';
      COD_MUN := 4314902;
      IM := '';
      SUFRAMA := '';
      IND_PERFIL := pfPerfilA;
      IND_ATIV := atOutros;
    end;

    with Registro0001New do
    begin
      IND_MOV := imComDados;

      // FILHO - Dados complementares da Empresa
      with Registro0005New do
      begin
        FANTASIA := 'NOME FANTASSIA';
        CEP := '29900000';
        ENDERECO := 'ENDEREÇO DA EMPRESA';
        NUM := 'S/N';
        COMPL := '';
        BAIRRO := 'BAIRRO';
        FONE := '';
        FAX := '';
        EMAIL := '171@171.COM.BR';
      end;
//
//      with Registro0015New do
//      begin
//        UF_ST := 'SC';
//        IE_ST := '254504230';
//      end;
//
      // FILHO - Dados do contador.
      with Registro0100New do
      begin
        NOME := 'SO CONFIE NO CONTADOR I';
        CPF := '12345678909'; // Deve ser uma informação valida
        CRC := '65924';
        CNPJ := '';
        CEP := '92420020';
        ENDERECO := 'R. NOME DA RUA';
        NUM := '450';
        COMPL := '';
        BAIRRO := 'BAIRRO CONTADORES';
        FONE := '';
        FAX := '';
        EMAIL := 'NOME@NOME.COM.BR';
        COD_MUN := 3200607;
      end;
      // Check(Reg0001.Registro0190.LocalizaRegistro(UNID), '(0-0190) UNIDADE MEDIDA: A unidade de medida "%s" foi duplicada na lista de registros 0190!', [UNID]);

      // FILHO
      for int0150 := 1 to 4 do
      begin
        // 10 Clientes
        with Registro0150New do
        begin
          COD_PART := IntToStr(int0150);
          NOME := 'CLIENTE DE TESTES ' + IntToStr(int0150);
          if int0150 = 3 then //um exemplo de cliente no exterior.
          begin
            COD_PAIS := '3131'; //GUAM
            CNPJ := '';
            CPF := '';
//            COD_MUN := '' //O código do município é deixado de fora propositalmente. O componente vai fazê-lo ficar vazio
          end
          else
          begin
//            CNPJ := '11111111000191';
            CPF := '12345678909';
            COD_PAIS := '1058';
          end;

          IE := '';
          COD_MUN := 3553005;
          SUFRAMA := '';
          ENDERECO := 'ENDERECO CLIENTE' + IntToStr(int0150);
          NUM := '';
          COMPL := 'COMPLEMENTO'+ IntToStr(int0150);
          BAIRRO := 'BAIRRO CLIENTE' + IntToStr(int0150);

          // Alteração de nome para Participantes 2 e 4
          if (int0150 = 2) or (int0150 = 4) then
          begin
            with Registro0175New do
            begin
              DT_ALT := DT_INI + 1;
              NR_CAMPO := '03'; //03 -> Nome
              CONT_ANT := 'Nome anterior do Participante' + IntToStr(int0150);
            end;
          end;
        end;
      end;

      // FILHO
      // 4 Unidades de medida
      // Const strUNID, esta declarada no inicio deste evento.
      for int0190 := Low(strUNID) to High(strUNID) do
      begin
        if not Registro0190.LocalizaRegistro(strUNID[int0190]) then
        begin
          with Registro0190New do
          begin
            UNID := strUNID[int0190];
            DESCR := 'Descricao ' + strUNID[int0190];
          end;
        end;
      end;

      for int0200 := 1 to 10 do
      begin
        with Registro0200New do
        begin
          COD_ITEM   := FormatFloat('000000', int0200);
          DESCR_ITEM := 'DESCRIÇÃO DO ITEM' + FormatFloat('000000', int0200);
          COD_BARRA := '';
          UNID_INV  := strUNID[int0200 mod (High(strUNID)+1)];
          TIPO_ITEM := tiMercadoriaRevenda;
          COD_NCM := '30049026';
          COD_GEN := '30';
          ALIQ_ICMS := 17.00;

          with Registro0220New do
          begin
            UNID_CONV := strUNID[int0200 mod (High(strUNID)+1)];
            FAT_CONV := 1;
          end;

          //REGISTRO 0206: CÓDIGO DE PRODUTO CONFORME TABELA PUBLICADA PELA ANP (COMBUSTÍVEIS)
  //        With Registro0206New do
  //        begin
  //          COD_COMB := '910101001';
  //        end;
        end;
      end;

      // FILHO
      for int0300 := 1 to 1 do
      begin
        // 10 Bens Imobilizados
        with Registro0300New do
        begin
          COD_IND_BEM := FormatFloat('000000', int0300);
          IDENT_MERC := 1;
          DESCR_ITEM := 'DESCRIÇÃO DO ITEM';
          COD_PRNC := '';
          COD_CTA := '1'; //0500
          NR_PARC := 10;
          // FILHO
          with Registro0305New do
          begin
            COD_CCUS := '1'; //0600
            FUNC := 'BREVE DESCRIÇÃO DA FUNÇÃO DO IMOBILIZADO ' +FormatFloat('000000', int0300);;
            VIDA_UTIL := 60;
          end;
        end;
      end;

      with Registro0400New do
      begin
        COD_NAT := '99991';
        DESCR_NAT := 'DESCRIÇÃO DA NATUREZA DE OPERAÇÃO 12020';
      end;

      with Registro0450New do
      begin
        COD_INF := '000001';
        TXT := 'INFORMAÇÃO COMPLEMENTAR DO DOCUMENTO FISCAL';
      end;

      with Registro0460New do
      begin
        COD_OBS := '000001';
        TXT := 'TEXTO DE OBSERVAÇÃO DO DOCUMENTO FISCAL ';
      end;

      for int0500 := 1 to 10 do
      begin
        with Registro0500New do
        begin
          DT_ALT := ACBrSPEDFiscal1.DT_FIN;
          COD_NAT_CC := '01';
          IND_CTA := 'A';
          NIVEL := '1';
          COD_CTA := IntToStr(int0500);
          NOME_CTA := 'CONTA CONTÁBIL ' + IntToStr(int0500);
        end;
      end;

      for int0600 := 1 to 10 do
      begin
        with Registro0600New do
        begin
          DT_ALT := ACBrSPEDFiscal1.DT_FIN;
          COD_CCUS := IntToStr(int0600);
          CCUS := 'CENTRO DE CUSTOS ' + IntToStr(int0600);
        end;
      end;
    end;
  end;

  if cbConcomitante.Checked then
  begin
    ACBrSPEDFiscal1.WriteBloco_0;
    LoadToMemo;
  end;
end;

procedure TFrmSPEDFiscal.btnB_9Click(Sender: TObject);
begin
  btnB_9.Enabled := False;
  ACBrSPEDFiscal1.WriteBloco_9;
  LoadToMemo;

  // Habilita os botões
  btnB_0.Enabled := True;
  btnB_1.Enabled := True;
  btnB_B.Enabled := True;
  btnB_C.Enabled := True;
  btnB_D.Enabled := True;
  btnB_E.Enabled := True;
  btnB_G.Enabled := True;
  btnB_H.Enabled := True;
  btnB_K.Enabled := True;

  cbConcomitante.Enabled := True;
end;

procedure TFrmSPEDFiscal.btnB_BClick(Sender: TObject);
begin
  // Alimenta o componente com informações para gerar todos os registros do
  // Bloco C.
  btnB_B.Enabled := False;
  btnB_C.Enabled := True;

  with ACBrSPEDFiscal1.Bloco_B do
  begin
    with RegistroB001New do
    begin
      IND_MOV := imComDados;
      with RegistroB020New do
      begin
        IND_OPER           := tpEntradaAquisicao;
        IND_EMIT           := edTerceiros;
        COD_PART           := '1';
        COD_MOD            := '8';
        COD_SIT            := sdRegular;
        SER                := '1';
        NUM_DOC            := '123456';
        CHV_NFE            := '';
        VL_CONT            := 50.00;
        VL_ISS             := 2.5;
        with RegistroB025New do
        begin
          COD_SERV := '7.12';
          ALIQ_ISS := 5;
          VL_ISS_P := 2.5;
        end;
      end;
    end;
  end;
  if cbConcomitante.Checked then
  begin
    ACBrSPEDFiscal1.WriteBloco_B;
    LoadToMemo;
  end;
end;

procedure TFrmSPEDFiscal.btnTXTClick(Sender: TObject);
begin
  btnTXT.Enabled := False;

  ACBrSPEDFiscal1.LinhasBuffer := StrToIntDef(edBufLinhas.Text, 0);

  // Limpa a lista de erros.
  memoError.Lines.Clear;
  // Informa o pata onde será salvo o arquivo TXT.
  // ACBrSpedFiscal1.Path := '.\Arquivo Sped TXT\';

  // Método que gera o arquivo TXT.
  ACBrSPEDFiscal1.SaveFileTXT;

  // Carrega o arquivo TXT no memo.
  LoadToMemo;

  // Habilita os botões
  btnB_0.Enabled := True;
  btnTXT.Enabled := True;
  cbConcomitante.Enabled := True;
end;

procedure TFrmSPEDFiscal.btnCancelaGeracaoClick(Sender: TObject);
begin
  ACBrSPEDFiscal1.CancelaGeracao;
end;

procedure TFrmSPEDFiscal.btnErrorClick(Sender: TObject);
begin
  // Limpa a lista de erros.
  memoError.Lines.Clear;

  // Método que gera o arquivo TXT.
  ACBrSPEDFiscal1.SaveFileTXT;

  // Habilita os botões
  btnB_0.Enabled := True;
  btnB_1.Enabled := True;
  btnB_C.Enabled := True;
  btnB_D.Enabled := True;
  btnB_E.Enabled := True;
  btnB_H.Enabled := True;
  btnB_K.Enabled := True;
end;

procedure TFrmSPEDFiscal.btnB_1Click(Sender: TObject);
begin
  btnB_1.Enabled := False;
  btnB_9.Enabled := cbConcomitante.Checked;

  // Alimenta o componente com informações para gerar todos os registros do Bloco 1.
  with ACBrSPEDFiscal1.Bloco_1 do
  begin
    with Registro1001New do
    begin
      IND_MOV := imComDados;

      with Registro1010New do
      begin
        IND_EXP   := 'N'; // Reg. 1100 - Ocorreu averbação (conclusão) de exportação no período:
        IND_CCRF  := 'N'; // Reg. 1200 – Existem informações acerca de créditos de ICMS a serem controlados, definidos pela Sefaz:
        IND_COMB  := 'N'; // Reg. 1300 – É comercio varejista de combustíveis:
        IND_USINA := 'N'; // Reg. 1390 – Usinas de açúcar e/álcool – O estabelecimento é produtor de açúcar e/ou álcool carburante:
        IND_VA    := 'N'; // Reg. 1400 – Existem informações a serem prestadas neste registro e o registro é obrigatório em sua Unidade da Federação:
        IND_EE    := 'N'; // Reg. 1500 - A empresa é distribuidora de energia e ocorreu fornecimento de energia elétrica para consumidores de outra UF:
        IND_CART  := 'N'; // Reg. 1600 - Realizou vendas com Cartão de Crédito ou de débito:
        IND_FORM  := 'N'; // Reg. 1700 - É obrigatório em sua unidade da federação o controle de utilização de documentos  fiscais em papel:
        IND_AER   := 'N'; // Reg. 1800 – A empresa prestou serviços de transporte aéreo de cargas e de passageiros:
        IND_GIAF1 := ifthen(cbEstado.Text = 'PE', 'S', 'N');
        IND_GIAF3 := ifthen(cbEstado.Text = 'PE', 'S', 'N');
        IND_GIAF4 := 'N';
      end;

      with Registro1960New do
      begin
        IND_AP := '1';
        G1_01  := 0.15;
        G1_02  := 5;
        G1_03  := 0.6;
        G1_04  := 0;
        G1_05  := 0;
        G1_06  := 0;
        G1_07  := 0;
        G1_08  := 0;
        G1_09  := 0;
        G1_10  := 0;
        G1_11  := 0;
      end;

      with Registro1970New do
      begin
        IND_AP := '12';
        G3_01  := 1.15;
        G3_02  := 25;
        G3_03  := 0.6;
        G3_04  := 0;
        G3_05  := 0;
        G3_06  := 3;
        G3_07  := 0;
        G3_T   := 0;
        G3_08  := 0;
        G3_09  := 0;
        with Registro1975new do
        begin
          ALIQ_IMP_BASE := 3.5;
          G3_10         := 0.15;
          G3_11         := 6;
          G3_12         := 2;
        end;
        with Registro1975new do
        begin
          ALIQ_IMP_BASE := 10;
          G3_10         := 19;
          G3_11         := 0;
          G3_12         := 3;
        end;
      end;
      with Registro1970New do
      begin
        IND_AP := '10';
        G3_01  := 25;
        G3_02  := 125;
        G3_03  := 3.6;
        G3_04  := 0;
        G3_05  := 0;
        G3_06  := 3;
        G3_07  := 0;
        G3_T   := 0;
        G3_08  := 0;
        G3_09  := 0;
        with Registro1975new do
        begin
          ALIQ_IMP_BASE := 3.5;
          G3_10         := 125;
          G3_11         := 25;
          G3_12         := 3.6;
        end;
      end;


    end;
  end;

  if cbConcomitante.Checked then
  begin
    ACBrSPEDFiscal1.WriteBloco_1;
    LoadToMemo;
  end;
end;

procedure TFrmSPEDFiscal.btnB_CClick(Sender: TObject);
var
  INotas: integer;
  IItens: integer;
  NNotas: integer;
  BNotas: integer;
begin
  // Alimenta o componente com informações para gerar todos os registros do
  // Bloco C.
  btnB_C.Enabled := False;
  btnB_D.Enabled := True;

  NNotas := StrToInt64Def(edNotas.Text, 1);
  BNotas := StrToInt64Def(edBufNotas.Text, 1);

  ProgressBar1.Visible := cbConcomitante.Checked;
  ProgressBar1.Max := NNotas;
  ProgressBar1.Position := 0;

  with ACBrSPEDFiscal1.Bloco_C do
  begin
    with RegistroC001New do
    begin
      IND_MOV := imComDados;
      //
      for INotas := 1 to NNotas do
      begin
        with RegistroC100New do
        begin
          IND_OPER := tpEntradaAquisicao;
          //Só pra variar a emissão entre própria e de terceiros
          if Odd(INotas) then IND_EMIT := edEmissaoPropria
          else IND_EMIT := edTerceiros;

          COD_PART := '1' ; //Baseado no registro 0150
          COD_MOD := '01';
          COD_SIT := sdRegular;
          SER := '';
          NUM_DOC := FormatFloat('11000000', INotas);
          CHV_NFE := '';
          DT_DOC := DT_INI + INotas;
          DT_E_S := DT_INI + INotas;
          VL_DOC := 0;
          IND_PGTO := tpVista;
          VL_DESC := 0;
          VL_ABAT_NT := 0;
          VL_MERC := 10;
          IND_FRT := tfSemCobrancaFrete;
          VL_SEG := 0;
          VL_OUT_DA := 0;
          VL_BC_ICMS := 10;
          VL_ICMS := 1.7;
          VL_BC_ICMS_ST := 0;
          VL_ICMS_ST := 0;
          VL_IPI := 0;
          VL_PIS := 0;
          VL_COFINS := 0;
          VL_PIS_ST := 0;
          VL_COFINS_ST := 0;


          { Gera registros específicos para notas emitidas por terceiros }
          if (INotas = 2) and (IND_EMIT = edTerceiros) then
          begin
            With RegistroC110New do
            begin
              COD_INF := '000001';
              TXT_COMPL := '';

              with RegistroC113New do
              begin
                IND_OPER := tpEntradaAquisicao;
                IND_EMIT := edEmissaoPropria;
                COD_PART := '2';
                COD_MOD := '01';
                SER := '1';
                SUB := '1';
                NUM_DOC := '333';
                DT_DOC := StrToDate('02/11/' + IntToStr(YearOf(DtRef.Date)));
              end;
//              with RegistroC114New do
//              begin
//                COD_MOD := 'a';
//              end;
            end;
          end;

          { Gera o registro de importação apenas para notas de entrada }
          if IND_OPER = tpEntradaAquisicao then
          begin
            with RegistroC120New do
            begin
              COD_DOC_IMP := diSimplificadaImport;
              NUM_DOC__IMP := '1024879531';
              PIS_IMP := 0.00;
              COFINS_IMP := 0.00;
              NUM_ACDRAW := '';
            end;
          end;

          { Gera registros específicos para notas emitidas por terceiros e de prestação }
          if (IND_EMIT = edTerceiros) and (IND_OPER = tpSaidaPrestacao) then
          begin
            with RegistroC130New do
            begin
              VL_SERV_NT := 10.12;
              VL_BC_ISSQN := 10.12;
              VL_ISSQN := 10.12;
              VL_BC_IRRF := 10.12;
              VL_IRRF := 10.12;
              VL_BC_PREV := 10.12;
              VL_PREV := 10.12;
            end;
          end;


          // REGISTRO C170: ITENS DO DOCUMENTO (CÓDIGO 01, 1B, 04 e 55).
          for IItens := 1 to 10 do
          begin
            with RegistroC170New do // Inicio Adicionar os Itens:
            begin
              NUM_ITEM := FormatFloat('000', IItens);
              COD_ITEM := FormatFloat('000000', StrToInt(NUM_ITEM));
              //DESCR_COMPL := 'Nota:'FormatFloat('11000000', INotas) + ' -> ITEM ' + COD_ITEM;
              DESCR_COMPL := 'DESCRIÇÃO Complementar DO ITEM' + FormatFloat('000000', IItens);
              QTD := 1;
              UNID := 'UN';
              VL_ITEM := 1;
              VL_DESC := 0;
              IND_MOV := mfNao;
              CST_ICMS := '010';
              CFOP := '1102';
              COD_NAT := '99991'; //0400
              VL_BC_ICMS := 1;
              ALIQ_ICMS := 17;
              VL_ICMS := 0.17;
              VL_BC_ICMS_ST := 0;
              ALIQ_ST := 0;
              VL_ICMS_ST := 0;
              IND_APUR := iaMensal;
              CST_IPI := CstIpiToStr(stipiEntradaIsenta);
              COD_ENQ := '';
              VL_BC_IPI := 0;
              ALIQ_IPI := 0;
              VL_IPI := 0;
              CST_PIS := CstPisToStr(stpisOutrasOperacoes);
              VL_BC_PIS := 0;
              ALIQ_PIS_PERC := 0;
              QUANT_BC_PIS := 0;
              ALIQ_PIS_R := 0;
              VL_PIS := 0;
              CST_COFINS := CstCofinsToStr(stcofinsOutrasOperacoes);
              VL_BC_COFINS := 0;
              ALIQ_COFINS_PERC := 0;
              QUANT_BC_COFINS := 0;
              ALIQ_COFINS_R := 0;
              VL_COFINS := 0;
              COD_CTA := '000';
              VL_ABAT_NT := 1.39;

              //REGISTRO C171: ARMAZENAMENTO DE COMBUSTIVEIS (código 01, 55)
              { Só gera para operações de aquisição }
//              if IND_OPER = tpEntradaAquisicao then
//              begin
//                with RegistroC171New do
//                begin
//                  NUM_TANQUE := '115';
//                  QTDE := 1.00;
//                end;
//              end;

              //REGISTRO C176: RESSARCIMENTO DE ICMS EM OPERAÇÕES COM
              //SUBSTITUIÇÃO TRIBUTÁRIA (CÓDIGO 01, 55).
//              with RegistroC176New do
//              begin
//                COD_MOD_ULT_E := '55';
//                NUM_DOC_ULT_E := '124567';
//                SER_ULT_E := '1';
//                DT_ULT_E := Now;
//                COD_PART_ULT_E := '000001';
//                QUANT_ULT_E := 10.00;
//                VL_UNIT_ULT_E := 1.00;
//                VL_UNIT_BC_ST := 5.00;
//              end;
            end; // Fim dos Itens;
          end;

          // REGISTRO C190: REGISTRO ANALÍTICO DO DOCUMENTO (CÓDIGO 01, 1B, 04 E 55).
          for IItens := 1 to 1 do
          begin
            with RegistroC190New do
            begin
              CST_ICMS := '010';
              CFOP := '1102';
              ALIQ_ICMS := 17;
              VL_OPR := 0;
              VL_BC_ICMS := 10;
              VL_ICMS := 1.7;
              VL_BC_ICMS_ST := 0;
              VL_ICMS_ST := 0;
              VL_RED_BC := 0;
              VL_IPI := 0;
              COD_OBS := '000001';
              // REGISTRO C191: INFORMAÇÕES DO FUNDO DE COMBATE À POBREZA – FCP – NA NFe (CÓDIGO 55)
              with RegistroC191New do
              begin
                VL_FCP_OP :=  0.34;
                VL_FCP_ST :=  0;
                VL_FCP_RET := 0;
              end; // Fim dos Itens;
            end; // Fim dos Itens;
          end;


        end;

        if cbConcomitante.Checked then
        begin
          if (INotas mod BNotas) = 0 then // Gravar a cada N notas
          begin
            // Grava registros na memoria para o TXT, e limpa memoria
            ACBrSPEDFiscal1.WriteBloco_C(False); // False, NAO fecha o Bloco
            ProgressBar1.Position := INotas;
            Application.ProcessMessages;
          end;
        end;
      end;

      //REGISTRO C400 - EQUIPAMENTO ECF (CÓDIGO 02 e 2D).
      With RegistroC400New do
      begin
        COD_MOD := '2D';
        ECF_MOD := 'DARUMA FS600';
        ECF_FAB := '21098765432123456789';
        ECF_CX := '001';

        With RegistroC405New do
        begin
          DT_DOC := DT_FIN; //StrToDate('30/11/2014');
          CRO := 1;
          CRZ := 1;
          NUM_COO_FIN := 1;
          GT_FIN := 100.00;
          VL_BRT := 100.00;

          With RegistroC410New do
          begin
            VL_PIS := 0.00;
            VL_COFINS := 0.00;
          end;

          With RegistroC420New do
          begin
            COD_TOT_PAR := 'T1700';
            VLR_ACUM_TOT := 100.00;
            NR_TOT := 1;
            DESCR_NR_TOT := 'TOTALIZADOR T1700';

            { Gera este registro somente para empresas do pergil B de apresentação }
            if Bloco_0.Registro0000.IND_PERFIL = pfPerfilB then
            begin
              With RegistroC425New do
              begin
                COD_ITEM := FCod_Item;
                QTD := 1;
                UNID := 'PC';
                VL_ITEM := 100.00;
                VL_PIS := 0.00;
                VL_COFINS := 0.00;
              end;
            end;
          end;

          if Bloco_0.Registro0000.IND_PERFIL <> pfPerfilB then
          begin
            with REgistroC460New do
            begin
              COD_MOD := '2D';
              COD_SIT := sdRegular;
              NUM_DOC := '000001';
              DT_DOC := ACBrSPEDFiscal1.DT_FIN;
              VL_DOC := 100.00;
              VL_PIS := 0.00;
              VL_COFINS := 0.00;
              CPF_CNPJ := '12345678909';
              NOM_ADQ := 'TESTE';

              with RegistroC470New do
              begin
                COD_ITEM := FCod_Item;
                QTD := 1;
                QTD_CANC := 0;
                UNID := 'UN';
                VL_ITEM := 100.00;
                CST_ICMS := '000';
                CFOP := '5102';
                ALIQ_ICMS := 17.00;
                VL_PIS := 0.00;
                VL_COFINS := 0.00;
              end;
            end;
          end;

          with RegistroC490New do
          begin
            CST_ICMS := '000';
            CFOP := '5102';
            ALIQ_ICMS := 17.00;
            VL_OPR := 100.00;
            VL_BC_ICMS := 100.00;
            VL_ICMS := 17.00;
            COD_OBS := '000001'
          end;

          { Só envia este registro se o contribuinte for da BA }
          if Bloco_0.Registro0000.UF = 'BA' then
          begin
            with RegistroC495New do
            begin
              ALIQ_ICMS := 17.00;
              COD_ITEM := FCod_Item;
              QTD := 1.00;
              QTD_CANC := 0.00;
              UNID := 'UN';
              VL_ITEM := 100.00;
              VL_DESC := 0.00;
              VL_CANC := 0.00;
              VL_ACMO := 0.00;
              VL_BC_ICMS := 100.00;
              VL_ICMS := 17.00;
              VL_ISEN := 0.00;
              VL_ICMS_ST := 0.00;
            end;
          end;
        end;
      end;

    end;
  end;

  if cbConcomitante.Checked then
  begin
    ACBrSPEDFiscal1.WriteBloco_C(True); // True, fecha o Bloco
    LoadToMemo;
  end;

  ProgressBar1.Visible := False;

end;

procedure TFrmSPEDFiscal.btnB_CompletoClick(Sender: TObject);
begin
  btnB_0Click(Self);
  btnB_BClick(Self);
  btnB_CClick(Self);
  btnB_DClick(Self);
  btnB_EClick(Self);
  btnB_GClick(Self);
  btnB_HClick(Self);
  btnB_KClick(Self);
  btnB_1Click(Self);
end;

procedure TFrmSPEDFiscal.btnB_DClick(Sender: TObject);
begin
  btnB_D.Enabled := False;
  btnB_E.Enabled := True;

  // Alimenta o componente com informações para gerar todos os registros do Bloco D.
  with ACBrSPEDFiscal1.Bloco_D do
  begin
    with RegistroD001New do
    begin
      IND_MOV := imComDados;

      with RegistroD100New do
      begin
        IND_OPER := tpEntradaAquisicao;
        IND_EMIT := edTerceiros;
        COD_PART := '3';
        COD_MOD := '08';
        COD_SIT := sdRegular;
        SER := '1';
        NUM_DOC := '012345';
        CHV_CTE := '';
        DT_DOC := DT_FIN - 1;
        DT_A_P := DT_FIN - 1;
        TP_CT_e := '1';
        VL_DOC := 100.00;
        VL_DESC := 0.00;
        IND_FRT := tfPorContaEmitente;
        VL_SERV := 100.00;
        VL_BC_ICMS := 100.00;
        VL_ICMS := 17.00;
        VL_NT := 10.10;
        COD_INF := '000001';
        COD_CTA := '111';
      end;

      with RegistroD190New do
      begin
        CST_ICMS := '000';
        CFOP := '1252';
        ALIQ_ICMS := 17.00;
        VL_OPR := 100.00;
        VL_BC_ICMS := 100.00;
        VL_ICMS := 17.00;
        VL_RED_BC := 0.00;
        COD_OBS := '000001';
      end;
    end;
  end;

  if cbConcomitante.Checked then
  begin
    ACBrSPEDFiscal1.WriteBloco_D;
    LoadToMemo;
  end;
end;

procedure TFrmSPEDFiscal.btnB_EClick(Sender: TObject);
const
  ESTADOS: array [0 .. 1] of String = ('RS', 'SC');
var
  I: Integer;
begin
  // Alimenta o componente com informações para gerar todos os registros do Bloco E.
  btnB_E.Enabled := False;
  btnB_G.Enabled := True;

  with ACBrSPEDFiscal1.Bloco_E do
  begin
    with RegistroE001New do
    begin
      IND_MOV := imComDados;

      with RegistroE100New do
      begin
        DT_INI := ACBrSPEDFiscal1.DT_INI;
        DT_FIN := ACBrSPEDFiscal1.DT_FIN;

        with RegistroE110New do
        begin
          VL_TOT_DEBITOS := 17.00;
          VL_AJ_DEBITOS := 0.00;
          VL_TOT_AJ_DEBITOS := 0.00;
          VL_ESTORNOS_CRED := 0;
          VL_TOT_CREDITOS := 20.4;
          VL_AJ_CREDITOS := 0;
          VL_TOT_AJ_CREDITOS := 0;
          VL_ESTORNOS_DEB := 0;
          VL_SLD_CREDOR_ANT := 0;
          VL_SLD_APURADO := 0.00;
          VL_TOT_DED := 0.00;
          VL_ICMS_RECOLHER := 0.00;
          VL_SLD_CREDOR_TRANSPORTAR := 3.4;
          DEB_ESP := 0;

          // with RegistroE111New do begin
          // COD_AJ_APUR    := 'RS009999';
          // DESCR_COMPL_AJ := '';
          // VL_AJ_APUR     := 1.00;
          //
          // with RegistroE112New do begin
          // NUM_DA    := '123';
          // NUM_PROC  := '123';
          // IND_PROC  := opOutros;
          // PROC      := 'DESCRIÇÃO RESUMIDA';
          // TXT_COMPL := 'COMPLEMENTO';
          // end;
          //
          // with RegistroE113New do begin
          // COD_PART := '000001';
          // COD_MOD  := '01';
          // SER      := 'SERI';
          // SUB      := '';
          // NUM_DOC  := '123456789';
          // DT_DOC   := Now;
          // COD_ITEM := '000001';
          // VL_AJ_ITEM := 0;
          // end;
          // end;

          { Rio Grande do Sul não possui as tabelas do registro E115, então este não precisa ser gerado }
          // with RegistroE115New do begin
          // COD_INF_ADIC   := 'RS000001';
          // VL_INF_ADIC    := 0;
          // DESCR_COMPL_AJ := '';
          // end;

          with RegistroE116New do
          begin
            COD_OR := '000';
            VL_OR := 0;
            DT_VCTO := Now;
            COD_REC := '0057';
            NUM_PROC := '10';
            IND_PROC := opSefaz;
            PROC := 'DESCRIÇÃO DO PROCESSO';
            TXT_COMPL := '';
            MES_REF := '112011';
          end;
        end;
      end;

      { Gera um registro E200 e filhos para cada estado onde o contribuinte possui inscrição estadual }
      for I := Low(ESTADOS) to High(ESTADOS) do
      begin
        with RegistroE200New do
        begin
          DT_INI := ACBrSPEDFiscal1.DT_INI;
          DT_FIN := ACBrSPEDFiscal1.DT_FIN;
          UF := ESTADOS[I];

          with RegistroE210New do
          begin
            IND_MOV_ST := mstSemOperacaoST;
            VL_SLD_CRED_ANT_ST := 0;
            VL_DEVOL_ST := 0;
            VL_RESSARC_ST := 0;
            VL_OUT_CRED_ST := 0;
            VL_AJ_CREDITOS_ST := 0;
            VL_SLD_DEV_ANT_ST := 0.00;
            VL_DEDUCOES_ST := 0;
            VL_ICMS_RECOL_ST := 0.00;
            VL_SLD_CRED_ST_TRANSPORTAR := 0;
            VL_OUT_DEB_ST := 0.00;
            DEB_ESP_ST := 0;

            // with RegistroE220New do begin
            // COD_AJ_APUR    := 'RS109999';
            // DESCR_COMPL_AJ := '';
            // VL_AJ_APUR     := 0.00;
            //
            // with RegistroE230New do begin
            // NUM_DA    := '123';
            // NUM_PROC  := '123';
            // IND_PROC  := opOutros;
            // PROC      := 'DESCRIÇÃO RESUMIDA';
            // TXT_COMPL := 'COMPLEMENTO';
            // end;
            //
            // with RegistroE240New do begin
            // COD_PART   := '000001';
            // COD_MOD    := '01';
            // SER        := 'SERI';
            // SUB        := '';
            // NUM_DOC    := '123456789';
            // DT_DOC     := Now;
            // COD_ITEM   := '000001';
            // VL_AJ_ITEM := 0;
            // end;
            // end;

            with RegistroE250New do
            begin
              COD_OR := '001';
              VL_OR := 0;
              DT_VCTO := Now;
              COD_REC := '600016';
              NUM_PROC := '1020304050';
              IND_PROC := opOutros;
              PROC := 'DESCRIÇÃO RESUMIDA';
              TXT_COMPL := '';
              MES_REF := '112011';
            end;
          end;
        end;
      end;


      //Só deve gerar o bloco E em caso de atividades Industriais
      if ACBrSPEDFiscal1.Bloco_0.Registro0000.IND_ATIV = atIndustrial then
      begin
        with RegistroE500New do
        begin
          IND_APUR := iaMensal;
          DT_INI := ACBrSPEDFiscal1.DT_INI;
          DT_FIN := ACBrSPEDFiscal1.DT_FIN;

          with RegistroE510New do
          begin
            CFOP := '5120';
            CST_IPI := '50';
            VL_CONT_IPI := 0;
            VL_BC_IPI := 0;
            VL_IPI := 0;
          end;

          with RegistroE520New do
          begin
            VL_SD_ANT_IPI := 0;
            VL_DEB_IPI := 0;
            VL_CRED_IPI := 0;
            VL_OD_IPI := 10.00;
            VL_OC_IPI := 0;
            VL_SC_IPI := 0;
            VL_SD_IPI := 10.00;

            with RegistroE530New do
            begin
              IND_AJ := ajDebito;
              VL_AJ := 10;
              COD_AJ := '001';
              IND_DOC := odOutros;
              NUM_DOC := '123';
              DESCR_AJ := 'DESCRIÇÃO DETALHADA';
            end;
          end;
          { fim registro E500 }
        end;
      end;
    end;
  end;

  if cbConcomitante.Checked then
  begin
    ACBrSPEDFiscal1.WriteBloco_E;
    LoadToMemo;
  end;
end;

procedure TFrmSPEDFiscal.btnB_GClick(Sender: TObject);
begin
  btnB_G.Enabled := False;
  btnB_H.Enabled := True;

  // Alimenta o componente com informações para gerar todos os registros do Bloco G.
  with ACBrSPEDFiscal1.Bloco_G do
  begin
    with RegistroG001New do
    begin
      IND_MOV := imComDados;

      With RegistroG110New do
      begin
        DT_INI := ACBrSPEDFiscal1.DT_INI;
        DT_FIN := ACBrSPEDFiscal1.DT_FIN;
        SALDO_IN_ICMS := 44.00;
        SOM_PARC := 4.40;
        VL_TRIB_EXP := 10.999;
        VL_TOTAL := 10.999;
        IND_PER_SAI := 1.00;
        ICMS_APROP := 4.40;
        SOM_ICMS_OC := 10.999;

        With RegistroG125New do
        begin
          COD_IND_BEM := '000001';
          DT_MOV := ACBrSPEDFiscal1.DT_INI;
          TIPO_MOV := mbcSI;
          VL_IMOB_ICMS_OP := 10.999;
          VL_IMOB_ICMS_ST := 10.999;
          VL_IMOB_ICMS_FRT := 10.999;
          VL_IMOB_ICMS_DIF := 10.999;
          NUM_PARC := 10;
          VL_PARC_PASS := 4.40;

          With RegistroG126New do
          begin
            DT_INI := StrToDate('01/10/' + IntToStr(YearOf(DtRef.Date)));;
            DT_FIN := StrToDate('30/10/' + IntToStr(YearOf(DtRef.Date)));;
            NUM_PARC := 1234;
            VL_PARC_PASS := 10.999;
            VL_TRIB_OC := 10.999;
            VL_TOTAL := 10.999;
            IND_PER_SAI := 1.00;
            VL_PARC_APROP := 10.999;
          end;

          With RegistroG130New do
          begin
            IND_EMIT := edEmissaoPropria;
            COD_PART := '4';
            COD_MOD := '55';
            SERIE := '1';
            NUM_DOC := '000068849';
            CHV_NFE_CTE := '35100260318797000100550010000688490882775007';
            DT_DOC := DT_INI;

            With RegistroG140New do
            begin
              NUM_ITEM := '9999';
              COD_ITEM := FCod_Item;
            end;
          end;
        end;
      end;
    end;
  end;

  if cbConcomitante.Checked then
  begin
    ACBrSPEDFiscal1.WriteBloco_G;
    LoadToMemo;
  end;

end;

procedure TFrmSPEDFiscal.btnB_HClick(Sender: TObject);
var
  IInvent: integer;
begin
  btnB_H.Enabled := False;
  btnB_K.Enabled := True;

  // Alimenta o componente com informações para gerar todos os registros do
  // Bloco H.
  with ACBrSPEDFiscal1.Bloco_H do
  begin
    with RegistroH001New do
    begin
      IND_MOV := imComDados;
      //
      with RegistroH005New do
      begin
        DT_INV := DT_FIN; //o valor informado no campo deve ser menor ou igual ao valor no campo DT_FIN do registro 0000
        VL_INV := 1000;
        // FILHO
        for IInvent := 1 to 10 do
        begin
          with RegistroH010New do
          begin
            COD_ITEM := FormatFloat('000000', IInvent);
            UNID := strUNID[IInvent mod (High(strUNID)+1)];
            QTD := 1;
            VL_UNIT := 100;
            VL_ITEM := 100;
            IND_PROP := piInformante;
            COD_PART := '';
            TXT_COMPL := '';
            COD_CTA := '1';

            with RegistroH020New do
            begin
              CST_ICMS := '010';
              BC_ICMS  := 1;
              VL_ICMS := 2;
            end;

          end;
        end;
      end;
    end;
  end;

  if cbConcomitante.Checked then
  begin
    ACBrSPEDFiscal1.WriteBloco_H;
    LoadToMemo;
  end;
end;

procedure TFrmSPEDFiscal.btnB_KClick(Sender: TObject);
begin
  btnB_K.Enabled := False;
  btnB_1.Enabled := True;


  // Alimenta o componente com informações para gerar todos os registros do
  // Bloco K.
  with ACBrSPEDFiscal1.Bloco_K do
  begin
    with RegistroK001New do
    begin
      IND_MOV := imComDados;

      with RegistroK100New do
      begin
        DT_INI := ACBrSPEDFiscal1.DT_INI;
        DT_FIN := ACBrSPEDFiscal1.DT_FIN;

        with RegistroK200New do
        begin
          COD_ITEM := FCod_Item;
          QTD := 1;
          IND_EST := estPropInformantePoder;
          COD_PART := '';
          DT_EST := ACBrSPEDFiscal1.DT_FIN;

          with RegistroK220New do
          begin
            DT_MOV := ACBrSPEDFiscal1.DT_INI;
            COD_ITEM_ORI := '000008';
            COD_ITEM_DEST := '000010';
            QTD := 1;
            QTD_DEST := 1;            
          end;
          with RegistroK260New do
          begin
            COD_OP_OS := '00035';
            COD_ITEM  := '00006';
            DT_SAIDA  :=  ACBrSPEDFiscal1.DT_INI;
            QTD_SAIDA :=  1;
            DT_RET    := ACBrSPEDFiscal1.DT_INI;
            QTD_RET   :=  1;
            with RegistroK265New do
            begin
              COD_ITEM := '000008';
              QTD_CONS := 1;
              QTD_RET := 1;
            end;
          end;
        end;
      end;
    end;
  end;

  if cbConcomitante.Checked then
  begin
    ACBrSPEDFiscal1.WriteBloco_K;
    LoadToMemo;
  end;
end;

procedure TFrmSPEDFiscal.edtFileChange(Sender: TObject);
begin
  ACBrSPEDFiscal1.Arquivo := edtFile.Text;
end;

procedure TFrmSPEDFiscal.LoadToMemo;
begin
  memoTXT.Lines.Clear;
  if FileExists(ACBrSPEDFiscal1.Path + ACBrSPEDFiscal1.Arquivo) then
    memoTXT.Lines.LoadFromFile(ACBrSPEDFiscal1.Path + ACBrSPEDFiscal1.Arquivo);
end;

procedure TFrmSPEDFiscal.cbConcomitanteClick(Sender: TObject);
begin
  btnTXT.Enabled := not cbConcomitante.Checked;
  btnError.Enabled := btnTXT.Enabled;

  edBufNotas.Enabled := cbConcomitante.Checked;

  if not cbConcomitante.Checked then
  begin
    btnB_0.Enabled := True;
    btnB_C.Enabled := False;
    btnB_D.Enabled := False;
    btnB_E.Enabled := False;
    btnB_H.Enabled := False;
    btnB_K.Enabled := False;
    btnB_1.Enabled := False;
    btnB_9.Enabled := False;
  end;
end;

end.
