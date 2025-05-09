{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Renato Rubinho                                  }
{                                                                              }
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

unit Frm_SPEDECF;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
{$IFNDEF FPC}
  Windows, Messages,
{$ENDIF}
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DateUtils,
  ACBrSpedECF, ExtCtrls, ComCtrls, EditBtn,
  ACBrECFBlocos;

type

  { TFrmSPEDECF }

  TFrmSPEDECF = class(TForm)
    btnB_0: TButton;
    btnB_J: TButton;
    btnB_K: TButton;
    btnB_L: TButton;
    btnB_P: TButton;
    btnError: TButton;
    btnTXT: TButton;
    btnB_9: TButton;
    cbConcomitante: TCheckBox;
    DtRef: TDateEdit;
    edBufLinhas: TEdit;
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
    btnB_M: TButton;
    btnB_Completo: TButton;
    btnCancelaGeracao: TButton;
    btnB_Y: TButton;
    Label9: TLabel;
    ACBrSPEDECF1: TACBrSPEDECF;
    btnB_Q: TButton;
    btnB_X: TButton;
    procedure btnB_YClick(Sender: TObject);
    procedure btnB_0Click(Sender: TObject);
    procedure btnB_9Click(Sender: TObject);
    procedure btnTXTClick(Sender: TObject);
    procedure btnB_LClick(Sender: TObject);
    procedure btnB_PClick(Sender: TObject);
    procedure btnErrorClick(Sender: TObject);
    procedure edtFileChange(Sender: TObject);
    procedure cbConcomitanteClick(Sender: TObject);
    procedure btnB_CompletoClick(Sender: TObject);
    procedure btnB_MClick(Sender: TObject);
    procedure btnCancelaGeracaoClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ACBrSPEDECF1Error(const MsnError: string);
    procedure btnB_JClick(Sender: TObject);
    procedure btnB_KClick(Sender: TObject);
    procedure btnB_QClick(Sender: TObject);
    procedure btnB_XClick(Sender: TObject);
  private
    procedure LoadToMemo;
    function AnoToVersao(ADtRef: TDateTime): TACBrECFCodVer;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmSPEDECF: TFrmSPEDECF;

implementation

{$R *.lfm}

procedure TFrmSPEDECF.ACBrSPEDECF1Error(const MsnError: string);
begin
  memoError.Lines.Add(MsnError);
end;

function TFrmSPEDECF.AnoToVersao(ADtRef: TDateTime): TACBrECFCodVer;
var
  xVer: string;
  iVer: integer;
  Ano: integer;
begin
  Ano := YearOf(ADtRef);
  iVer := Ano - 2013;
  xVer := Format('%.4d', [iVer]);

  try
    Result := StrToCodVer(xVer);
  Except
    Result := TACBrECFCodVer(High(TACBrECFCodVer));
  end;
end;

procedure TFrmSPEDECF.btnB_0Click(Sender: TObject);
var
  i: integer;
begin
  // Alimenta o componente com informações para gerar todos os registros do
  // Bloco 0.
  cbConcomitante.Enabled := False;
  btnB_0.Enabled := False;
  btnB_J.Enabled := True;

  ACBrSPEDECF1.DT_INI := EncodeDate(YearOf(DtRef.Date),1,1);
  ACBrSPEDECF1.DT_FIN := EncodeDate(YearOf(DtRef.Date),12,31);

  if cbConcomitante.Checked then
  begin
    ACBrSPEDECF1.LinhasBuffer := StrToIntDef(edBufLinhas.Text, 0);
    ACBrSPEDECF1.IniciaGeracao;
    LoadToMemo;
  end;

  with ACBrSPEDECF1.Bloco_0 do
  begin
    // Dados da Empresa
    with Registro0000New do
    begin
      COD_VER := AnoToVersao(ACBrSPEDECF1.DT_INI);
      CNPJ := '12345678000195';
      NOME := 'RAZÃO SOCIAL DA EMPRESA OU SCP';
      IND_SIT_INI_PER := '0';
      SIT_ESPECIAL := '0';
      PAT_REMAN_CIS := 0;
      DT_SIT_ESP := 0;
      DT_INI := ACBrSPEDECF1.DT_INI;
      DT_FIN := ACBrSPEDECF1.DT_FIN;
      RETIFICADORA := 'N';
      NUM_REC := '';
      TIP_ECF := '0';
      COD_SCP := '';
    end;

    // ABERTURA DO BLOCO 0
    with Registro0001New do
    begin
      IND_DAD := idComDados;

      // PARÂMETROS DE TRIBUTAÇÃO
      with Registro0010New do
      begin
        HASH_ECF_ANTERIOR := '';
        OPT_REFIS := idNao;
        FORMA_TRIB := ftlLucroReal;
        FORMA_APUR := 'A';
        COD_QUALIF_PJ := '01';
        FORMA_TRIB_PER := 'R';
        MES_BAL_RED := 'E';
        TIP_ESC_PRE := 'L';
        TIP_ENT := '';
        FORMA_APUR_I := '';
        APUR_CSLL := '';
        IND_REC_RECEITA := irrRegimeCaixa;
      end;

      // PARÂMETROS COMPLEMENTARES
      with Registro0020New do
      begin
        IND_QTE_SCP := 0;
        IND_ALIQ_CSLL := '1';
        IND_ADM_FUN_CLU := 'N';
        IND_PART_CONS := 'N';
        IND_OP_EXT := 'N';
        IND_OP_VINC := 'N';
        IND_PJ_ENQUAD := 'N';
        IND_ATIV_RURAL := 'N';
        IND_LUC_EXP := 'N';
        IND_RED_ISEN := 'N';
        IND_FIN := 'N';
        IND_DOA_ELEIT := 'N';
        IND_PART_COLIG := 'N';
        IND_VEND_EXP := 'N';
        IND_REC_EXT := 'N';
        IND_ATIV_EXT := 'N';
        IND_COM_EXP := 'N';
        IND_PGTO_EXT := 'N';
        IND_ECOM_TI := 'N';
        IND_ROY_REC := 'N';
        IND_ROY_PAG := 'N';
        IND_REND_SERV := 'N';
        IND_PGTO_REM := 'N';
        IND_INOV_TEC := 'N';
        IND_CAP_INF := 'N';
        IND_PJ_HAB := 'N';
        IND_POLO_AM := 'N';
        IND_ZON_EXP := 'N';
        IND_AREA_COM := 'N';
        IND_PAIS_A_PAIS := 'N';
        IND_DEREX := 'N';
        IND_PR_TRANSF := 'N';
      end;
    end;

    // DADOS CADASTRAIS
    with Registro0030New do
    begin
      COD_NAT := '1';
      CNAE_FISCAL := '1234567';
      ENDERECO := 'ENDERECO';
      NUM := 'NUM';
      COMPL := 'COMPL';
      BAIRRO := 'BAIRRO';
      UF := 'SP';
      COD_MUN := '3503105';
      CEP := '00000000';
      NUM_TEL := '123456789';
      EMAIL := 'email@tes.te';
    end;

    // IDENTIFICAÇÃO DAS SCP
    for i := 0 to 0 do
    begin
      with Registro0035New do
      begin
        COD_SCP := '12345678000195';
        NOME_SCP := 'NOME_SCP';
      end;
    end;

    // IDENTIFICAÇÃO DOS SIGNATÁRIOS DA ECF
    for i := 0 to 0 do
    begin
      with Registro0930New do
      begin
        IDENT_NOM := 'IDENT_NOM';
        IDENT_CPF_CNPJ := '12345678000195';
        IND_CRC := '12345';
        EMAIL := 'email@tes.te';
        FONE := '123456789';
        IDENT_QUALIF := qaContador;
      end;
    end;
  end;

  if cbConcomitante.Checked then
  begin
    ACBrSPEDECF1.WriteBloco_0;
    LoadToMemo;
  end;
end;

procedure TFrmSPEDECF.btnB_9Click(Sender: TObject);
begin
  btnB_9.Enabled := False;
  ACBrSPEDECF1.WriteBloco_9;
  LoadToMemo;

  // Habilita os botões
  btnB_0.Enabled := True;
  btnB_J.Enabled := True;
  btnB_K.Enabled := True;
  btnB_L.Enabled := True;
  btnB_M.Enabled := True;
  btnB_P.Enabled := True;
  btnB_Q.Enabled := True;
  btnB_X.Enabled := True;
  btnB_Y.Enabled := True;

  cbConcomitante.Enabled := True;
end;

procedure TFrmSPEDECF.btnTXTClick(Sender: TObject);
begin
  btnTXT.Enabled := False;

  ACBrSPEDECF1.LinhasBuffer := StrToIntDef(edBufLinhas.Text, 0);

  // Limpa a lista de erros.
  memoError.Lines.Clear;
  // Informa o pata onde será salvo o arquivo TXT.
  // ACBrSPEDECF1.Path := '.\Arquivo Sped TXT\';

  // Método que gera o arquivo TXT.
  ACBrSPEDECF1.SaveFileTXT;

  // Carrega o arquivo TXT no memo.
  LoadToMemo;

  // Habilita os botões
  btnB_0.Enabled := True;
  btnTXT.Enabled := True;
  cbConcomitante.Enabled := True;
end;

procedure TFrmSPEDECF.btnCancelaGeracaoClick(Sender: TObject);
begin
  ACBrSPEDECF1.CancelaGeracao;
end;

procedure TFrmSPEDECF.btnErrorClick(Sender: TObject);
begin
  // Limpa a lista de erros.
  memoError.Lines.Clear;

  // Método que gera o arquivo TXT.
  ACBrSPEDECF1.SaveFileTXT;

  // Habilita os botões
  btnB_0.Enabled := True;
  btnB_J.Enabled := True;
  btnB_K.Enabled := True;
  btnB_L.Enabled := True;
  btnB_M.Enabled := True;
  btnB_P.Enabled := True;
  btnB_Q.Enabled := True;
  btnB_X.Enabled := True;
  btnB_Y.Enabled := True;
end;

procedure TFrmSPEDECF.btnB_CompletoClick(Sender: TObject);
begin
  btnB_0Click(Self);
  btnB_JClick(Self);
  btnB_KClick(Self);
  btnB_LClick(Self);
  btnB_MClick(Self);
  btnB_PClick(Self);
  btnB_QClick(Self);
  btnB_XClick(Self);
  btnB_YClick(Self);
end;

procedure TFrmSPEDECF.btnB_LClick(Sender: TObject);
var
  i: integer;
  trimestre: Integer;
  data: TDate;
begin
  // Alimenta o componente com informações para gerar todos os registros do
  // Bloco L.
  cbConcomitante.Enabled := False;
  btnB_L.Enabled := False;
  btnB_M.Enabled := True;

  with ACBrSPEDECF1.Bloco_L do
  begin
    // ABERTURA DO BLOCO L
    with RegistroL001New do
    begin
      IND_DAD := idComDados;

      // IDENTIFICAÇÃO DO PERÍODO
      trimestre := 1;
      data := 0;

      case MonthOf(ACBrSPEDECF1.DT_INI) of
        1..3: trimestre := 1;
        4..6: trimestre := 2;
        7..9: trimestre := 3;
        10..12: trimestre := 4;
      end;

      while (trimestre <= 4) do
      begin
        with RegistroL030New do
        begin
          if (data = 0) then
            DT_INI := ACBrSPEDECF1.DT_INI
          else
            DT_INI := data;

          case trimestre of
            1: data := EncodeDate(YearOf(ACBrSPEDECF1.DT_INI), 3, 31);
            2: data := EncodeDate(YearOf(ACBrSPEDECF1.DT_INI), 6, 30);
            3: data := EncodeDate(YearOf(ACBrSPEDECF1.DT_INI), 9, 30);
            4: data := EncodeDate(YearOf(ACBrSPEDECF1.DT_INI), 12, 31);
          end;

          DT_FIN := EndOfTheMonth(data);
          PER_APUR := 'T0' + IntToStr(trimestre);

          // BALANÇO PATRIMONIAL
          for i := 0 to 0 do
          begin
            with RegistroL100New do
            begin
              CODIGO := '00.000.000';
              DESCRICAO := 'DESCRICAO';
              TIPO := 'A';
              NIVEL := 1;
              COD_NAT := '01';
              COD_CTA_SUP := '';
              VAL_CTA_REF_INI := 100;
              IND_VAL_CTA_REF_INI := 'C';
              VAL_CTA_REF_DEB := 0;
              VAL_CTA_REF_CRED := 100;
              VAL_CTA_REF_FIN := 200;
              IND_VAL_CTA_REF_FIN := 'C';
            end;
          end;

          // MÉTODO DE AVALIAÇÃO DO ESTOQUE FINAL
          with RegistroL200New do
          begin
            IND_AVAL_ESTOQ := '1';
          end;

          // INFORMATIVO DE COMPOSIÇÃO DE CUSTOS
          for i := 0 to 0 do
          begin
            with RegistroL210New do
            begin
              CODIGO := '00.000.000';
              DESCRICAO := 'DESCRICAO';
              VALOR := 200;
            end;
          end;

          // DEMONSTRATIVO DO RESULTADO LÍQUIDO NO PERÍODO FISCAL
          for i := 0 to 0 do
          begin
            with RegistroL300New do
            begin
              CODIGO := '00.000.000';
              DESCRICAO := 'DESCRICAO';
              TIPO := 'A';
              NIVEL := 1;
              COD_NAT := '01';
              COD_CTA_SUP := '';
              VALOR := 200;
              IND_VALOR := 'C';
            end;
          end;
        end;

        data := data + 1;
        Inc(trimestre);
      end;
    end;
  end;

  if cbConcomitante.Checked then
  begin
    ACBrSPEDECF1.WriteBloco_L;
    LoadToMemo;
  end;
end;

procedure TFrmSPEDECF.btnB_MClick(Sender: TObject);
var
  i: integer;
  j: integer;
  k: integer;
  trimestre: Integer;
  data: TDate;
begin
  // Alimenta o componente com informações para gerar todos os registros do
  // Bloco M.
  cbConcomitante.Enabled := False;
  btnB_M.Enabled := False;
  btnB_P.Enabled := True;

  with ACBrSPEDECF1.Bloco_M do
  begin
    // ABERTURA DO BLOCO M
    with RegistroM001New do
    begin
      IND_DAD := idComDados;

      // IDENTIFICAÇÃO DA CONTA NA PARTE B DO e-LALUR E DO e-LACS
      for i := 0 to 0 do
      begin
        with RegistroM010New do
        begin
          COD_CTA_B := '00.000.000';
          DESC_CTA_LAL := 'DESC_CTA_LAL';
          DT_AP_LAL := ACBrSPEDECF1.DT_FIN;
          COD_PB_RFB := '000001';
          DT_LIM_LAL := ACBrSPEDECF1.DT_FIN;
          COD_TRIBUTO := 'I';
          VL_SALDO_INI := 100;
          IND_VL_SALDO_INI := 'C';
          CNPJ_SIT_ESP := '12345678000195';
        end;
      end;

      // IDENTIFICAÇÃO DO PERÍODO
      trimestre := 1;
      data := 0;

      case MonthOf(ACBrSPEDECF1.DT_INI) of
        1..3: trimestre := 1;
        4..6: trimestre := 2;
        7..9: trimestre := 3;
        10..12: trimestre := 4;
      end;

      while (trimestre <= 4) do
      begin
        with RegistroM030New do
        begin
          if (data = 0) then
            DT_INI := ACBrSPEDECF1.DT_INI
          else
            DT_INI := data;

          case trimestre of
            1: data := EncodeDate(YearOf(ACBrSPEDECF1.DT_INI), 3, 31);
            2: data := EncodeDate(YearOf(ACBrSPEDECF1.DT_INI), 6, 30);
            3: data := EncodeDate(YearOf(ACBrSPEDECF1.DT_INI), 9, 30);
            4: data := EncodeDate(YearOf(ACBrSPEDECF1.DT_INI), 12, 31);
          end;

          DT_FIN := EndOfTheMonth(data);
          PER_APUR := 'T0' + IntToStr(trimestre);

          // LANÇAMENTOS DA PARTE A DO e-LALUR
          for i := 0 to 0 do
          begin
            with RegistroM300New do
            begin
              CODIGO := '001';
              DESCRICAO := 'DESCRICAO';
              TIPO_LANCAMENTO := 'A';
              IND_RELACAO := 1;
              VALOR := 200;
              HIST_LAN_LAL := 'HISTORICO';

              // CONTA DA PARTE B DO e-LALUR
              for j := 0 to 0 do
              begin
                with RegistroM305New do
                begin
                  COD_CTA_B := '00.000.000';
                  VL_CTA := 200;
                  IND_VL_CTA := 'C';
                end;
              end;

              // CONTAS CONTABEIS RELACIONADAS AO LANÇAMENTO DA PARTE A DO e-LALUR
              for j := 0 to 0 do
              begin
                with RegistroM310New do
                begin
                  COD_CTA := '00.000.000';
                  VL_CTA := 200;
                  IND_VL_CTA := 'C';

                  // NÚMEROS DOS LANÇAMENTOS RELACIONADOS À CONTA CONTÁBIL
                  for k := 0 to 0 do
                  begin
                    with RegistroM312New do
                    begin
                      NUM_LCTO := '0001';
                    end;
                  end;
                end;
              end;
            end;
          end;

          // LANÇAMENTOS DA PARTE A DO e-LACS
          for i := 0 to 0 do
          begin
            with RegistroM350New do
            begin
              CODIGO := '001';
              DESCRICAO := 'DESCRICAO';
              TIPO_LANCAMENTO := 'A';
              IND_RELACAO := 1;
              VALOR := 200;
              HIST_LAN_LAL := 'HISTORICO';

              // CONTA DA PARTE B DO e-LACS
              for j := 0 to 0 do
              begin
                with RegistroM355New do
                begin
                  COD_CTA_B := '00.000.000';
                  VL_CTA := 200;
                  IND_VL_CTA := 'C';
                end;
              end;

              // CONTAS CONTABEIS RELACIONADAS AO LANÇAMENTO DA PARTE A DO e-LACS
              for j := 0 to 0 do
              begin
                with RegistroM360New do
                begin
                  COD_CTA := '00.000.000';
                  COD_CCUS := '00.000.000';
                  VL_CTA := 200;
                  IND_VL_CTA := 'C';

                  // NÚMEROS DOS LANÇAMENTOS RELACIONADOS À CONTA CONTÁBIL
                  for k := 0 to 0 do
                  begin
                    with RegistroM362New do
                    begin
                      NUM_LCTO := '0001';
                    end;
                  end;
                end;
              end;
            end;
          end;

          // LANÇAMENTOS NA CONTA DA PARTE “B” DO e-LALUR e do e-LACS SEM REFLEXO NA PARTE A
          for i := 0 to 0 do
          begin
            with RegistroM410New do
            begin
              COD_CTA_B := '00.000.000';
              COD_TRIBUTO := 'I';
              VAL_LAN_LALB_PB := 200;
              IND_VAL_LAN_LALB_PB := 'CR';
              COD_CTA_B_CTP := '00.000.000';
              HIST_LAN_LALB := 'HIST_LAN_LALB';
              IND_LAN_ANT := 'S';

              // IDENTIFICAÇÃO DE PROCESSOS JUDICIAIS E ADMINISTRATIVOS REFERENTES AO LANÇAMENTO
              for j := 0 to 0 do
              begin
                with RegistroM415New do
                begin
                  IND_PROC := '1';
                  NUM_PROC := '12345678901234567890';
                end;
              end;
            end;
          end;
        end;

        data := data + 1;
        Inc(trimestre);
      end;
    end;
  end;

  if cbConcomitante.Checked then
  begin
    ACBrSPEDECF1.WriteBloco_M;
    LoadToMemo;
  end;
end;

procedure TFrmSPEDECF.btnB_PClick(Sender: TObject);
var
  i: integer;
  trimestre: Integer;
  data: TDate;
begin
  // Alimenta o componente com informações para gerar todos os registros do
  // Bloco P.
  cbConcomitante.Enabled := False;
  btnB_P.Enabled := False;
  btnB_Q.Enabled := True;

  with ACBrSPEDECF1.Bloco_P do
  begin
    // ABERTURA DO BLOCO P
    with RegistroP001New do
    begin
      IND_DAD := idComDados;

      // IDENTIFICAÇÃO DOS PERÍODO E FORMAS DE APURAÇÃO DO IRPJ E DA CSLL DAS EMPRESAS TRIBUTADAS PELO LUCRO PRESUMIDO
      trimestre := 1;
      data := 0;

      case MonthOf(ACBrSPEDECF1.DT_INI) of
        1..3: trimestre := 1;
        4..6: trimestre := 2;
        7..9: trimestre := 3;
        10..12: trimestre := 4;
      end;

      while (trimestre <= 4) do
      begin
        with RegistroP030New do
        begin
          if (data = 0) then
            DT_INI := ACBrSPEDECF1.DT_INI
          else
            DT_INI := data;

          case trimestre of
            1: data := EncodeDate(YearOf(ACBrSPEDECF1.DT_INI), 3, 31);
            2: data := EncodeDate(YearOf(ACBrSPEDECF1.DT_INI), 6, 30);
            3: data := EncodeDate(YearOf(ACBrSPEDECF1.DT_INI), 9, 30);
            4: data := EncodeDate(YearOf(ACBrSPEDECF1.DT_INI), 12, 31);
          end;

          DT_FIN := EndOfTheMonth(data);
          PER_APUR := 'T0' + IntToStr(trimestre);

          // BALANÇO PATRIMONIAL
          for i := 0 to 0 do
          begin
            with RegistroP100New do
            begin
              CODIGO := '00.000.000';
              DESCRICAO := 'DESCRICAO';
              TIPO := 'A';
              NIVEL := 1;
              COD_NAT := '01';
              COD_CTA_SUP := '';
              VAL_CTA_REF_INI := 100;
              IND_VAL_CTA_REF_INI := 'C';
              VAL_CTA_REF_FIN := 200;
              IND_VAL_CTA_REF_FIN := 'C';
            end;
          end;

          // DEMONSTRAÇÃO DAS RECEITAS INCENTIVADAS DO LUCRO PRESUMIDO
          for i := 0 to 0 do
          begin
            with RegistroP130New do
            begin
              CODIGO := '00.000.000';
              DESCRICAO := 'DESCRICAO';
              VALOR := 200;
            end;
          end;

          // DEMONSTRATIVO DO RESULTADO LÍQUIDO NO PERÍODO FISCAL
          for i := 0 to 0 do
          begin
            with RegistroP150New do
            begin
              CODIGO := '00.000.000';
              DESCRICAO := 'DESCRICAO';
              TIPO := 'A';
              NIVEL := 1;
              COD_NAT := '01';
              COD_CTA_SUP := '';
              VALOR := 100;
              IND_VALOR := 'C';
            end;
          end;

          // APURAÇÃO DA BASE DE CÁLCULO DO IRPJ COM BASE NO LUCRO PRESUMIDO
          for i := 0 to 0 do
          begin
            with RegistroP200New do
            begin
              CODIGO := '00.000.000';
              DESCRICAO := 'DESCRICAO';
              VALOR := 200;
            end;
          end;

          // CÁLCULO DA ISENÇÃO E REDUÇÃO DO LUCRO PRESUMIDO
          for i := 0 to 0 do
          begin
            with RegistroP230New do
            begin
              CODIGO := '00.000.000';
              DESCRICAO := 'DESCRICAO';
              VALOR := 200;
            end;
          end;

          // CÁLCULO DO IRPJ COM BASE DO LUCRO PRESUMIDO
          for i := 0 to 0 do
          begin
            with RegistroP300New do
            begin
              CODIGO := '00.000.000';
              DESCRICAO := 'DESCRICAO';
              VALOR := 200;
            end;
          end;

          // APURAÇÃO DA BASE DE CÁLCULO DA CSLL COM BASE DO LUCRO PRESUMIDOREGISTRO P300: CÁLCULO DO IRPJ COM BASE DO LUCRO PRESUMIDO
          for i := 0 to 0 do
          begin
            with RegistroP400New do
            begin
              CODIGO := '00.000.000';
              DESCRICAO := 'DESCRICAO';
              VALOR := 200;
            end;
          end;

          // CÁLCULO DA CSLL COM BASE DO LUCRO PRESUMIDO
          for i := 0 to 0 do
          begin
            with RegistroP500New do
            begin
              CODIGO := '00.000.000';
              DESCRICAO := 'DESCRICAO';
              VALOR := 200;
            end;
          end;
        end;

        data := data + 1;
        Inc(trimestre);
      end;
    end;
  end;

  if cbConcomitante.Checked then
  begin
    ACBrSPEDECF1.WriteBloco_P;
    LoadToMemo;
  end;
end;

procedure TFrmSPEDECF.btnB_QClick(Sender: TObject);
var
  i: integer;
begin
  // Alimenta o componente com informações para gerar todos os registros do
  // Bloco Q.
  cbConcomitante.Enabled := False;
  btnB_Q.Enabled := False;
  btnB_X.Enabled := True;

  with ACBrSPEDECF1.Bloco_Q do
  begin
    // ABERTURA DO BLOCO 0
    with RegistroQ001New do
    begin
      IND_DAD := idComDados;

      // DEMONSTRATIVO DO LIVRO CAIXA
      for i := 0 to 0 do
      begin
        with RegistroQ100New do
        begin
          DATA := ACBrSPEDECF1.DT_INI;
          NUM_DOC := '';
          HIST := 'HIST';
          VL_ENTRADA := 200;
          VL_SAIDA := 0;
          SLD_FIN := 200;
        end;
      end;
    end;
  end;

  if cbConcomitante.Checked then
  begin
    ACBrSPEDECF1.WriteBloco_Q;
    LoadToMemo;
  end;
end;

procedure TFrmSPEDECF.btnB_XClick(Sender: TObject);
var
  i: integer;
  j: integer;
begin
  // Alimenta o componente com informações para gerar todos os registros do
  // Bloco X
  cbConcomitante.Enabled := False;
  btnB_X.Enabled := False;
  btnB_Y.Enabled := True;

  with ACBrSPEDECF1.Bloco_X do
  begin
    // ABERTURA DO BLOCO X
    with RegistroX001New do
    begin
      IND_DAD := idComDados;

      // ATIVIDADES INCENTIVADAS – PJ EM GERAL
      for i := 0 to 0 do
      begin
        with RegistroX280New do
        begin
          IND_ATIV := '01';
          ATO_CONC := 'Outros';
          IND_PROJ := '99';
          VIG_INI := ACBrSPEDECF1.DT_INI;
          VIG_FIM := ACBrSPEDECF1.DT_FIN;
        end;
      end;

      // OPERAÇÕES COM O EXTERIOR – PESSOA NÃO VINCULADA/NÃO INTERPOSTA/PAÍS SEM TRIBUTAÇÃO FAVORECIDA
      for i := 0 to 0 do
      begin
        with RegistroX292New do
        begin
          CODIGO := '000001';
          DESCRICAO := 'DESCRICAO';
          VALOR := 200;
        end;
      end;

      // IDENTIFICAÇÃO DA PARTICIPAÇÃO NO EXTERIOR
      for i := 0 to 0 do
      begin
        with RegistroX340New do
        begin
          RAZ_SOCIAL := 'RAZ_SOCIAL';
          NIF := '0000';
          IND_CONTROLE := 1;
          PAIS := 105;
          IND_ISEN_PETR := 'N';
          IND_CONSOL := 'N';
          MOT_NAO_CONSOL := 1;

          // PARTICIPAÇÕES NO EXTERIOR – RESULTADO DO PERÍODO DE APURAÇÃO
          with RegistroX350New do
          begin
            REC_LIQ := 100;
            CUSTOS := 0;
            LUC_BRUTO := 100;
            REC_AUFERIDAS := 100;
            REC_OUTRAS_OPER := 0;
            DESP_BRASIL := 100;
            DESP_OPER := 100;
            LUC_OPER := 100;
            REC_PARTIC := 100;
            REC_OUTRAS := 0;
            DESP_OUTRAS := 0;
            LUC_LIQ_ANT_IR := 100;
            LUC_ARB_ANT_IMP := 100;
            IMP_DEV := 0;
            LUC_LIQ := 100;
          end;
        end;
      end;

      // INFORMAÇÕES GERAIS SOBRE PREÇOS DE TRANSFERÊNCIA
      for i := 0 to 0 do
      begin
        with RegistroX390New do
        begin
          CODIGO := '000001';
          DESCRICAO := 'DESCRICAO';
          VALOR := 200;
        end;
      end;

      // COMÉRCIO ELETRÔNICO E TECNOLOGIA DA INFORMAÇÃO – INFORMAÇÕES DAS VENDAS
      for i := 0 to 0 do
      begin
        with RegistroX400New do
        begin
          CODIGO := '000001';
          DESCRICAO := 'DESCRICAO';
          VALOR := 200;
        end;
      end;

      // COMÉRCIO ELETRÔNICO – INFORMAÇÃO DE HOMEPAGE/SERVIDOR
      for i := 0 to 0 do
      begin
        with RegistroX410New do
        begin
          PAIS := 105;
          IND_HOME_DISP := 'S';
          IND_SERV_DISP := 'S';
        end;
      end;

      // ROYALTIES RECEBIDOS OU PAGOS A BENEFICIÁRIOS DO BRASIL E DO EXTERIOR
      for i := 0 to 0 do
      begin
        with RegistroX420New do
        begin
          TIP_ROY := 'R';
          PAIS := 105;
          VL_EXPL_DIR_SW := 100;
          VL_EXPL_DIR_AUT := 100;
          VL_EXPL_MARCA := 100;
          VL_EXPL_PAT := 100;
          VL_EXPL_KNOW := 100;
          VL_EXPL_FRANQ := 100;
          VL_EXPL_INT := 100;
        end;
      end;

      // RENDIMENTOS RELATIVOS A SERVIÇOS, JUROS E DIVIDENDOS RECEBIDOS DO BRASIL E DO EXTERIOR
      for i := 0 to 0 do
      begin
        with RegistroX430New do
        begin
          PAIS := 105;
          VL_SERV_ASSIST := 100;
          VL_SERV_SEM_ASSIST := 100;
          VL_SERV_SEM_ASSIST_EXT := 100;
          VL_JURO := 1;
          VL_DEMAIS_JUROS := 1;
          VL_DIVID := 100;
        end;
      end;

      // PAGAMENTOS OU REMESSAS A TÍTULO DE SERVIÇOS, JUROS E DIVIDENDOS A BENEFICIÁRIOS DO BRASIL E DO EXTERIOR
      for i := 0 to 0 do
      begin
        with RegistroX450New do
        begin
          PAIS := 105;

          // PAGAMENTOS OU REMESSAS A TÍTULO DE SERVIÇOS, JUROS E DIVIDENDOS A BENEFICIÁRIOS DO BRASIL E DO EXTERIOR – DEMAIS INFORMAÇÕES
          for j := 0 to 0 do
          begin
            with RegistroX451New do
            begin
              CODIGO := '000001';
              DESCRICAO := 'DESCRICAO';
              VALOR := 200;
            end;
          end;
        end;
      end;

      // INOVAÇÃO TECNOLÓGICA E DESENVOLVIMENTO TECNOLÓGICO
      for i := 0 to 0 do
      begin
        with RegistroX460New do
        begin
          CODIGO := '000001';
          DESCRICAO := 'DESCRICAO';
          VALOR := 200;
        end;
      end;

      // CAPACITAÇÃO DE INFORMÁTICA E INCLUSÃO DIGITAL
      for i := 0 to 0 do
      begin
        with RegistroX470New do
        begin
          CODIGO := '000001';
          DESCRICAO := 'DESCRICAO';
          VALOR := 200;
        end;
      end;

      // BENEFÍCIOS FISCAIS – PARTE I
      for i := 0 to 0 do
      begin
        with RegistroX480New do
        begin
          CODIGO := '000001';
          DESCRICAO := 'DESCRICAO';
          VALOR := 200;
        end;
      end;

      // PÓLO INDUSTRIAL DE MANAUS E AMAZÔNIA OCIDENTAL
      for i := 0 to 0 do
      begin
        with RegistroX490New do
        begin
          CODIGO := '000001';
          DESCRICAO := 'DESCRICAO';
          VALOR := 200;
        end;
      end;

      // ZONAS DE PROCESSAMENTO DE EXPORTAÇÃO (ZPE)
      for i := 0 to 0 do
      begin
        with RegistroX500New do
        begin
          CODIGO := '000001';
          DESCRICAO := 'DESCRICAO';
          VALOR := 200;
        end;
      end;

      // ÁREAS DE LIVRE COMÉRCIO (ALC)
      for i := 0 to 0 do
      begin
        with RegistroX510New do
        begin
          CODIGO := '000001';
          DESCRICAO := 'DESCRICAO';
          VALOR := 200;
        end;
      end;
    end;
  end;

  if cbConcomitante.Checked then
  begin
    ACBrSPEDECF1.WriteBloco_X;
    LoadToMemo;
  end;
end;

procedure TFrmSPEDECF.btnB_JClick(Sender: TObject);
begin
  // Alimenta o componente com informações para gerar todos os registros do
  // Bloco J.
  cbConcomitante.Enabled := False;
  btnB_J.Enabled := False;
  btnB_K.Enabled := True;

  with ACBrSPEDECF1.Bloco_J do
  begin
    // ABERTURA DO BLOCO J
    with RegistroJ001New do
    begin
      IND_DAD := idComDados;

      // PLANO DE CONTAS DO CONTRIBUINTE
      with RegistroJ050New do
      begin
        DT_ALT := ACBrSPEDECF1.DT_FIN;
        COD_NAT := '01';
        IND_CTA := 'A';
        NIVEL := '1';
        COD_CTA := '00.000.000';
        COD_CTA_SUP := '';
        CTA := 'NOME CONTA';

        // PLANO DE CONTAS REFERENCIAL
        with RegistroJ051New do
        begin
          COD_CTA_REF := '001';
        end;
      end;
    end;
  end;

  if cbConcomitante.Checked then
  begin
    ACBrSPEDECF1.WriteBloco_J;
    LoadToMemo;
  end;
end;

procedure TFrmSPEDECF.btnB_KClick(Sender: TObject);
var
  i: integer;
  trimestre: Integer;
  data: TDate;
begin
  // Alimenta o componente com informações para gerar todos os registros do
  // Bloco K.
  cbConcomitante.Enabled := False;
  btnB_K.Enabled := False;
  btnB_L.Enabled := True;

  with ACBrSPEDECF1.Bloco_K do
  begin
    // ABERTURA DO BLOCO K
    with RegistroK001New do
    begin
      IND_DAD := idComDados;

      // IDENTIFICAÇÃO DO PERÍODO
      trimestre := 1;
      data := 0;

      case MonthOf(ACBrSPEDECF1.DT_INI) of
        1..3: trimestre := 1;
        4..6: trimestre := 2;
        7..9: trimestre := 3;
        10..12: trimestre := 4;
      end;

      while (trimestre <= 4) do
      begin
        with RegistroK030New do
        begin
          if (data = 0) then
            DT_INI := ACBrSPEDECF1.DT_INI
          else
            DT_INI := data;

          case trimestre of
            1: data := EncodeDate(YearOf(ACBrSPEDECF1.DT_INI), 3, 31);
            2: data := EncodeDate(YearOf(ACBrSPEDECF1.DT_INI), 6, 30);
            3: data := EncodeDate(YearOf(ACBrSPEDECF1.DT_INI), 9, 30);
            4: data := EncodeDate(YearOf(ACBrSPEDECF1.DT_INI), 12, 31);
          end;

          DT_FIN := EndOfTheMonth(data);
          PER_APUR := 'T0' + IntToStr(trimestre);

          // DETALHES DOS SALDOS CONTÁBEIS (DEPOIS DO ENCERRAMENTO DO RESULTADO DO PERÍODO)
          for i := 0 to 0 do
          begin
            with RegistroK155New do
            begin
              COD_CTA := '00.000.000';
              VL_SLD_INI := 100;
              IND_VL_SLD_INI := 'C';
              VL_DEB := 0;
              VL_CRED := 100;
              VL_SLD_FIN := 200;
              IND_VL_SLD_FIN := 'C'
            end;
          end;

          // SALDOS FINAIS DAS CONTA CONTÁBEIS DE RESULTADO ANTES DO ENCERRAMENTO
          for i := 0 to 0 do
          begin
            with RegistroK355New do
            begin
              COD_CTA := '00.000.000';
              VL_SLD_FIN := 200;
              IND_VL_SLD_FIN := 'C';
            end;
          end;
        end;

        data := data + 1;
        Inc(trimestre);
      end;
    end;
  end;

  if cbConcomitante.Checked then
  begin
    ACBrSPEDECF1.WriteBloco_K;
    LoadToMemo;
  end;
end;

procedure TFrmSPEDECF.btnB_YClick(Sender: TObject);
var
  i: integer;
  j: integer;
  trimestre: Integer;
  data: TDate;
begin
  // Alimenta o componente com informações para gerar todos os registros do
  // Bloco Y.
  cbConcomitante.Enabled := False;
  btnB_Y.Enabled := False;
  btnB_9.Enabled := True;

  with ACBrSPEDECF1.Bloco_Y do
  begin
    // ABERTURA DO BLOCO Y
    with RegistroY001.Create do
    begin
      IND_DAD := idComDados;

      // IDENTIFICAÇÃO E REMUNERAÇÃO DE SÓCIOS, TITULARES, DIRIGENTES E CONSELHEIROS
      for i := 0 to 0 do
      begin
        with RegistroY600New do
        begin
          DT_ALT_SOC := ACBrSPEDECF1.DT_INI;
          DT_FIM_SOC := 0;
          PAIS := 105;
          CPF_CNPJ := '12345678000195';
          IND_QUALIF_SOCIO := 'PJ';
          NOM_EMP := 'NOM_EMP';
          QUALIF := '01';
          PERC_CAP_TOT := 100;
          PERC_CAP_VOT := 100;
          CPF_REP_LEG := '98765432100';
          QUALIF_REP_LEG := qrlNenhum;
          VL_REM_TRAB := 100;
          VL_LUC_DIV := 100;
          VL_JUR_CAP := 100;
          VL_DEM_REND := 100;
          VL_IR_RET := 100;
        end;
      end;

      // IDENTIFICAÇÃO E RENDIMENTOS DE DIRIGENTES E CONSELHEIROS – IMUNES OU ISENTAS
      for i := 0 to 0 do
      begin
        with RegistroY612New do
        begin
          CPF := '98765432100';
          NOME := 'NOME';
          QUALIF := 16;
          VL_REM_TRAB := 100;
          VL_DEM_REND := 100;
          VL_IR_RET := 100;
        end;
      end;

      // PARTICIPAÇÕES AVALIADAS PELO MÉTODO DE EQUIVALÊNCIA PATRIMONIAL
      for i := 0 to 0 do
      begin
        with RegistroY620New do
        begin
          DT_EVENTO := ACBrSPEDECF1.DT_INI;
          IND_RELAC := 1;
          PAIS := 105;
          CNPJ := '12345678000195';
          NOM_EMP := 'NOM_EMP';
          VALOR_REAIS := 100;
          VALOR_ESTR := 100;
          PERC_CAP_TOT := 100;
          PERC_CAP_VOT := 100;
          RES_EQ_PAT := 100;
          DATA_AQUIS := ACBrSPEDECF1.DT_INI;
          IND_PROC_CART := 'N';
          IND_PROC_RFB := 'N';
        end;
      end;

      // FUNDOS/CLUBES DE INVESTIMENTO
      for i := 0 to 0 do
      begin
        with RegistroY630New do
        begin
          CNPJ := '12345678000195';
          QTE_QUOT := 1;
          QTE_QUOTA := 1;
          PATR_FIN_PER := 100;
          DAT_ABERT := ACBrSPEDECF1.DT_INI;
          DAT_ENCER := 0;
        end;
      end;

      // PARTICIPAÇÕES EM CONSÓRCIOS DE EMPRESAS
      for i := 0 to 0 do
      begin
        with RegistroY640New do
        begin
          CNPJ := '12345678000195';
          COND_DECL := 1;
          VL_CONS := 100;
          CNPJ_LID := '12345678000195';
          VL_DECL := 100;

          // PARTICIPANTES DO CONSÓRCIO
          for j := 0 to 0 do
          begin
            with RegistroY650New do
            begin
              CNPJ := '12345678000195';
              VL_PART := 100;
            end;
          end;
        end;
      end;

      // DADOS DE SUCESSORAS
      for i := 0 to 0 do
      begin
        with RegistroY660New do
        begin
          CNPJ := '12345678000195';
          NOM_EMP := 'NOM_EMP';
          PERC_PAT_LIQ := 100;
        end;
      end;

      // OUTRAS INFORMAÇÕES (LUCRO PRESUMIDO OU LUCRO ARBITRADO)
      with RegistroY672New do
      begin
        VL_CAPITAL_ANT := 100;
        VL_CAPITAL := 100;
        VL_ESTOQUE_ANT := 100;
        VL_ESTOQUES := 100;
        VL_CAIXA_ANT := 100;
        VL_CAIXA := 100;
        VL_APLIC_FIN_ANT := 100;
        VL_APLIC_FIN := 100;
        VL_CTA_REC_ANT := 100;
        VL_CTA_REC := 100;
        VL_CTA_PAG_ANT := 100;
        VL_CTA_PAG := 100;
        VL_COMPRA_MERC := 100;
        VL_COMPRA_ATIVO := 100;
        VL_RECEITAS := 100;
        TOT_ATIVO := 100;
        IND_AVAL_ESTOQ := '1';
      end;

      // MÊS DAS INFORMAÇÕES DE OPTANTES PELO REFIS (LUCRO REAL, PRESUMIDO E ARBITRADO)
      for i := 0 to 0 do
      begin
        with RegistroY680New do
        begin
          MES := '01';

          // INFORMAÇÕES DE OPTANTES PELO REFIS (LUCRO REAL, PRESUMIDO E ARBITRADO)
          for J := 0 to 0 do
          begin
            with RegistroY681New do
            begin
              CODIGO := '000001';
              DESCRICAO := 'DESCRICAO';
              //VALOR := null;
            end;
          end;
        end;
      end;

      // INFORMAÇÕES DE OPTANTES PELO REFIS (IMUNES OU ISENTAS)
      for i := 0 to 0 do
      begin
        with RegistroY682New do
        begin
          MES := 1;
        end;
      end;

      // OUTRAS INFORMAÇÕES
      for i := 0 to 0 do
      begin
        with RegistroY800New do
        begin
          TIPO_DOC := '003';
          DESC_RTF := 'DESCRICAO';
          HASH_RTF := '';
          ARQ_RTF := '{\rtf1\ansi\ansicpg1252\uc1...';
        end;
      end;
   end;
  end;

  if cbConcomitante.Checked then
  begin
    ACBrSPEDECF1.WriteBloco_Y;
    LoadToMemo;
  end;
end;

procedure TFrmSPEDECF.edtFileChange(Sender: TObject);
begin
  ACBrSPEDECF1.Arquivo := edtFile.Text;
end;

procedure TFrmSPEDECF.FormCreate(Sender: TObject);
begin
  DtRef.Date := EncodeDate(YearOf(IncYear(now, -1)),1,1);
end;

procedure TFrmSPEDECF.LoadToMemo;
begin
  memoTXT.Lines.Clear;
  if FileExists(ACBrSPEDECF1.Path + ACBrSPEDECF1.Arquivo) then
    memoTXT.Lines.LoadFromFile(ACBrSPEDECF1.Path + ACBrSPEDECF1.Arquivo);
end;

procedure TFrmSPEDECF.cbConcomitanteClick(Sender: TObject);
begin
  btnTXT.Enabled := not cbConcomitante.Checked;
  btnError.Enabled := btnTXT.Enabled;

  if not cbConcomitante.Checked then
  begin
    btnB_0.Enabled := True;
    btnB_J.Enabled := False;
    btnB_K.Enabled := False;
    btnB_L.Enabled := False;
    btnB_M.Enabled := False;
    btnB_P.Enabled := False;
    btnB_Q.Enabled := False;
    btnB_X.Enabled := False;
    btnB_Y.Enabled := False;
  end;
end;

end.

