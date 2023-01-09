{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{																			   }
{ Colaboradores nesse arquivo: Isaque Pinheiro                                 }
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

{******************************************************************************
|* Historico
|*
|* 10/04/2009: Isaque Pinheiro
|*  - Criação e distribuição da Primeira Versao
|* 21/02/2010: EMSoft Sistemas Ltda - RJ
|*  - Criação deste Demo
*******************************************************************************}

unit uFormPrincipal;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ACBrPAF, Math, ACBrEAD, ExtCtrls, ACBrPAFRegistros,
  ComCtrls, ACBrBase, ACBrPAF_W;

type
  TForm6 = class(TForm)
    GroupBox1: TGroupBox;
    ACBrPAF: TACBrPAF;
    Label1: TLabel;
    edtUF: TEdit;
    Label2: TLabel;
    edtCNPJ: TEdit;
    Label3: TLabel;
    edtIE: TEdit;
    edtIM: TEdit;
    Label4: TLabel;
    edtRAZAO: TEdit;
    Label5: TLabel;
    btnN: TButton;
    ACBrEAD: TACBrEAD;
    Image1: TImage;
    btnTITP: TButton;
    btnRegistrosPAF: TButton;
    pc1: TPageControl;
    ts1: TTabSheet;
    logErros: TMemo;
    ts2: TTabSheet;
    mmArquivoGerado: TMemo;
    btnZ: TButton;
    cbEAD: TCheckBox;
    Button1: TButton;
    btnRegistrosPAFNFCe: TButton;
    Button3: TButton;
    Button2: TButton;
    Button4: TButton;
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PreencherHeader(Header: TRegistroX1);
    function GerarDados(Tipo: Char; Tam: integer): Variant;
    procedure ACBrPAFMsnError(const MsnError: String);
    procedure ACBrPAFPAFCalcEAD(Arquivo: String);
    procedure btnNClick(Sender: TObject);
    procedure btnTITPClick(Sender: TObject);
    procedure btnRegistrosPAFClick(Sender: TObject);
    procedure btnRegistrosPAFNFCeClick(Sender: TObject);
    procedure btnZClick(Sender: TObject);
    procedure cbEADClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
    function QualquerNumero: Integer;
    function QualquerChar: Char;
  public
    { Public declarations }
  end;

var
  Form6: TForm6;

implementation

uses
  ACBrPAF_D, ACBrPAF_E, ACBrPAF_P, ACBrPAF_V, ACBrPAF_A,
  ACBrPAF_R, ACBrPAF_T, ACBrPaf_H, ACBrPaf_Z, ACBrPAF_S, ACBrPAF_J;

const
     NUM_FAB      = 'NUMFAB78901234567890';
     MF_ADICIONAL = '';
     TIPO_ECF     = 'ECF-IF';
     MARCA_ECF    = 'ACBr';
     MODELO_ECF   = 'PAF';

{$R *.dfm}

procedure TForm6.FormShow(Sender: TObject);
begin
     // os relatorios PAF estes parâmetros não são necessários
//     ACBrPAF.CurMascara :='';
//     ACBrPAF.Delimitador:='';

  edtUF.Text:='SP';
  edtRAZAO.Text:='DEMO PAF';
  edtCNPJ.Text:=GerarDados('I',14);
  edtIE.Text:=GerarDados('I',14);
  edtIM.Text:=GerarDados('I',14);
end;

procedure TForm6.Button2Click(Sender: TObject);
var
  i: Integer;
  LRegistroW4: TRegistroW4;
begin
  // Sempre altere o layout antes de preencher os registros. Isso porque
  // ao alterar o layout, todos registros já lançados são apagados automaticamente.
  ACBrPAF.Layout := lpPAFNFCe;

  //W1
  ACBrPAF.PAF_W.RegistroW1.CNPJ             := edtCNPJ.Text;
  ACBrPAF.PAF_W.RegistroW1.IE               := edtIE.Text;
  ACBrPAF.PAF_W.RegistroW1.IM               := edtIM.Text;
  ACBrPAF.PAF_W.RegistroW1.RAZAOSOCIAL      := edtRAZAO.Text;

  ACBrPAF.PAF_W.RegistroW2.CNPJ             := edtCNPJ.Text;
  ACBrPAF.PAF_W.RegistroW2.IE               := edtIE.Text;
  ACBrPAF.PAF_W.RegistroW2.IM               := edtIM.Text;
  ACBrPAF.PAF_W.RegistroW2.RAZAOSOCIAL      := edtRAZAO.Text;


  ACBrPAF.PAF_W.RegistroW3.NOME:='seu software';
  ACBrPAF.PAF_W.RegistroW3.VERSAO:='0.0.0';

  for i:=  0 to 5 do
  begin
    LRegistroW4:= ACBrPAF.PAF_W.RegistroW4.New;
    LRegistroW4.ORIGEMDARE := 'A';
    LRegistroW4.STATUSDARE := 'F';
    LRegistroW4.CRE := '123';
    LRegistroW4.DAV := '123';
    LRegistroW4.PREVENDA := '123';
    LRegistroW4.VALORTOTALDARE := 4.50;
  end;

  ACBrPAF.SaveToFile_RegistrosPAF('RegistrosPAFNFCeW.txt');

  if FileExists('RegistrosPAFNFCeW.txt') then
  begin
    mmArquivoGerado.Lines.LoadFromFile('RegistrosPAFNFCeW.txt');
    pc1.ActivePageIndex:= 1;
  end;
end;

procedure TForm6.Button3Click(Sender: TObject);
var
  i: Integer;
  LRegistroZ4: TRegistroZ4;
begin
  // Sempre altere o layout antes de preencher os registros. Isso porque
  // ao alterar o layout, todos registros já lançados são apagados automaticamente.
  ACBrPAF.Layout := lpPAFNFCe;

  //W1
  ACBrPAF.PAF_Z.RegistroZ1.CNPJ             := edtCNPJ.Text;
  ACBrPAF.PAF_Z.RegistroZ1.IE               := edtIE.Text;
  ACBrPAF.PAF_Z.RegistroZ1.IM               := edtIM.Text;
  ACBrPAF.PAF_Z.RegistroZ1.RAZAOSOCIAL      := edtRAZAO.Text;

  ACBrPAF.PAF_Z.RegistroZ2.CNPJ             := edtCNPJ.Text;
  ACBrPAF.PAF_Z.RegistroZ2.IE               := edtIE.Text;
  ACBrPAF.PAF_Z.RegistroZ2.IM               := edtIM.Text;
  ACBrPAF.PAF_Z.RegistroZ2.RAZAOSOCIAL      := edtRAZAO.Text;

  ACBrPAF.PAF_Z.RegistroZ3.NOME:= 'seu softwar';
  ACBrPAF.PAF_Z.RegistroZ3.VERSAO:=  '0.0.0';

  for i := 0 to 5 do
  begin
    LRegistroZ4 := ACBrPAF.PAF_Z.RegistroZ4.New;

    LRegistroZ4.CPF_CNPJ:= '000000000';
    LRegistroZ4.VL_TOTAL_MENSAL:= i;
    LRegistroZ4.DATA_INI:= now;
    LRegistroZ4.DATA_FIM:= now;
  end;


  ACBrPAF.SaveToFile_Z('RegistrosPAFNFCeZ.txt');

  if FileExists('RegistrosPAFNFCeZ.txt') then
  begin
    mmArquivoGerado.Lines.LoadFromFile('RegistrosPAFNFCeZ.txt');
    pc1.ActivePageIndex:= 1;
  end;
end;

function TForm6.GerarDados(Tipo: Char; Tam: integer): Variant;
var
     i: integer;
     sTmp: String;
     R: Variant;
begin
     if Tipo='S' then begin
        sTmp:='';
        for I := 1 to Tam do sTmp:=sTmp+QualquerChar;
        R:=sTmp;
     end;
     if Tipo='I' then begin
        sTmp:='';
        for I := 1 to Tam do sTmp:=sTmp+IntToStr(QualquerNumero);
        R:=StrToInt64(sTmp);
     end;
     Result:=R;
end;

function TForm6.QualquerNumero: Integer;
begin
     Result:=Random(9); // 0..9
end;

function TForm6.QualquerChar: Char;
begin
     Result:=Chr(RandomRange(65,90)); // A..Z
end;

procedure TForm6.PreencherHeader(Header: TRegistroX1);
begin
     // o header dos relatórios PAF a maioria são todos iguais
     Header.UF         :=edtUF.Text;
     Header.CNPJ       :=edtCNPJ.Text;
     Header.IE         :=edtIE.Text;
     Header.IM         :=edtIM.Text;
     Header.RAZAOSOCIAL:=edtRAZAO.Text;
end;

procedure TForm6.btnNClick(Sender: TObject);
begin
  // registro P1
  PreencherHeader(ACBrPAF.PAF_N.RegistroN1); // preencher header do arquivo

  // registro P2
  ACBrPAF.PAF_N.RegistroN2.LAUDO  := 'LAU1234567';
  ACBrPAF.PAF_N.RegistroN2.NOME   := 'NOME APLICATIVO NO LAUDO';
  ACBrPAF.PAF_N.RegistroN2.VERSAO := '1000';

  with ACBrPAF.PAF_N.RegistroN3.New do
  begin
    NOME_ARQUIVO := ExtractFileName(ParamStr(0));
    MD5          := ACBrEAD.MD5FromFile(ParamStr(0));
  end;
  ACBrPAF.SaveToFile_N('PAF_N.TXT');
end;

procedure TForm6.btnTITPClick(Sender: TObject);
var
  iProduto: Integer;
  iInsumo: Integer;
begin
  // Tabela de indice tecnico de produção

  ACBrPAF.PAF_TITP.Titulo   := 'Tabela de indice tecnico de produção';
  ACBrPAF.PAF_TITP.DataHora := NOW;

  for iProduto := 0 to 10 - 1 do
  begin
    with ACBrPAF.PAF_TITP.Mercadorias.New do
    begin
      Codigo      := IntToStr(iProduto);
      Ean         := StringOfChar(Codigo[1], 13);
      Descricao   := Format('Descricao do produto %d', [iProduto]);
      Unidade     := 'UN';
      CST         := '000';
      Aliquota    := 7.00;
      VlrUnitario := 1.23;
      Quantidade  := 10.00;

      for iInsumo := 0 to 3 - 1 do
      begin
        with Insumos.New do
        begin
          Codigo      := IntToStr(iInsumo) + '/' + IntToStr(iProduto);
          Ean         := '';
          Descricao   := Format('Descricao do insumo %d do produto %d', [iInsumo, iProduto]);
          Unidade     := 'UN';
          CST         := '000';
          Aliquota    := 7.00;
          VlrUnitario := 0.23;
          Quantidade  := 2.00;
        end;
      end;
    end;
  end;

  ACBrPAF.SaveToFile_TITP('PAF_TITP.TXT');
end;

procedure TForm6.btnZClick(Sender: TObject);
var
//  Z4: TRegistroZ4;
  i: integer;
begin
  // registro Z1
  PreencherHeader(ACBrPAF.PAF_Z.RegistroZ1); // preencher header do arquivo
  PreencherHeader(ACBrPAF.PAF_Z.RegistroZ2); // preencher header do arquivo
  with ACBrPAF.PAF_Z do
  begin
    with RegistroZ3 do
      begin
        LAUDO  := '11111';
        NOME   := 'NOME';
        VERSAO := '1.00';
      end;
    // registro Z4
    RegistroZ4.Clear;
    for I := 1 to 15 do
    begin
      with RegistroZ4.New do
        begin
          CPF_CNPJ := '99.999.999/9999-11';
          VL_TOTAL := 10 * I;
          DATA_INI := Now;
          DATA_FIM := Now;
        end;
    end;
  end;
  ACBrPAF.SaveToFile_Z('PAF_Z.TXT');
end;

procedure TForm6.Button1Click(Sender: TObject);
var
//  V4: TRegistroV4;
  i: integer;
begin
  // registro V1
  PreencherHeader(ACBrPAF.PAF_V.RegistroV1); // preencher header do arquivo
  PreencherHeader(ACBrPAF.PAF_V.RegistroV2); // preencher header do arquivo
  with ACBrPAF.PAF_V do
  begin
    with RegistroV3 do
      begin
        LAUDO  := '11111';
        NOME   := 'NOME';
        VERSAO := '1.00';
      end;
    // registro Z4
    RegistroV4.Clear;
    for I := 1 to 5 do
    begin
      with RegistroV4.New do
        begin
          NUMUMEROFABRICACAO := '99.999.999/9999-1'+ IntToStr(I);
          MARCAECF := 'MARCA'+IntToStr(I);
          MFADICIONAL := 'B';
          MODELOECF := 'MODELO'+IntToStr(I);
        end;
    end;
  end;
  ACBrPAF.SaveToFile_V('PAF_V.TXT');
end;

procedure TForm6.cbEADClick(Sender: TObject);
begin
ACBrPAF.AssinarArquivo := cbEAD.Checked;
end;

procedure TForm6.ACBrPAFMsnError(const MsnError: String);
begin
     logErros.Lines.Add(MsnError); // captura os erros encontrados pelo ACBrPAF
end;

procedure TForm6.ACBrPAFPAFCalcEAD(Arquivo: String);
begin
//
end;

procedure TForm6.btnRegistrosPAFClick(Sender: TObject);
var
  i, j: Integer;
begin
  ACBrPAF.Layout := lpPAFNFCe;

  //U1
  with ACBrPAF.PAF_U.RegistroU1 do
    begin
      CNPJ:= edtCNPJ.Text;
      IE  := edtIE.Text;
      IM  := edtIM.Text;
      RAZAOSOCIAL:= edtRAZAO.Text;

      InclusaoExclusao:= True;
    end;

  //A2
  ACBrPAF.PAF_A.RegistroA2.Clear;
  for I := 1 to 9 do
    begin
      with ACBrPAF.PAF_A.RegistroA2.New do
        begin
          DT := Date;
          case ( i mod 3) of
            1: MEIO_PGTO:= 'Cheque';
            2: MEIO_PGTO:= 'Cartao';
          else
            MEIO_PGTO:= 'Dinheiro';
          end;
          TIPO_DOC := '1'; //1-CupomFiscal, 2-CNF, 3-Nota Fiscal
          VL := (1.5 * i);

          RegistroValido:= True;
        end;
    end;

  //P2
  ACBrPAF.PAF_P.RegistroP2.Clear;
  for I := 1 to 5 do
    begin
      with ACBrPAF.PAF_P.RegistroP2.New do
        begin
          COD_MERC_SERV := GerarDados('I',14);
          DESC_MERC_SERV:= GerarDados('S',50);
          UN_MED        := GerarDados('S',2);
          IAT           := 'A';
          IPPT          := 'T';
          ST            := 'FF';
          ALIQ          := 0;
          VL_UNIT       := GerarDados('I',2);

          RegistroValido:= True;
        end;
    end;

  //E2
  ACBrPAF.PAF_E.RegistroE2.Clear;
  for I := 1 to 5 do
    begin
      with ACBrPAF.PAF_E.RegistroE2.New do
        begin
          COD_MERC :=GerarDados('I',14);
          DESC_MERC:=GerarDados('S',50);
          UN_MED   :=GerarDados('S',2);
          QTDE_EST :=GerarDados('I',3);

          RegistroValido:= True;
        end;
    end;

  //E3
  with ACBrPAF.PAF_E.RegistroE3 do
    begin
      NUM_FAB      := uFormPrincipal.NUM_FAB;
      MF_ADICIONAL := uFormPrincipal.MF_ADICIONAL;
      TIPO_ECF     := uFormPrincipal.TIPO_ECF;
      MARCA_ECF    := uFormPrincipal.MARCA_ECF;
      MODELO_ECF   := uFormPrincipal.MODELO_ECF;
      DT_EST       := Now;

      RegistroValido:= True;
    end;

  //D2 - DAV
  ACBrPAF.PAF_D.RegistroD2.Clear;
  for I := 1 to 5 do
  begin
    with ACBrPAF.PAF_D.RegistroD2.New do
    begin
      NUM_FAB      := uFormPrincipal.NUM_FAB;
      MF_ADICIONAL := uFormPrincipal.MF_ADICIONAL;
      TIPO_ECF     := uFormPrincipal.TIPO_ECF;
      MARCA_ECF    := uFormPrincipal.MARCA_ECF;
      MODELO_ECF   := uFormPrincipal.MODELO_ECF;
      COO          := IntToStr(I * QualquerNumero);
      NUM_DAV      := IntToStr(I * QualquerNumero);
      DT_DAV       := Date - QualquerNumero;
      TIT_DAV      := 'Pedido';
      VLT_DAV      := GerarDados('I', 2);
      COO_DFV      := '0';
      NUMERO_ECF   := '1';
      NOME_CLIENTE := 'NOME CLIENTE';
      CPF_CNPJ     := '12345678921';

      RegistroValido := True; // diz quando o registro foi modificado no banco

      //D3
      for j := 1 to 2 do
      begin
        with RegistroD3.New do
        begin
          DT_INCLUSAO   := DATE;
          NUM_ITEM      := i;
          COD_ITEM      := '10';
          DESC_ITEM     := 'descricao do item';
          QTDE_ITEM     := 10.00;
          UNI_ITEM      := 'UN';
          VL_UNIT       := 1.00;
          VL_DESCTO     := 0.00;
          VL_ACRES      := 0.00;
          VL_TOTAL      := 10.00;
          SIT_TRIB      := 'T'; // T, S, I, N, F
          ALIQ          := 7.00; // SOMENTE QUANDO T E S
          IND_CANC      := 'N';
          DEC_QTDE_ITEM := 2;
          DEC_VL_UNIT   := 2;

          RegistroValido := True;
        end;
      end;

      //D4 - Log alterações DAV
      for j := 1 to 2 do
      begin
        with RegistroD4.New do
        begin
          NUM_DAV       := IntToStr(I * QualquerNumero);
          DT_ALT        := Now;
          COD_ITEM      := '10';
          DESC_ITEM     := 'descricao do item';
          QTDE_ITEM     := 10.00;
          UNI_ITEM      := 'UN';
          VL_UNIT       := 1.00;
          VL_DESCTO     := 0.00;
          VL_ACRES      := 0.00;
          VL_TOTAL      := 10.00;
          SIT_TRIB      := 'T'; // T, S, I, N, F
          ALIQ          := 7.00; // SOMENTE QUANDO T E S
          IND_CANC      := 'N';
          DEC_QTDE_ITEM := 2;
          DEC_VL_UNIT   := 2;
          TIP_ALT       := 'I';

          RegistroValido := True;
        end;
      end;

    end;
  end;



  //F2
  ACBrPAF.PAF_F.RegistroF2.Clear;
  for I := 0 to 5 do
    begin
      With ACBrPAF.PAF_F.RegistroF2.New do
      begin
        CNPJ_EMP  := '';
        CNPJ_ORG  := '';
        COD_LOCAL := '01';
        ID_LINHA  := 'idlinha';
        DESC_LINHA:= 'descrição da linha';
        DT_PART   := Now;
        COD_VIAGEM:= '00';

        RegistroValido:= True;
      end;
    end;

  //F3
  ACBrPAF.PAF_F.RegistroF3.Clear;
  for I := 0 to 10 do
    begin
      With ACBrPAF.PAF_F.RegistroF3.New do
      begin
        NUM_FAB      := uFormPrincipal.NUM_FAB;
        MF_ADICIONAL := uFormPrincipal.MF_ADICIONAL;
        MODELO_ECF   := uFormPrincipal.MODELO_ECF;
        NUM_USU      := 1;
        CCF          := 310 + i;
        COO          := 459 + (i*2);
        COD_ORIG     := '01';
        COD_DEST     := '01';
        VL_DOC       := 57.91;
        ST           := 'S';
        COD_TSER     := '06';
        POLTRONA     := 17 + i;

        RegistroValido:= True;
      end;
    end;

  //F4
  ACBrPAF.PAF_F.RegistroF4.Clear;
  for I := 0 to 2 do
    begin
      with ACBrPAF.PAF_F.RegistroF4.New do
      begin
        COD_TSER  := '0' + IntToStr(i + 1);
        QTDE_TOTAL:= 3 + i;

        RegistroValido:= True;
      end;
    end;

  //M2
  ACBrPAF.PAF_M.RegistroM2.Clear;
  for I := 0 to 5 do
    begin
      with ACBrPAF.PAF_M.RegistroM2.New do
      begin
        CNPJ        := edtCNPJ.Text;
        IE          := edtIE.Text;
        IM          := edtIM.Text;
        NUM_FAB     := uFormPrincipal.NUM_FAB;
        MF_ADICIONAL:= uFormPrincipal.MF_ADICIONAL;
        TIPO_ECF    := uFormPrincipal.TIPO_ECF;
        MARCA_ECF   := uFormPrincipal.MARCA_ECF;
        MODELO_ECF  := uFormPrincipal.MODELO_ECF;
        NUM_USU     := 1;
        CCF         := 111 + i;
        COO         := 185 + i;
        DT_EMI      := Date;
        COD_MOD     := '01';
        COD_CAT     := '04';
        ID_LINHA    := 'idlinha';
        COD_ORIG    := '01';
        COD_DEST    := '01';
        COD_TSER    := '06';
        DT_VIA      := Date;
        TIP_VIA     := '00';
        POLTRONA    := 17+i;
        PLATAFORMA  := 'n1';
        COD_DESC    := '00';
        VL_TARIFA   := 3.20;
        ALIQ        := 17;
        VL_PEDAGIO  := 0;
        VL_TAXA     := 0;
        VL_TOTAL    := 3.2;
        FORM_PAG    := '01';
        VL_PAGO     := 3.2;
        NOME_PAS    := 'fulano de tal';
        NDOC_PAS    := '123.456.789-09';
        SAC         := '0800123456';
        AGENCIA     := 'Razão Social';
      end;
    end;

  //L2
  ACBrPAF.PAF_L.RegistroL2.Clear;
  for I := 0 to 5 do
    begin
      with ACBrPAF.PAF_L.RegistroL2.New do
      begin
        CNPJ        := edtCNPJ.Text;
        IE          := edtIE.Text;
        IM          := edtIM.Text;
        NUM_FAB     := uFormPrincipal.NUM_FAB;
        MF_ADICIONAL:= uFormPrincipal.MF_ADICIONAL;
        TIPO_ECF    := uFormPrincipal.TIPO_ECF;
        MARCA_ECF   := uFormPrincipal.MARCA_ECF;
        MODELO_ECF  := uFormPrincipal.MODELO_ECF;
        NUM_USU     := 1;
        COO         := 185 + i;
        GNF         := 0;
        GRG         := 0;
        DT_EMI      := Date;
        COD_MOD     := '01';
        COD_CAT     := '04';
        ID_LINHA    := 'idlinha';
        COD_ORIG    := '01';
        COD_DEST    := '01';
        COD_TSER    := '06';
        DT_VIA      := Date;
        TIP_VIA     := '00';
        POLTRONA    := 17+i;
        PLATAFORMA  := 'n1';
        COD_DESC    := '00';
        VL_TARIFA   := 3.20;
        VL_PEDAGIO  := 0;
        VL_TAXA     := 0;
        VL_TOTAL    := 3.2;
        FORM_PAG    := '01';
        VL_PAGO     := 3.2;
        NOME_PAS    := 'fulano de tal';
        NDOC_PAS    := '123.456.789-09';
        SAC         := '0800123456';
        AGENCIA     := 'Razão Social';
      end;
    end;

  //G2
  ACBrPAF.PAF_G.RegistroG2.Clear;
  for I:= 0 to 10 do
    begin
      with ACBrPAF.PAF_G.RegistroG2.New do
      begin
        CNPJ         := '';
        NUM_FAB      := uFormPrincipal.NUM_FAB;
        MF_ADICIONAL := uFormPrincipal.MF_ADICIONAL;
        TIPO_ECF     := uFormPrincipal.TIPO_ECF;
        MARCA_ECF    := uFormPrincipal.MARCA_ECF;
        MODELO_ECF   := uFormPrincipal.MODELO_ECF;
        DT           := Date;
        NUM_CAB      := i + 1;
        COO_INI      := 409 + i;
        COO_FIN      := 441 + i;
        CCF_INI      := 179 + i;
        CCF_FIN      := 201 + i;
        VL_2EIX_SIMPLES     := 11.27;
        VL_2EIX_SIMPLES_MOTO:= 11.27;
        VL_2EIX_DUPLA       := 11.27;
        VL_3EIX_SIMPLES     := 11.27;
        VL_3EIX_DUPLA       := 11.27;
        VL_4EIX_SIMPLES     := 11.27;
        VL_4EIX_DUPLA       := 11.27;
        VL_5EIX_DUPLA       := 11.27;
        VL_6EIX_DUPLA       := 11.27;
        VL_OUTROS           := 179.14;
        VL_TOTAL_DIA        := VL_2EIX_SIMPLES + VL_2EIX_SIMPLES_MOTO + VL_2EIX_DUPLA +
                               VL_3EIX_SIMPLES + VL_3EIX_DUPLA + VL_4EIX_SIMPLES +
                               VL_4EIX_DUPLA +  VL_5EIX_DUPLA + VL_6EIX_DUPLA + VL_OUTROS;
        QTDE_VEIC_ISENTO:= 3;
        LOCALIZACAO:= 'RODOVIA SC-370 KM 195 MUNICIPIO BRAÇO DO NORTE';
      end;
    end;

  // registro E2
  ACBrPAF.PAF_H.RegistroH2.Clear;
  for I := 1 to 15 do
  begin
    with ACBrPAF.PAF_H.RegistroH2.New do
    begin
      CNPJ_CRED_CARTAO := '99.999.999/9999-11';
      NUM_FAB          := uFormPrincipal.NUM_FAB;
      MF_ADICIONAL     := uFormPrincipal.MF_ADICIONAL;
      TIPO_ECF         := uFormPrincipal.TIPO_ECF;
      MARCA_ECF        := uFormPrincipal.MARCA_ECF;
      MODELO_ECF       := uFormPrincipal.MODELO_ECF;
      COO              := GerarDados('I', 6);
      CCF              := GerarDados('I', 6);
      VLR_TROCO        := GerarDados('I', 2);
      DT_TROCO         := DATE;
      CPF              := '111.111.111-99';
      TITULO           := GerarDados('S', 7);

      RegistroValido := True;
    end;
  end;

  //S2
  ACBrPAF.PAF_S.RegistroS2.Clear;
  for I := 1 to 5 do
  begin
    With ACBrPAF.PAF_S.RegistroS2.New  do
      begin
        CNPJ       := '';
        DT_ABER    := Now;
        SITU       := 'F';
        VL_TOT     := 12.5 * i;
        COO_CM     := IntToStr(i);
        NUM_FAB_CM := uFormPrincipal.NUM_FAB;
        COO        := IntToStr(i+15);
        NUM_FAB    := uFormPrincipal.NUM_FAB;

        //S3
        for j := 1 to 2 do
        begin
          with RegistroS3.New do
            begin
              COD_ITEM  := IntToStr(j);
              DESC_ITEM := 'descricao do item';
              QTDE_ITEM := 1;
              UNI_ITEM  := 'UN';
              VL_UNIT   := 2;
            end;
        end;
      end;
  end;

  // Registro R1 - Identificação do ECF, do Usuário, do PAF-ECF e da Empresa Desenvolvedora e Dados do Arquivo
  with ACBrPAF.PAF_R.RegistroR01.New do
  begin
    NUM_FAB     := uFormPrincipal.NUM_FAB;
    MF_ADICIONAL:= uFormPrincipal.MF_ADICIONAL;
    TIPO_ECF    := uFormPrincipal.TIPO_ECF;
    MARCA_ECF   := uFormPrincipal.MARCA_ECF;
    MODELO_ECF  := uFormPrincipal.MODELO_ECF;
    VERSAO_SB   := '010101';
    DT_INST_SB  := date;
    HR_INST_SB  := time;
    NUM_SEQ_ECF := 1;
    CNPJ        := edtCNPJ.Text;
    IE          := edtIE.Text;
    CNPJ_SH     := edtCNPJ.Text;
    IE_SH       := edtIE.Text;
    IM_SH       := edtIM.Text;
    NOME_SH     := edtRAZAO.Text;
    NOME_PAF    := 'MeuPAFECF';
    VER_PAF     := '0107';
    COD_MD5     := GerarDados('S', 32);
    DT_INI      := Date;
    DT_FIN      := Date;
    ER_PAF_ECF  := '0113';

    InclusaoExclusao := False;
    RegistroValido   := True;

    // Registro R02 - Relação de Reduções Z
    //        e R03 - Detalhe da Redução Z
    for I := 1 to 15 do
    begin
      with RegistroR02.New do
      begin
        NUM_USU     := 1;
        CRZ         := GerarDados('I', 3);
        COO         := GerarDados('I', 3);
        CRO         := GerarDados('I', 3);
        DT_MOV      := DATE;
        DT_EMI      := DATE;
        HR_EMI      := TIME;
        VL_VBD      := GerarDados('I', 3);
        PAR_ECF     := '';
        // Registro R03 - FILHO
        for J := 1 to 7 do
        begin
          with RegistroR03.New do
          begin
             TOT_PARCIAL := 'TOT ' + GerarDados('S', 2);
             VL_ACUM     := GerarDados('I', 2);
          end;
        end;
      end;
    end;

    // Registro R04 - Cupom Fiscal, Nota Fiscal de Venda a Consumidor ou Bilhete de Passagem
    //        e R05 - Detalhe do Cupom Fiscal, Nota Fiscal de Venda a Consumidor ou Bilhete de Passagem
    //        e R07 - Detalhe do Cupom Fiscal e do Documento Não Fiscal - Meio de Pagamento
    for I := 1 to 22 do
    begin
      with RegistroR04.New do
      begin
        NUM_USU     := 1;
        NUM_CONT    := 900 + I; //Esse daqui é o CCF!!
        COO         := 1000 + I;
        DT_INI      := date;
        SUB_DOCTO   := GerarDados('I', 2);
        SUB_DESCTO  := GerarDados('I', 2);
        TP_DESCTO   := 'V';
        SUB_ACRES   := 0;
        TP_ACRES    := 'V';
        VL_TOT      := GerarDados('I', 2);
        CANC        := 'N';
        VL_CA       := 0;
        ORDEM_DA    := 'D';
        NOME_CLI    := 'CLIENTE' + GerarDados('S', 43);
        CNPJ_CPF    := GerarDados('I',12);

        // Registro R05 - FILHO
        for J := 1 to RandomRange(1, 10) do
        begin
          with RegistroR05.New do
          begin
            NUM_ITEM     := J;
            COD_ITEM     := GerarDados('I', 14); //Código do produto ou Serviço
            DESC_ITEM    := 'DESCRICAO '+ GerarDados('S', 86) + ' FIM';
            QTDE_ITEM    := RandomRange(1, 5);
            UN_MED       := GerarDados('S', 2);
            VL_UNIT      := GerarDados('I', 2);
            DESCTO_ITEM  := 0;
            ACRES_ITEM   := 0;
            VL_TOT_ITEM  := VL_UNIT * QTDE_ITEM; //GerarDados('I', 2);
            COD_TOT_PARC := 'FF';
            IND_CANC     := 'N';
            QTDE_CANC    := 0;
            VL_CANC      := 0;
            VL_CANC_ACRES:= 0;
            IAT          := 'A';
            IPPT         := 'T';
            QTDE_DECIMAL := 2;
            VL_DECIMAL   := 2;
          end;
        end;

        // Registro R07 - FILHO
        for J := 1 to RandomRange(1, 3) do
        begin
          with RegistroR07.New do
          begin
            CCF         := NUM_CONT; //GerarDados('I',3); //Pega do R04
            MP          := 'PG_CUPOM ';// + GerarDados('S', 6);
            VL_PAGTO    := GerarDados('I', 2);
            IND_EST     := 'N';
            VL_EST      := 0;
          end;
        end;
      end;
    end;

    // Registro R06 - Demais documentos emitidos pelo ECF
    //        e R07 - Detalhe do Cupom Fiscal e do Documento Não Fiscal - Meio de Pagamento
    for I := 1 to 10 do
    begin
      with RegistroR06.New do
      begin
        NUM_USU     := 1;
        COO         := 300 + I; // GerarDados('I',3);
        GNF         := GerarDados('I', 3);
        GRG         := GerarDados('I', 3);
        CDC         := GerarDados('I', 3);
        DENOM       := GerarDados('S', 2);
        DT_FIN      := now;
        HR_FIN      := now;
        // Registro R07 - FILHO
        for J := 1 to 3 do
        begin
          with RegistroR07.New do
          begin
            MP          := 'PG_R06 ' + GerarDados('S', 7);
            VL_PAGTO    := GerarDados('I', 2);
            IND_EST     := 'N';
            VL_EST      := 0;
          end;
        end;
      end;
    end;
  end;

  ACBrPAF.SaveToFile_RegistrosPAF('RegistrosPAF.txt');

  if FileExists('RegistrosPAF.txt') then
  begin
    mmArquivoGerado.Lines.LoadFromFile('RegistrosPAF.txt');
    pc1.ActivePageIndex:= 1;
  end;
end;

procedure TForm6.btnRegistrosPAFNFCeClick(Sender: TObject);
var
  i, j: Integer;
  NovoRegistroA2: TRegistroA2;
  NovoRegistroD2: TRegistroD2;
  NovoRegistroD3: TRegistroD3;
  NovoRegistroD4: TRegistroD4;
  NovoRegistroE2: TRegistroE2;
  NovoRegistroP2: TRegistroP2;
  NovoRegistroS2: TRegistroS2;
  NovoRegistroS3: TRegistroS3;
  NovoRegistroJ1: TRegistroJ1;
//  NovoRegistroJ2: TRegistroJ2;
begin
  // Sempre altere o layout antes de preencher os registros. Isso porque
  // ao alterar o layout, todos registros já lançados são apagados automaticamente.
  ACBrPAF.Layout := lpPAFNFCe;

  //U1
  ACBrPAF.PAF_U.RegistroU1.CNPJ             := edtCNPJ.Text;
  ACBrPAF.PAF_U.RegistroU1.IE               := edtIE.Text;
  ACBrPAF.PAF_U.RegistroU1.IM               := edtIM.Text;
  ACBrPAF.PAF_U.RegistroU1.RAZAOSOCIAL      := edtRAZAO.Text;
  ACBrPAF.PAF_U.RegistroU1.InclusaoExclusao := True;

  //A2
  for I := 1 to 9 do
  begin
    NovoRegistroA2 := ACBrPAF.PAF_A.RegistroA2.New;

    NovoRegistroA2.DT             := Date + (i div 4);
    case ( i mod 4) of
      1: NovoRegistroA2.MEIO_PGTO := 'Dinheiro';
      2: NovoRegistroA2.MEIO_PGTO := 'Cartao';
      3: NovoRegistroA2.MEIO_PGTO := 'Cheque';
    else
      NovoRegistroA2.MEIO_PGTO    := 'Pix';
    end;
    NovoRegistroA2.TIPO_DOC       := '1'; // 1-NFC-e 2-NF-e 3-Operação não tributável, identificando o CPF ou CNPJ do cliente.
    NovoRegistroA2.VL             := (1.5 * i);
    NovoRegistroA2.CNPJ           := ''; // Só é preciso informar para o tipo de documento "3"
    NovoRegistroA2.NUMDOCUMENTO   := ''; // Só é preciso informar para o tipo de documento "3"

    NovoRegistroA2.RegistroValido := True;
  end;

  //P2
  ACBrPAF.PAF_P.RegistroP1.CNPJ:= ACBrPAF.PAF_U.RegistroU1.CNPJ;
  for I := 1 to 5 do
  begin
    NovoRegistroP2 := ACBrPAF.PAF_P.RegistroP2.New;

    NovoRegistroP2.COD_MERC_SERV  := GerarDados('I',14);
    NovoRegistroP2.DESC_MERC_SERV := GerarDados('S',50);
    NovoRegistroP2.UN_MED         := GerarDados('S',2);
    NovoRegistroP2.IAT            := 'A';
    NovoRegistroP2.IPPT           := 'T';
    NovoRegistroP2.ST             := 'FF';
    NovoRegistroP2.ALIQ           := 0;
    NovoRegistroP2.VL_UNIT        := GerarDados('I',2);

    NovoRegistroP2.RegistroValido := True;
  end;

  //E2
  for I := 1 to 5 do
  begin
    NovoRegistroE2 := ACBrPAF.PAF_E.RegistroE2.New;

    NovoRegistroE2.COD_MERC       := GerarDados('I', 14);
    NovoRegistroE2.CEST           := GerarDados('I', 7);
    NovoRegistroE2.NCM            := GerarDados('I', 8);
    NovoRegistroE2.DESC_MERC      := GerarDados('S', 50);
    NovoRegistroE2.UN_MED         := GerarDados('S', 2);
    NovoRegistroE2.QTDE_EST       := GerarDados('I', 3);
    NovoRegistroE2.DATAEMISSAO    := Date;
    NovoRegistroE2.DATAESTOQUE    := Date;

    NovoRegistroE2.RegistroValido := True;
  end;

  //D2 - DAV
  for I := 1 to 5 do
  begin
    NovoRegistroD2 := ACBrPAF.PAF_D.RegistroD2.New;

    NovoRegistroD2.NUM_DAV      := IntToStr(I * QualquerNumero);
    NovoRegistroD2.DT_DAV       := Date - QualquerNumero;
    NovoRegistroD2.TIT_DAV      := 'Pedido';
    NovoRegistroD2.VLT_DAV      := GerarDados('I', 2);
    NovoRegistroD2.NOME_CLIENTE := 'NOME CLIENTE';
    NovoRegistroD2.CPF_CNPJ     := '12345678921';

    NovoRegistroD2.RegistroValido := True; // diz quando o registro foi modificado no banco

    //D3
    for j := 1 to 2 do
    begin
      NovoRegistroD3 := NovoRegistroD2.RegistroD3.New;

      NovoRegistroD3.DT_INCLUSAO   := DATE;
      NovoRegistroD3.NUM_ITEM      := i;
      NovoRegistroD3.COD_ITEM      := '10';
      NovoRegistroD3.DESC_ITEM     := 'descricao do item';
      NovoRegistroD3.QTDE_ITEM     := 10.00;
      NovoRegistroD3.UNI_ITEM      := 'UN';
      NovoRegistroD3.VL_UNIT       := 1.00;
      NovoRegistroD3.VL_DESCTO     := 0.00;
      NovoRegistroD3.VL_ACRES      := 0.00;
      NovoRegistroD3.VL_TOTAL      := 10.00;
      NovoRegistroD3.SIT_TRIB      := 'T'; // T, S, I, N, F
      NovoRegistroD3.ALIQ          := 7.00; // SOMENTE QUANDO T E S
      NovoRegistroD3.IND_CANC      := 'N';
      NovoRegistroD3.DEC_QTDE_ITEM := 2;
      NovoRegistroD3.DEC_VL_UNIT   := 2;

      NovoRegistroD3.RegistroValido := True;
    end;

    //D4 - Log alterações DAV
    for j := 1 to 2 do
    begin
      NovoRegistroD4 := NovoRegistroD2.RegistroD4.New;

      NovoRegistroD4.NUM_DAV       := IntToStr(I * QualquerNumero);
      NovoRegistroD4.DT_ALT        := Now;
      NovoRegistroD4.COD_ITEM      := '10';
      NovoRegistroD4.DESC_ITEM     := 'descricao do item';
      NovoRegistroD4.QTDE_ITEM     := 10.00;
      NovoRegistroD4.UNI_ITEM      := 'UN';
      NovoRegistroD4.VL_UNIT       := 1.00;
      NovoRegistroD4.VL_DESCTO     := 0.00;
      NovoRegistroD4.VL_ACRES      := 0.00;
      NovoRegistroD4.VL_TOTAL      := 10.00;
      NovoRegistroD4.SIT_TRIB      := 'T'; // T, S, I, N, F
      NovoRegistroD4.ALIQ          := 7.00; // SOMENTE QUANDO T E S
      NovoRegistroD4.IND_CANC      := 'N';
      NovoRegistroD4.DEC_QTDE_ITEM := 2;
      NovoRegistroD4.DEC_VL_UNIT   := 2;
      NovoRegistroD4.TIP_ALT       := 'I';

      NovoRegistroD4.RegistroValido := True;
    end;

  end;

  //S2
  for I := 1 to 5 do
  begin
    NovoRegistroS2 := ACBrPAF.PAF_S.RegistroS2.New;

    NovoRegistroS2.CNPJ       := ACBrPAF.PAF_U.RegistroU1.CNPJ;
    NovoRegistroS2.DT_ABER    := Now;
    NovoRegistroS2.NUM_MESA   := IntToStr(i);
    NovoRegistroS2.VL_TOT     := 12.5 * i;
    //"Nº do Conferencia de Mesa" abaixo "COO_CM" - Deve ser informado apenas quando houver registro destes dados.
//    NovoRegistroS2.COO_CM     := IntToStr(i);

    //S3
    for j := 1 to 2 do
    begin
      NovoRegistroS3 := NovoRegistroS2.RegistroS3.New;

      NovoRegistroS3.COD_ITEM  := IntToStr(j);
      NovoRegistroS3.DESC_ITEM := 'descricao do item';
      NovoRegistroS3.QTDE_ITEM := 1;
      NovoRegistroS3.UNI_ITEM  := 'UN';
      NovoRegistroS3.VL_UNIT   := 2;
    end;
  end;

  //J1
  for I := 1 to 3 do
  begin
    NovoRegistroJ1 := ACBrPAF.PAF_J.RegistroJ1.New;

    NovoRegistroJ1.CNPJ                  := ACBrPAF.PAF_U.RegistroU1.CNPJ;
    NovoRegistroJ1.DATA_EMISSAO          := Now;
    NovoRegistroJ1.SUBTOTAL              := 30;
    NovoRegistroJ1.DESC_SUBTOTAL         := 0;
    NovoRegistroJ1.INDICADOR_DESC        := 'V';
    NovoRegistroJ1.ACRES_SUBTOTAL        := 0;
    NovoRegistroJ1.INDICADOR_ACRES       := 'V';
    NovoRegistroJ1.VALOR_LIQUIDO         := 30;
    NovoRegistroJ1.TIPOEMISSAO           := '1';
    NovoRegistroJ1.CHAVE_NF              := GerarDados('S', 44);
    NovoRegistroJ1.NUMERO_NOTA           := GerarDados('S', 10);
    NovoRegistroJ1.SERIE_NOTA            := '001';
    NovoRegistroJ1.CPFCNPJ_CLIENTE       := GerarDados('S', 14);

    //J2
    //Registros J2 são gerados apenas para NFC-e que forem emitidas em Contingência. Não é o caso desse exemplo.
//    for j := 1 to 2 do
//    begin
//      NovoRegistroJ2 := NovoRegistroJ1.RegistroJ2.New;
//
//      NovoRegistroJ2.DATA_EMISSAO            := Date;
//      NovoRegistroJ2.NUMERO_ITEM             := IntToStr(j);
//      NovoRegistroJ2.COD_ITEM                := IntToStr(j);
//      NovoRegistroJ2.DESC_ITEM               := 'descricao do item';
//      NovoRegistroJ2.QTDE_ITEM               := 1;
//      NovoRegistroJ2.UNI_ITEM                := 'UN';
//      NovoRegistroJ2.VL_UNIT                 := 2;
//      NovoRegistroJ2.DESCONTO_ITEM           := 2;
//      NovoRegistroJ2.ACRESCIMO_ITEM          := 2;
//      NovoRegistroJ2.VALOR_LIQUIDO           := 2;
//      NovoRegistroJ2.TOTALIZADOR_PARCIAL     := 2;
//      NovoRegistroJ2.CASAS_DECIMAIS_QTDE     := 2;
//      NovoRegistroJ2.CASAS_DECIMAIS_VAL_UNIT := 2;
//      NovoRegistroJ2.NUMERO_NOTA             := 2;
//      NovoRegistroJ2.SERIE_NOTA              := 2;
//      NovoRegistroJ2.CHAVE_NF                := GerarDados('I', 44);;
//    end;
  end;


  ACBrPAF.SaveToFile_RegistrosPAF('RegistrosPAFNFCe.txt');

  if FileExists('RegistrosPAFNFCe.txt') then
  begin
    mmArquivoGerado.Lines.LoadFromFile('RegistrosPAFNFCe.txt');
    pc1.ActivePageIndex:= 1;
  end;
end;

procedure TForm6.Button4Click(Sender: TObject);
var
  i: Integer;
  LRegistroV2: TRegistroV2;
  LRegistroV3: TRegistroV3;
begin
  // Sempre altere o layout antes de preencher os registros. Isso porque
  // ao alterar o layout, todos registros já lançados são apagados automaticamente.
  ACBrPAF.Layout := lpPAFNFCe;

  //V1
  ACBrPAF.PAF_V.RegistroV1.CNPJ             := edtCNPJ.Text;
  ACBrPAF.PAF_V.RegistroV1.IE               := edtIE.Text;
  ACBrPAF.PAF_V.RegistroV1.IM               := edtIM.Text;
  ACBrPAF.PAF_V.RegistroV1.RAZAOSOCIAL      := edtRAZAO.Text;

  //V2
  for i := 0 to 10 do
  begin
    LRegistroV2 := ACBrPAF.PAF_V.RegistrosV2.New;
    LRegistroV2.DATA := now;
    LRegistroV2.DAV:= i;
  end;
  //V3
  for i := 0 to 5 do
  begin
    LRegistroV3 := ACBrPAF.PAF_V.RegistrosV3.New;
    LRegistroV3.DAV:= i;
  end;

  //V4
  ACBrPAF.PAF_V.RegistroV4.DATA:= now;

  ACBrPAF.SaveToFile_V('RegistrosPAFNFCeV.txt');

  if FileExists('RegistrosPAFNFCeV.txt') then
  begin
    mmArquivoGerado.Lines.LoadFromFile('RegistrosPAFNFCeV.txt');
    pc1.ActivePageIndex:= 1;
  end;

end;

end.

