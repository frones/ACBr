{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2009   Isaque Pinheiro                      }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
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

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ACBrPAF, ACBrPAF_D, ACBrPAF_E, ACBrPAF_P,
  ACBrPAF_R, ACBrPAF_T, ACBrPaf_H, ACBrPAFRegistros, Math, ACBrEAD, ExtCtrls,
  ComCtrls, FileUtil;

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
    btnD: TButton;
    btnE: TButton;
    btnP: TButton;
    btnR: TButton;
    btnT: TButton;
    btnC: TButton;
    btnN: TButton;
    ACBrEAD: TACBrEAD;
    Image1: TImage;
    btnH: TButton;
    btnTITP: TButton;
    btnRegistrosPAF: TButton;
    pc1: TPageControl;
    ts1: TTabSheet;
    logErros: TMemo;
    ts2: TTabSheet;
    mmArquivoGerado: TMemo;
    procedure btnDClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PreencherHeader(Header: TRegistroX1);
    function GerarDados(Tipo: Char; Tam: integer): Variant;
    procedure btnEClick(Sender: TObject);
    procedure btnPClick(Sender: TObject);
    procedure btnRClick(Sender: TObject);
    procedure btnTClick(Sender: TObject);
    procedure ACBrPAFMsnError(const MsnError: String);
    procedure ACBrPAFPAFCalcEAD(Arquivo: String);
    procedure btnCClick(Sender: TObject);
    procedure btnNClick(Sender: TObject);
    procedure btnHClick(Sender: TObject);
    procedure btnTITPClick(Sender: TObject);
    procedure btnRegistrosPAFClick(Sender: TObject);
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

const
     NUM_FAB      = 'NUMFAB78901234567890';
     MF_ADICIONAL = '';
     TIPO_ECF     = 'ECF-IF';
     MARCA_ECF    = 'ACBr';
     MODELO_ECF   = 'PAF';

{$R *.lfm}

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

procedure TForm6.btnCClick(Sender: TObject);
var
     i: integer;
     OldMask:string;
begin
     // registro C1
     PreencherHeader(ACBrPAF.PAF_C.RegistroC1); // preencher header do arquivo
     // registro C2
     ACBrPAF.PAF_C.RegistroC2.Clear;
     for I := 1 to 15 do
     begin
       with ACBrPAF.PAF_C.RegistroC2.New do
       begin
          TANQUE                := '001';
          BOMBA                 := '001';
          BICO                  := '001';
          COMBUSTIVEL           := GerarDados('S',20);
          HORA_ABASTECIMENTO    := time;
          ENCERRANTE_INICIAL    := GerarDados('I',5);
          ENCERRANTE_FINAL      := GerarDados('I',5);
          STATUS_ABASTECIMENTO  := 'EMITIDO CF';
          NRO_SERIE_ECF         := GerarDados('S',14);
          DATA                  := date;
          HORA                  := time;
          COO                   := GerarDados('I',3);
          NRO_NOTA_FISCAL       := GerarDados('I',4);
          VOLUME                := GerarDados('I',2);
       end;
     end;
     OldMask := ACBrPAF.CurMascara;
     ACBrPAF.CurMascara := '';
     ACBrPAF.SaveFileTXT_C('PAF_C.txt');
     ACBrPAF.CurMascara := OldMask;
end;

procedure TForm6.btnDClick(Sender: TObject);
var
     D2: TRegistroD2;
     I, X: integer;
begin
     // registro D1
     PreencherHeader(ACBrPAF.PAF_D.RegistroD1); // preencher header do arquivo
     // registro D2
     ACBrPAF.PAF_D.RegistroD2.Clear;
     for I := 1 to 15 do
     begin
       D2:=ACBrPAF.PAF_D.RegistroD2.New;
       D2.NUM_FAB      := NUM_FAB;
       D2.MF_ADICIONAL := MF_ADICIONAL;
       D2.TIPO_ECF     := TIPO_ECF;
       D2.MARCA_ECF    := MARCA_ECF;
       D2.MODELO_ECF   := MODELO_ECF;
       D2.COO          := IntToStr(I * QualquerNumero);
       D2.NUM_DAV      := IntToStr(I * QualquerNumero);
       D2.DT_DAV       := Date - QualquerNumero;
       D2.TIT_DAV      := 'Pedido';
       D2.VLT_DAV      := GerarDados('I', 2);
       D2.COO_DFV      := '0';
       D2.NUMERO_ECF   := '1';
       D2.NOME_CLIENTE := 'NOME CLIENTE';
       D2.CPF_CNPJ     := '12345678921';

       D2.RegistroValido := True; // diz quando o registro foi modificado no banco

//       D2.CCF         :=''; // não está no layout do ato/cotepe
       for X := 1 to 5 do
       begin
         with D2.RegistroD3.New do
         begin
           DT_INCLUSAO   := DATE;
           NUM_ITEM      := X;
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

     end;
     ACBrPAF.SaveFileTXT_D('PAF_D.txt');
end;



procedure TForm6.btnEClick(Sender: TObject);
var
  E2: TRegistroE2;
  i: integer;
begin
  // registro E1
  PreencherHeader(ACBrPAF.PAF_E.RegistroE1); // preencher header do arquivo
  with ACBrPAF.PAF_E do
  begin
    RegistroE1.NUM_FAB      := NUM_FAB;
    RegistroE1.MF_ADICIONAL := MF_ADICIONAL;
    RegistroE1.TIPO_ECF     := TIPO_ECF;
    RegistroE1.MARCA_ECF    := MARCA_ECF;
    RegistroE1.MODELO_ECF   := MODELO_ECF;
    RegistroE1.DT_EST       := Now;

    RegistroE1.InclusaoExclusao := True;
    RegistroE1.RegistroValido   := False;

    // registro E2
    RegistroE2.Clear;
    for I := 1 to 15 do
    begin
      E2:=ACBrPAF.PAF_E.RegistroE2.New;
      E2.COD_MERC :=GerarDados('I',14);
      E2.DESC_MERC:=GerarDados('S',50);
      E2.UN_MED   :=GerarDados('S',2);
      E2.QTDE_EST :=GerarDados('I',3);

      E2.RegistroValido := False;
    end;
  end;

  ACBrPAF.SaveFileTXT_E('PAF_E.txt');
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

  ACBrPAF.SaveFileTXT_N('PAF_N.txt');
end;

procedure TForm6.btnPClick(Sender: TObject);
var
     P2: TRegistroP2;
     i: integer;
begin
     // registro P1
     PreencherHeader(ACBrPAF.PAF_P.RegistroP1); // preencher header do arquivo
     // registro P2
     ACBrPAF.PAF_P.RegistroP2.Clear;
     for I := 1 to 15 do
     begin
       P2:=ACBrPAF.PAF_P.RegistroP2.New;
       P2.COD_MERC_SERV :=GerarDados('I',14);
       P2.DESC_MERC_SERV:=GerarDados('S',50);
       P2.UN_MED        :=GerarDados('S',2);
       P2.IAT           :='A';
       P2.IPPT          :='T';
       P2.ST            :='FF';
       P2.ALIQ          :=0;
       P2.VL_UNIT       :=GerarDados('I',2);
     end;
     ACBrPAF.SaveFileTXT_P('PAF_P.txt');
end;

procedure TForm6.btnRClick(Sender: TObject);
var
  i,j: integer;
begin
  // Registro R1 - Identificação do ECF, do Usuário, do PAF-ECF e da Empresa Desenvolvedora e Dados do Arquivo
  with ACBrPAF.PAF_R.RegistroR01.New do
  begin
    NUM_FAB     := 'NUMFAB78901234567890'; //NUM_FAB;
    MF_ADICIONAL:= ''; // MF_ADICIONAL;
    TIPO_ECF    := 'ECF-IF'; // TIPO_ECF;
    MARCA_ECF   := 'IMPRESSORA ACBr'; //MARCA_ECF;
    MODELO_ECF  := 'MODELO DE ECF........'; //MODELO_ECF;
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
    DT_FIN      := date;
    ER_PAF_ECF  := '0104';

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
  //           CCF         := 0; //GerarDados('I', 3); // Outros documentos não tem CCF
             MP          := 'PG_R06 ' + GerarDados('S', 7);
             VL_PAGTO    := GerarDados('I', 2);
             IND_EST     := 'N';
             VL_EST      := 0;
          end;
        end;
     end;
    end;
  end;


  ACBrPAF.SaveFileTXT_R('PAF_R.txt');
end;

procedure TForm6.btnTClick(Sender: TObject);
var
     i: integer;
begin
     // registro T1
     PreencherHeader(ACBrPAF.PAF_T.RegistroT1); // preencher header do arquivo
     // registro T2
     ACBrPAF.PAF_T.RegistroT2.Clear;
     for I := 1 to 15 do
     begin
       with ACBrPAF.PAF_T.RegistroT2.New do
       begin
          DT_MOV     :=date;
          TP_DOCTO   :=''; // não faz parte do ato/cotepe
          SERIE      :=GerarDados('S',2);
          NUM_BILH_I :=GerarDados('I',2);
          NUM_BILH_F :=GerarDados('I',3);
          NUM_ECF    :='001';
          CRZ        :=GerarDados('I',3);
          CFOP       :='5102';
          VL_CONT    :=GerarDados('I',3);
          VL_BASECALC:=GerarDados('I',3);
          ALIQ       :=GerarDados('I',1);
          VL_IMPOSTO :=GerarDados('I',3);
          VL_ISENTAS :=GerarDados('I',3);
          VL_OUTRAS  :=GerarDados('I',3);
       end;
     end;
     ACBrPAF.SaveFileTXT_T('PAF_T.txt');
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

  ACBrPAF.SaveFileTXT_TITP('PAF_TITP.TXT');
end;

procedure TForm6.ACBrPAFMsnError(const MsnError: String);
begin
     logErros.Lines.Add(MsnError); // captura os erros encontrados pelo ACBrPAF
end;

procedure TForm6.ACBrPAFPAFCalcEAD(Arquivo: String);
begin
//
end;

procedure TForm6.btnHClick(Sender: TObject);
var
  H2: TRegistroH2;
  i: integer;
begin
  // registro E1
  PreencherHeader(ACBrPAF.PAF_H.RegistroH1); // preencher header do arquivo
  with ACBrPAF.PAF_H do
  begin
    RegistroH1.NUM_FAB      := NUM_FAB;
    RegistroH1.MF_ADICIONAL := MF_ADICIONAL;
    RegistroH1.TIPO_ECF     := TIPO_ECF;
    RegistroH1.MARCA_ECF    := MARCA_ECF;
    RegistroH1.MODELO_ECF   := MODELO_ECF;
    RegistroH1.DT_EST       := Now;

    RegistroH1.InclusaoExclusao := False;
    RegistroH1.RegistroValido   := False;

    // registro E2
    RegistroH2.Clear;
    for I := 1 to 15 do
    begin
      H2 := ACBrPAF.PAF_H.RegistroH2.New;
      H2.CNPJ_CRED_CARTAO := '99.999.999/9999-11';
      H2.NUM_FAB          := GerarDados('S', 20);
      H2.MF_ADICIONAL     := '';
      H2.TIPO_ECF         := GerarDados('S', 7);
      H2.MARCA_ECF        := GerarDados('S', 20);
      H2.MODELO_ECF       := GerarDados('S', 20);
      H2.COO              := GerarDados('I', 6);
      H2.CCF              := GerarDados('I', 6);
      H2.VLR_TROCO        := GerarDados('I', 2);
      H2.DT_TROCO         := DATE;
      H2.CPF              := '111.111.111-99';
      H2.Titulo           := GerarDados('S', 7);

      H2.RegistroValido := True;
    end;
  end;

  ACBrPAF.SaveFileTXT_H('PAF_H.txt');
end;

procedure TForm6.btnRegistrosPAFClick(Sender: TObject);
var
  i, j: Integer;
begin
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



  ACBrPAF.SaveFileTXT_RegistrosPAF( 'RegistrosPAF.txt');
  if FileExistsUTF8('RegistrosPAF.txt') { *Converted from FileExists* } then
    begin
      mmArquivoGerado.Lines.LoadFromFile('RegistrosPAF.txt');
      pc1.ActivePageIndex:= 1;
    end;
end;

end.

