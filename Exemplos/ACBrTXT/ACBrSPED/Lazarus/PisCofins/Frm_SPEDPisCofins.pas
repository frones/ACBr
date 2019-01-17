unit Frm_SPEDPisCofins;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
{$IFNDEF FPC}
  Windows, Messages,
{$ENDIF}
  SysUtils, Variants, Classes, Graphics, Controls, Forms, ACBrEPCBlocos,
  Dialogs, StdCtrls, ACBrSpedPisCofins, ExtCtrls, ComCtrls, ACBrUtil,
  ACBrTXTClass;

type
  
  { TFrmSPEDPisCofins }

  TFrmSPEDPisCofins = class(TForm)
    btnB_0: TButton;
    btnB_1: TButton;
    btnB_C: TButton;
    btnB_D: TButton;
    btnB_I: TButton;
    btnB_P: TButton;
    btnError: TButton;
    btnTXT: TButton;
    btnB_9: TButton;
    btnVariosBlocos: TButton;
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
    ACBrSPEDPisCofins1: TACBrSPEDPisCofins;
    btnB_A: TButton;
    btnB_F: TButton;
    btnB_M: TButton;
    procedure btnB_0Click(Sender: TObject);
    procedure btnB_9Click(Sender: TObject);
    procedure btnB_IClick(Sender: TObject);
    procedure btnB_PClick(Sender: TObject);
    procedure btnTXTClick(Sender: TObject);
    procedure btnB_1Click(Sender: TObject);
    procedure btnB_CClick(Sender: TObject);
    procedure btnB_DClick(Sender: TObject);
    procedure btnErrorClick(Sender: TObject);
    procedure edtFileChange(Sender: TObject);
    procedure cbConcomitanteClick(Sender: TObject);
    procedure ACBrSPEDPisCofins1Error(const MsnError: AnsiString);
    procedure btnB_FClick(Sender: TObject);
    procedure btnB_MClick(Sender: TObject);
    procedure btnB_AClick(Sender: TObject);
    procedure btnVariosBlocosClick(Sender: TObject);
  private
     procedure LoadToMemo;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmSPEDPisCofins: TFrmSPEDPisCofins;

implementation

uses
  ACBrEPCBloco_0, ACBrEPCBloco_1
, ACBrEPCBloco_A, ACBrEPCBloco_C, ACBrEPCBloco_D, ACBrEPCBloco_F
, ACBrEPCBloco_M;

{$IFDEF FPC}
 {$R *.lfm}
{$ELSE}
 {$R *.dfm}
{$ENDIF}

procedure TFrmSPEDPisCofins.ACBrSPEDPisCofins1Error(const MsnError: AnsiString);
begin
   memoError.Lines.Add(MsnError);
end;

procedure TFrmSPEDPisCofins.btnB_0Click(Sender: TObject);
const
  strUNID: array[0..4] of string = ('PC', 'UN', 'LT', 'KG', 'MT');

var
int0140: integer;
int0150: integer;
int0190: integer;
int0200: integer;

begin
   // Alimenta o componente com informações para gerar todos os registros do
   // Bloco 0.

   cbConcomitante.Enabled := False ;
   btnB_0.Enabled := false;
   btnB_C.Enabled := True ;
   with ACBrSPEDPisCofins1 do
   begin
     DT_INI := StrToDate('01/01/2014');
     DT_FIN := StrToDate('31/01/2014');
     IniciaGeracao;
   end;

   if cbConcomitante.Checked then
   begin
      with ACBrSPEDPisCofins1 do
      begin
//         DT_INI := StrToDate('01/04/2011');
//         DT_FIN := StrToDate('30/04/2011');
         LinhasBuffer := StrToIntDef( edBufLinhas.Text, 0 );

//         IniciaGeracao;
      end;

      LoadToMemo;
   end;

   with ACBrSPEDPisCofins1.Bloco_0 do
   begin
      // Dados da Empresa
      with Registro0000New do
      begin
        COD_VER          := vlVersao101;
        TIPO_ESCRIT      := tpEscrOriginal;
        IND_SIT_ESP      := indSitAbertura;
        NUM_REC_ANTERIOR := '';
        NOME             := 'NOME DA EMPRESA';
        CNPJ             := '11111111000191';
        UF               := 'ES';
        COD_MUN          := 3200607;
        SUFRAMA          := '';
        IND_NAT_PJ       := indNatPJSocEmpresariaGeral;
        IND_ATIV         := indAtivIndustrial;

        with Registro0001New do
        begin
           IND_MOV := imComDados;

           // FILHO - Dados do contador.
           with Registro0100New do
           begin
              NOME       := 'NOME DO CONTADOR';
              CPF        := '12345678909'; // Deve ser uma informação valida
              CRC        := '123456';
              CNPJ       := '22222222000000';
              CEP        := '';
              ENDERECO   := '';
              NUM        := '';
              COMPL      := '';
              BAIRRO     := '';
              FONE       := '';
              FAX        := '';
              EMAIL      := '';
              COD_MUN    := 3200607;
           end;

           // FILHO - Regime de Apuração
           with Registro0110New do
           begin
              COD_INC_TRIB  := codEscrOpIncNaoCumulativo; //codEscrOpIncCumulativo;
              IND_APRO_CRED := indMetodoApropriacaoDireta;
              COD_TIPO_CONT := codIndTipoConExclAliqBasica;
              //Campo IND_REG_CUM apenas para Lucro Presumido e (COD_INC_TRIB = 2)
              //IND_REG_CUM := 1;
           end;


           //0140 - Tabela de Cadastro de Estabelecimento
           for int0140 := 1 to 2 do
           begin
           // FILHO
              with Registro0140New do
              begin
                 COD_EST := IntToStr(int0140);
                 NOME    := 'NOME DO ESTABELECIMENTO '+IntToStr(int0140);
                 if int0140 = 1 then
                    CNPJ    := '11111111000191'
                 else
                    CNPJ    := '11111111000272'; //oito primeiros dígitos devem bater...

                 UF      := 'ES';
                 IE      := '';
                 COD_MUN := 3200607;
                 IM      := '';
                 SUFRAMA := '';

                 // 10 Clientes por estabelecimento
                 for int0150 := 1 to 10 do
                 begin
                    //0150 - Tabela de Cadastro do Participante
                    with Registro0150New do
                    begin
                       COD_PART := IntToStr(int0150);
                       NOME     := 'NOME DO CLIENTE '+ IntToStr(int0150);
                       COD_PAIS := '1058';
                       CNPJ     := '';
                       CPF      := '12345678909';
                       IE       := '';
                       COD_MUN  := 3200607;
                       SUFRAMA  := '';
                       ENDERECO := 'ENDERECO DO CLIENTE '+ IntToStr(int0150);
                       NUM      := IntToStr(int0150);
                       COMPL    := 'COMPLEMENTO DO CLIENTE '+ IntToStr(int0150);
                       BAIRRO   := 'BAIRRO';
                       //
                    end;
                 end;

                 // 0190 - Identificação das Unidades de Medida
                 for int0190 := Low(strUNID) to High(strUNID) do
                 begin
                    with Registro0190New do
                    begin
                       UNID  := strUNID[int0190];
                       DESCR := 'Descricao ' + strUNID[int0190];
                    end;
                 end;

                 //10 produtos/serviços
                 for int0200 := 1 to 10 do
                 begin
                    // 0200 - Tabela de Identificação do Item (Produtos e Serviços)
                    with Registro0200New do
                    begin
                       COD_ITEM     := FormatFloat('000000', int0200);
                       DESCR_ITEM   := 'DESCRIÇÃO DO ITEM';
                       COD_BARRA    := '';
                       COD_ANT_ITEM := '';
                       UNID_INV     := strUNID[int0200 mod (High(strUNID))];
                       TIPO_ITEM    := tiMercadoriaRevenda;
                       COD_NCM      := '12345678';
                       EX_IPI       := '';
                       COD_GEN      := '';
                       COD_LST      := '';
                       ALIQ_ICMS    := 0;

                      //Cria uma alteração apenas para o item 5...
                      if (int0200 = 5) then with Registro0205New do
                      begin
                        DESCR_ANT_ITEM := 'DESCRIÇÃO ANTERIOR DO ITEM 5';
                        DT_INI := StrToDate('01/04/2011');
                        DT_FIM := StrToDate('15/04/2011'); //Observe que o campo é DT_FIM e não DT_FIN
                      end;
                    end;
                 end;
              end;
           end;

           // FILHO - REGISTRO 0500: PLANO DE CONTAS CONTÁBEIS
           with Registro0500New do
           begin
             DT_ALT := StrToDate('01/04/2011');
             COD_NAT_CC := ncgAtivo;
             IND_CTA := indCTASintetica;
             NIVEL := '0';
             COD_CTA := '01';
             NOME_CTA := 'NOME CTA';
             COD_CTA_REF := '0';
             CNPJ_EST := '33333333000191';
           end;

        end;
      end;

   end;

   if cbConcomitante.Checked then
   begin
      ACBrSPEDPisCofins1.WriteBloco_0;
      LoadToMemo;
   end;
end;

procedure TFrmSPEDPisCofins.btnB_9Click(Sender: TObject);
begin
   btnB_9.Enabled := False ;
   ACBrSPEDPisCofins1.WriteBloco_9;
   LoadToMemo;

   // Habilita os botões
   btnB_0.Enabled := true;
   btnB_1.Enabled := true;
   btnB_A.Enabled := true;
   btnB_C.Enabled := true;
   btnB_D.Enabled := true;
   btnB_F.Enabled := true;
   btnB_M.Enabled := true;

   cbConcomitante.Enabled := True ;
end;

procedure TFrmSPEDPisCofins.btnB_IClick(Sender: TObject);
begin
  // Alimenta o componente com informações para gerar todos os registros do
   // Bloco I.
   with ACBrSPEDPisCofins1.Bloco_I do
   begin
      with RegistroI001New do
      begin
        IND_MOV := imComDados;

        //I010 - Identificação do Estabelecimento
        with RegistroI010New do
        begin
           CNPJ := '11111111000191';
           IND_ATIV := 1;

           //I100 -
           with RegistroI100New do
           begin
             VL_REC := 0;
             CST_PIS_COFINS := stpiscofinsOperTribuComAliqBasica;
             VL_TOT_DED_GER := 0;
             VL_TOT_DED_ESP := 0;
             VL_BC_PIS := 0;
             ALIQ_PIS := 0 ;
             VL_PIS := 0;
             VL_BC_COFINS := 0;
             ALIQ_COFINS := 0;
             VL_COFINS := 0;
             INFO_COMPL := '';
           end;
        end;
      end;
   end;
   btnB_I.Enabled := false;

   if cbConcomitante.Checked then
   begin
      ACBrSPEDPisCofins1.WriteBloco_I;
      LoadToMemo;
   end;
end;

procedure TFrmSPEDPisCofins.btnB_PClick(Sender: TObject);
begin
  // Alimenta o componente com informações para gerar todos os registros do
   // Bloco P.
   with ACBrSPEDPisCofins1.Bloco_P do
   begin
      with RegistroP001New do
      begin
        IND_MOV := imComDados;

        //P010 - Identificação do Estabelecimento
        with RegistroP010New do
        begin
           CNPJ := '11111111000191';

           //P100 -
           with RegistroP100New do
           begin
             DT_INI            := now;
             DT_FIM            := now;
             VL_REC_TOT_EST    := 0;
             COD_ATIV_ECON     := 'XXX';
             VL_REC_ATIV_ESTAB := 0;
             VL_EXC            := 0;
             VL_BC_CONT        := 0;
             ALIQ_CONT         := 0;
             VL_CONT_APU       := 0;
             COD_CTA           := '';
             INFO_COMPL        := '';
             //RegistroP110.new
             //RegistroP199.new
           end;
        end;
      end;
   end;
   btnB_P.Enabled := false;

   if cbConcomitante.Checked then
   begin
      ACBrSPEDPisCofins1.WriteBloco_P;
      LoadToMemo;
   end;
end;

procedure TFrmSPEDPisCofins.btnTXTClick(Sender: TObject);
begin
   btnTXT.Enabled := False ;

   ACBrSPEDPisCofins1.LinhasBuffer := StrToIntDef( edBufLinhas.Text, 0 );

   with ACBrSPEDPisCofins1 do
   begin
      DT_INI := StrToDate('01/01/2014');
      DT_FIN := StrToDate('31/01/2014');
   end;

   // Limpa a lista de erros.
   memoError.Lines.Clear;
   // Informa o pasta onde será salvo o arquivo TXT.
   ACBrSPEDPisCofins1.Path := '.\';
   ACBrSPEDPisCofins1.Arquivo := edtFile.Text;

   // Método que gera o arquivo TXT.
   ACBrSPEDPisCofins1.SaveFileTXT ;

   // Carrega o arquivo TXT no memo.
   LoadToMemo;

   // Habilita os botões
   btnB_0.Enabled := true;
   btnB_1.Enabled := true;
   btnB_A.Enabled := true;
   btnB_C.Enabled := true;
   btnB_D.Enabled := true;
   btnB_F.Enabled := true;
   btnB_M.Enabled := true;
   btnB_I.Enabled := true;
   btnB_P.Enabled := true;
   btnTXT.Enabled := True ;
   cbConcomitante.Enabled := True ;
end;

procedure TFrmSPEDPisCofins.btnVariosBlocosClick(Sender: TObject);
begin
  btnB_0.Click;
  btnB_A.Click;
  btnB_C.Click;
  btnB_D.Click;
  btnB_F.Click;
  btnB_I.Click;
  btnB_M.Click;
  btnB_P.Click;
  btnB_1.Click;

end;

procedure TFrmSPEDPisCofins.btnErrorClick(Sender: TObject);
begin
   with ACBrSPEDPisCofins1 do
   begin
      DT_INI := StrToDate('01/04/2011');
      DT_FIN := StrToDate('30/04/2011');
   end;

   // Limpa a lista de erros.
   memoError.Lines.Clear;

   // Método que gera o arquivo TXT.
   ACBrSPEDPisCofins1.SaveFileTXT ;

   // Habilita os botões
   btnB_0.Enabled := true;
   btnB_1.Enabled := true;
   btnB_A.Enabled := true;
   btnB_C.Enabled := true;
   btnB_D.Enabled := true;
   btnB_F.Enabled := true;
   btnB_M.Enabled := true;
   btnB_I.Enabled := true;
   btnB_P.Enabled := true;
end;

procedure TFrmSPEDPisCofins.btnB_1Click(Sender: TObject);
begin
   btnB_1.Enabled := false;
   btnB_9.Enabled := cbConcomitante.Checked ;

   // Alimenta o componente com informações para gerar todos os registros do
   // Bloco 1.
//   with ACBrSPEDPisCofins1.Bloco_1 do
//   begin
//      with Registro1001New do
//      begin
//        IND_MOV := 1;
//      end;
//   end;

   with ACBrSPEDPisCofins1.Bloco_1 do
   begin
      with Registro1001New do
      begin
         IND_MOV := imSemDados;

         with Registro1050New do
         begin
           DT_REF         :=now;
           IND_AJ_BC      := tcaDevolucoesVendas   ;
           CNPJ           := '12345678901234';
           VL_AJ_TOT      :=0;
           VL_AJ_CST01    :=0;
           VL_AJ_CST02    :=0;
           VL_AJ_CST03    :=0;
           VL_AJ_CST04    :=0;
           VL_AJ_CST05    :=0;
           VL_AJ_CST06    :=0;
           VL_AJ_CST07    :=0;
           VL_AJ_CST08    :=0;
           VL_AJ_CST09    :=0;
           VL_AJ_CST49    :=0;
           VL_AJ_CST99    :=0;
           IND_APROP      :=iaaRefPisCofins      ;
           NUM_REC        :='1234';
           INFO_COMPL     :='';
         end;
      end;
   end;


   if cbConcomitante.Checked then
   begin
      ACBrSPEDPisCofins1.WriteBloco_1;
      LoadToMemo;
   end;
end;

procedure TFrmSPEDPisCofins.btnB_CClick(Sender: TObject);
var
INotas: Integer;
IItens: Integer;
NNotas: Integer;
BNotas: Integer;
begin
   // Alimenta o componente com informações para gerar todos os registros do
   // Bloco C.
   btnB_C.Enabled := false;
   btnB_D.Enabled := True ;

   NNotas := StrToInt64Def(edNotas.Text,1);
   BNotas := StrToInt64Def(edBufNotas.Text,1);

   ProgressBar1.Visible := cbConcomitante.Checked ;
   ProgressBar1.Max     := NNotas;
   ProgressBar1.Position:= 0 ;

   with ACBrSPEDPisCofins1.Bloco_C do
   begin
      with RegistroC001New do
      begin
         IND_MOV := imComDados;

         //C010 - Identificação do Estabelecimento
         with RegistroC010New do
         begin
           CNPJ := '11111111000191';
           IND_ESCRI := IndEscriConsolidado;

           //Inserir Notas...
           for INotas := 1 to NNotas do
           begin
              //C100 - Documento - Nota Fiscal (código 01), Nota Fiscal Avulsa (código 1B), Nota
              // Fiscal de Produtor (código 04) e NF-e (código 55)
              with RegistroC100New do
              begin
                IND_OPER      := tpEntradaAquisicao;
                IND_EMIT      := edEmissaoPropria;
                COD_PART      := '2'; //Baseado no registro 0200
                COD_MOD       := '01';
                COD_SIT       := sdRegular;
                SER           := '';
                NUM_DOC       := FormatFloat('000000000',INotas); //
                CHV_NFE       := '';
                DT_DOC        := DT_INI + INotas;
                DT_E_S        := DT_INI + INotas;
                VL_DOC        := 0;
                IND_PGTO      := tpSemPagamento;
                VL_DESC       := 0;
                VL_ABAT_NT    := 0;
                VL_MERC       := 0;
                IND_FRT       := tfSemCobrancaFrete;
                VL_FRT        := 0;
                VL_SEG        := 0;
                VL_OUT_DA     := 0;
                VL_BC_ICMS    := 0;
                VL_ICMS       := 0;
                VL_BC_ICMS_ST := 0;
                VL_ICMS_ST    := 0;
                VL_IPI        := 0;
                VL_PIS        := 0;
                VL_COFINS     := 0;
                VL_PIS_ST     := 0;
                VL_COFINS_ST  := 0;

                //10 itens para cada nota...
                for IItens := 1 to 10 do
                begin
                  //c170 - Complemento de Documento – Itens do Documento (códigos 01, 1B, 04 e 55)
                  with RegistroC170New do   //Inicio Adicionar os Itens:
                  begin
                     NUM_ITEM         := FormatFloat('000', IItens);
                     COD_ITEM         := FormatFloat('000000',StrToInt(NUM_ITEM));
                     DESCR_COMPL      := FormatFloat('NF000000',INotas)+' -> ITEM '+COD_ITEM;
                     QTD              := 1.123456; // O último dígito deve ser ignorado no arquivo
                     UNID             := 'UN';
                     VL_ITEM          := 0;
                     VL_DESC          := 0;
                     IND_MOV          := mfNao;
                     CST_ICMS         := sticmsTributadaIntegralmente;
                     CFOP             := '1252';
                     COD_NAT          := '';
//                     COD_NAT          := '64'; //Informar no 0400 antes de utilizá-lo
                     VL_BC_ICMS       := 0;
                     ALIQ_ICMS        := 0;
                     VL_ICMS          := 0;
                     VL_BC_ICMS_ST    := 0;
                     ALIQ_ST          := 0;
                     VL_ICMS_ST       := 0;
                     IND_APUR         := iaMensal;
                     CST_IPI          := stipiEntradaIsenta;
                     COD_ENQ          := '';
                     VL_BC_IPI        := 0;
                     ALIQ_IPI         := 0;
                     VL_IPI           := 0;
                     CST_PIS          := stpisOutrasOperacoes;
                     VL_BC_PIS        := 0;
                     ALIQ_PIS_PERC    := 0;
                     QUANT_BC_PIS     := 0;
                     ALIQ_PIS_R       := 1;
                     VL_PIS           := 0;
                     CST_COFINS       := stcofinsOutrasOperacoes;
                     VL_BC_COFINS     := 0;
                     ALIQ_COFINS_PERC := 0;
                     QUANT_BC_COFINS  := 0;
                     ALIQ_COFINS_R    := 0;
                     VL_COFINS        := 0;
                     COD_CTA          := '01'; //Baseado no 0500
                  end; //Fim dos Itens;
                end;

                if cbConcomitante.Checked then
                begin
                   if (INotas mod BNotas) = 0 then   // Gravar a cada BNotas notas
                   begin
                      // Grava registros na memoria para o TXT, e limpa memoria
                      ACBrSPEDPisCofins1.WriteBloco_C( False );  // False, NAO fecha o Bloco
                      ProgressBar1.Position := INotas;
                      Application.ProcessMessages;
                   end;
                end;

              end;
           end;


//           //Registros c190
//           for INotas := 1 to NNotas  do
//           begin
//             // c190 - Consolidação de Notas Fiscais Eletrônicas (Código 55) – Operações de
//             // Aquisição com Direito a Crédito, e Operações de Devolução de Compras e
//             // Vendas.
//             with RegistroC190New do
//             begin
//               COD_MOD := '55';
//               DT_REF_INI := Date;
//               DT_REF_FIN := Date;
//               COD_ITEM := ''; //Código do item (campo 02 do Registro 0200)
//               COD_NCM := '';
//               EX_IPI := '';
//               VL_TOT_ITEM := 0;
//
//               //Registros C191 e C195
//
//
//             end;
//           end;
         end;
      end;
   end;

   if cbConcomitante.Checked then
   begin
      ACBrSPEDPisCofins1.WriteBloco_C(True);  // True, fecha o Bloco
      LoadToMemo;
   end;

   ProgressBar1.Visible := False ;
end;

procedure TFrmSPEDPisCofins.btnB_DClick(Sender: TObject);
begin
   // Alimenta o componente com informações para gerar todos os registros do
   // Bloco D.
//   with ACBrSPEDPisCofins1.Bloco_D do
//   begin
//      with RegistroD001New do
//      begin
//        IND_MOV := 1;
//      end;
//   end;


   with ACBrSPEDPisCofins1.Bloco_D do
   begin
      with RegistroD001New do
      begin
        IND_MOV := imComDados;


        //Estabelecimento
        with RegistroD010New do
        begin
          CNPJ := '11111111000191';

//          for INotas := 1 to NNotas do
//          begin
//            with RegistroD100New do
//            begin
//              IND_OPER := 0;
//              IND_EMIT := 0;
//              COD_PART := '';
//              COD_MOD :=  '';
//            end;
//          end;

          //D200 - Resumo da Escrituração Diária – Prestação de Serviços de Transportes
          // (Códigos 07, 08, 8B, 09, 10, 11, 26, 27 e 57).
          with RegistroD200New do
          begin
          //|D200|08|00|||11574|11854|6352|000011574|000011854|6352|25072011|6807,57|0,00|
            COD_MOD := '08';
            COD_SIT := sdfRegular;
            SER := '';
            SUB := '';
            NUM_DOC_INI := 11574;
            NUM_DOC_FIN := 11854;
            CFOP := 6352;
            DT_REF := DT_INI;// StrToDate('15/04/2011');
            VL_DOC := 6807.57;
            VL_DESC := 0;
          end;


//          with RegistroD350New do
//          begin
//            COD_MOD := '2E';
//            ECF_MOD := 'MODELO DO ECF';
//            ECF_FAB := 'NUMSERIEECF';
//            DT_DOC := DT_INI;
//            CRO := 1;
//            CRZ := 1;
//            NUM_COO_FIN := 10;
//            GT_FIN := 10000;
//            VL_BRT := 10000;
//            CST_PIS := stpisValorAliquotaDiferenciada;
//            VL_BC_PIS := 100;
//            ALIQ_PIS := 2;
//            QUANT_BC_PIS := 100;
//            ALIQ_PIS_QUANT := 1;
//            VL_PIS := 1;
//            CST_COFINS := stcofinsValorAliquotaDiferenciada;
//            VL_BC_COFINS := 100;
//            ALIQ_COFINS := 2;
//            QUANT_BC_COFINS := 300;
//            ALIQ_COFINS_QUANT := 2;
//            VL_COFINS := 3;
//            COD_CTA := '';
//          end;
        end;
      end;
   end;
   btnB_D.Enabled := false;

   if cbConcomitante.Checked then
   begin
      ACBrSPEDPisCofins1.WriteBloco_D;
      LoadToMemo;
   end;
end;

procedure TFrmSPEDPisCofins.edtFileChange(Sender: TObject);
begin
   ACBrSPEDPisCofins1.Arquivo := edtFile.Text ;
end;

procedure TFrmSPEDPisCofins.LoadToMemo;
begin
   memoTXT.Lines.Clear;
   if FileExists(ACBrSPEDPisCofins1.Path + ACBrSPEDPisCofins1.Arquivo ) { *Converted from FileExists* } then
      memoTXT.Lines.LoadFromFile(ACBrSPEDPisCofins1.Path + ACBrSPEDPisCofins1.Arquivo);
end;

procedure TFrmSPEDPisCofins.cbConcomitanteClick(Sender: TObject);
begin
   btnTXT.Enabled   := not cbConcomitante.Checked ;
   btnError.Enabled := btnTXT.Enabled ;

   edBufNotas.Enabled := cbConcomitante.Checked ;

   if not cbConcomitante.Checked then
   begin
      btnB_0.Enabled := True ;
      btnB_A.Enabled := False ;
      btnB_C.Enabled := False ;
      btnB_D.Enabled := False ;
      btnB_F.Enabled := False ;
      btnB_M.Enabled := False ;
      btnB_1.Enabled := False ;
      btnB_9.Enabled := False ;
   end;
end;

procedure TFrmSPEDPisCofins.btnB_FClick(Sender: TObject);
begin
   // Alimenta o componente com informações para gerar todos os registros do
   // Bloco F.
   with ACBrSPEDPisCofins1.Bloco_F do
   begin
      with RegistroF001New do
      begin
        IND_MOV := imComDados;

        //F010 - Identificação do Estabelecimento
        with RegistroF010New do
        begin
           CNPJ := '11111111000191';

           //F100 - Demais Documentos e Operações Geradoras de Contribuição e Créditos
           with RegistroF100New do
           begin
              IND_OPER      := indRepCustosDespesasEncargos;
              COD_PART      := '1';
              COD_ITEM      := '000001'; //Codigo do Item no registro 0200
              DT_OPER       := Date();
              VL_OPER       := 0.01;  //Deve ser Maior que zero
              CST_PIS       := stpisCredPresAquiExcRecTribMercInt;  //Para Operação Representativa de Aquisição, Custos, Despesa ou Encargos, Sujeita à Incidência de Crédito, o CST deve ser referente a Operações com Direito a Crédito (50 a 56) ou a Crédito Presumido (60 a 66).Para Operação Representativa de Receita Auferida, Sujeita ao Pagamento da Contribuição, o CST deve ser igual a 01, 02, 03 ou 05.Para Operação Representativa de Receita Auferida NÃO Sujeita ao Pagamento da Contribuição, o CST deve ser igual a 04, 06, 07, 08, 09, 49 ou 99.
              VL_BC_PIS     := 0;
              ALIQ_PIS      := 1.2375;
              VL_PIS        := 0;
              CST_COFINS    := stcofinsCredPresAquiExcRecTribMercInt;
              VL_BC_COFINS  := 0;
              ALIQ_COFINS   := 3.04;
              VL_COFINS     := 0;
              NAT_BC_CRED   := bccAqBensRevenda;
              IND_ORIG_CRED := opcMercadoInterno;
              COD_CTA       := '';
              COD_CCUS      := '';
//              COD_CCUS      := '123';//Para usar o COD_CCUS é necessário gerar, primeiro, um registro 0600 correspondente.
              DESC_DOC_OPER := ''; 
           end;
        end;
      end;
   end;
   btnB_F.Enabled := false;

   if cbConcomitante.Checked then
   begin
      ACBrSPEDPisCofins1.WriteBloco_F;
      LoadToMemo;
   end;
end;

procedure TFrmSPEDPisCofins.btnB_MClick(Sender: TObject);
var
  vlBC, vlBcCofins, aliqCofins, vlcredNC: Real;
begin
   // Alimenta o componente com informações para gerar todos os registros do
   // Bloco M.
   with ACBrSPEDPisCofins1.Bloco_M do
   begin
      with RegistroM001New do
      begin
        IND_MOV := imComDados;

        //M100 - Crédito de PIS/PASEP Relativo ao Período
         with RegistroM100New do
         begin
            COD_CRED       := '106';
            IND_CRED_ORI   := icoOperProprias;
            VL_BC_PIS      := 0;
            ALIQ_PIS       := 1.2375;
            QUANT_BC_PIS   := 0;
            ALIQ_PIS_QUANT := 0;
            VL_CRED        := 0; //OBRIGATORIO
            VL_AJUS_ACRES  := 0;
            VL_AJUS_REDUC  := 0;
            VL_CRED_DIF    := 0;
            VL_CRED_DISP   := 0;
            IND_DESC_CRED  := idcTotal;
            VL_CRED_DESC   := 0; //Valor do Crédito disponível, descontado da contribuição apurada no próprio período. Se IND_DESC_CRED=0, informar o valor total do Campo 12; Se IND_DESC_CRED=1, informar o valor parcial do Campo 12.
            SLD_CRED       := 0;

            with RegistroM105New do
            begin
              NAT_BC_CRED := bccAqBensRevenda;
              CST_PIS := stpisCredPresAquiExcRecTribMercInt;
              VL_BC_PIS_TOT := 0;
              VL_BC_PIS_CUM := 0;
              VL_BC_PIS_NC := 0;
              VL_BC_PIS := 0;
              QUANT_BC_PIS_TOT := 0;
              QUANT_BC_PIS := 0;
              DESC_CRED := '';
            end;

         end;

         with RegistroM200New do
         begin
           VL_TOT_CONT_NC_PER := 0;
           VL_TOT_CRED_DESC := 0;
           VL_TOT_CRED_DESC_ANT := 0;
           VL_TOT_CONT_NC_DEV := 0;
           VL_RET_NC := 0;
           VL_OUT_DED_NC := 0;
           VL_CONT_NC_REC := 0;
           VL_TOT_CONT_CUM_PER := 0;
           VL_RET_CUM := 0;
           VL_OUT_DED_CUM := 0;
           VL_CONT_CUM_REC := 0;
           VL_TOT_CONT_REC := 0;

//           with RegistroM210New do
//           begin
//             COD_CONT := ccNaoAcumAliqBasica;
//             VL_REC_BRT := 0;
//             VL_BC_CONT := 0;
//             ALIQ_PIS := 0;
//             QUANT_BC_PIS := 0;
//             ALIQ_PIS_QUANT := 0;
//             VL_CONT_APUR := 0.01;
//             VL_AJUS_ACRES := 0;
//             VL_AJUS_REDUC := 0;
//             VL_CONT_DIFER := 0;
//             VL_CONT_DIFER_ANT := 0;
//             VL_CONT_PER := 0.01;
//           end;

         end;

         with RegistroM500New do
         begin
           COD_CRED           := '106';
           IND_CRED_ORI       := icoOperProprias;
           VL_BC_COFINS       := 0;
           ALIQ_COFINS        := 3.0400;
           QUANT_BC_COFINS    := 0;
           ALIQ_COFINS_QUANT  := 0;
           VL_CRED            := 0; //OBRIGATORIO
           VL_AJUS_ACRES      := 0;
           VL_AJUS_REDUC      := 0;
           VL_CRED_DIFER      := 0;
           VL_CRED_DISP       := 0;
           IND_DESC_CRED      := idcTotal;
           VL_CRED_DESC       := 0;
           SLD_CRED           := 0;

           with RegistroM505New do
           begin
              NAT_BC_CRED := bccAqBensRevenda;
              CST_COFINS := stcofinsCredPresAquiExcRecTribMercInt;
              VL_BC_COFINS_TOT := 0;
              VL_BC_COFINS_CUM := 0;
              VL_BC_COFINS_NC := 0;
              VL_BC_COFINS := 0;
              QUANT_BC_COFINS_TOT := 0;
              QUANT_BC_COFINS := 0;
              DESC_CRED := '';
           end;

         end;


         with RegistroM600 do
         begin
           VL_TOT_CONT_NC_PER := 0;
           VL_TOT_CRED_DESC := 0;
           VL_TOT_CRED_DESC_ANT := 0;
           VL_TOT_CONT_NC_DEV := 0;
           VL_RET_NC := 0;
           VL_OUT_DED_NC := 0;
           VL_CONT_NC_REC := 0;
           VL_TOT_CONT_CUM_PER := 0;
           VL_RET_CUM := 0;
           VL_OUT_DED_CUM := 0;
           VL_CONT_CUM_REC := 0;
           VL_TOT_CONT_REC := 0;
//
//           with RegistroM610New do
//           begin
//             COD_CONT := ccNaoAcumAliqBasica;
//             VL_REC_BRT := 2;
//             VL_BC_CONT := 3;
//             ALIQ_COFINS := 4;
//             QUANT_BC_COFINS := 5;
//             ALIQ_COFINS_QUANT := 6;
//             VL_CONT_APUR := 7;
//             VL_AJUS_ACRES := 8;
//             VL_AJUS_REDUC := 9;
//             VL_CONT_DIFER := 10;
//             VL_CONT_DIFER_ANT := 11;
//             VL_CONT_PER := 12;
//           end;
//
         end;

//          vlBC := 293040.02;
//          vlBcCofins := 20823.48;
//          aliqCofins := 7.6;
//          vlcredNC := 0;
//          with RegistroM600New do begin
//            {02} VL_TOT_CONT_NC_PER := StrToCurr(FormatCurr('#####0.00',(vlBcCofins*aliqCofins)/100));
//            {03} VL_TOT_CRED_DESC := vlcredNC;
//            {04} VL_TOT_CRED_DESC_ANT := 0;
//            {05} VL_TOT_CONT_NC_DEV := StrToCurr(FormatCurr('#####0.00',(vlBcCofins*aliqCofins)/100))-vlcredNC;
//            {06} VL_RET_NC := 0;
//            {07} VL_OUT_DED_NC := 0;
//            {08} VL_CONT_NC_REC := StrToCurr(FormatCurr('#####0.00',(vlBcCofins*aliqCofins)/100))-vlcredNC;
//            {09} VL_TOT_CONT_CUM_PER := 0;
//            {10} VL_RET_CUM := 0;
//            {11} VL_OUT_DED_CUM := 0;
//            {12} VL_CONT_CUM_REC := 0;
//            {13} VL_TOT_CONT_REC := StrToCurr(FormatCurr('#####0.00',(vlBcCofins*aliqCofins)/100))-vlcredNC;
//
//            with RegistroM610New do begin
//            {02} COD_CONT := ccNaoAcumAliqBasica;
//            {03} VL_REC_BRT := vlBC;
//            {04} VL_BC_CONT := vlBcCofins;
//            {05} ALIQ_COFINS := aliqCofins;
//            {06} QUANT_BC_COFINS := 0;
//            {07} ALIQ_COFINS_QUANT := 0;
//            {08} VL_CONT_APUR := StrToCurr(FormatCurr('#####0.00',(vlBcCofins*aliqCofins)/100));
//            {09} VL_AJUS_ACRES := 0;
//            {10} VL_AJUS_REDUC := 0;
//            {11} VL_CONT_DIFER := 0;
//            {12} VL_CONT_DIFER_ANT:= 0;
//            {13} VL_CONT_PER := StrToCurr(FormatCurr('#####0.00',(vlBcCofins*aliqCofins)/100));
//            end;
//          end;


      end;
   end;
   btnB_M.Enabled := false;

   if cbConcomitante.Checked then
   begin
      ACBrSPEDPisCofins1.WriteBloco_M;
      LoadToMemo;
   end;
end;

procedure TFrmSPEDPisCofins.btnB_AClick(Sender: TObject);
var
  INotas: Integer;
  IItens: Integer;
  NNotas: Integer;
  BNotas: Integer;
begin
   // Alimenta o componente com informações para gerar todos os registros do
   // Bloco A.
   btnB_A.Enabled := false;

   NNotas := StrToInt64Def(edNotas.Text,1);
   BNotas := StrToInt64Def(edBufNotas.Text,1);

   ProgressBar1.Visible := cbConcomitante.Checked ;
   ProgressBar1.Max     := NNotas;
   ProgressBar1.Position:= 0 ;

   with ACBrSPEDPisCofins1.Bloco_A do
   begin
      with RegistroA001New do
      begin
         IND_MOV := imComDados;
         //
         with RegistroA010New do
         begin
            CNPJ := '11111111000191'; //ou 33333333333328
           for INotas := 1 to NNotas do
           begin
              with RegistroA100New do
              begin
                 IND_OPER      := itoContratado;
                 IND_EMIT      := iedfProprio;
                 COD_PART      := '2'; // baseado no registro 0150
                 COD_SIT       := sdfRegular;
                 SER           := '';
                 SUB           := '';
                 NUM_DOC       := FormatFloat('NF000000',INotas);
                 CHV_NFSE      := '';
                 DT_DOC        := DT_INI + INotas;
                 DT_EXE_SERV   := DT_INI + INotas;
                 VL_DOC        := 0.01; //Deve ser maior que zero
                 IND_PGTO      := tpSemPagamento;
                 VL_DESC       := 0;
                 VL_BC_PIS     := 0;
                 VL_PIS        := 0;
                 VL_BC_COFINS  := 0;
                 VL_COFINS     := 0;
                 VL_PIS_RET    := 0;
                 VL_COFINS_RET := 0;
                 VL_ISS        := 0;

                 //A170
                 for IItens := 1 to 5 do
                 begin
                   with RegistroA170New do   //Inicio Adicionar os Itens:
                   begin
                      NUM_ITEM         := IItens;
                      COD_ITEM         := FormatFloat('000000', NUM_ITEM); //Código deve ser baseado no registro 0200
//                      COD_ITEM         := IntToStr(NUM_ITEM);
                      DESCR_COMPL      := FormatFloat('NF000000',INotas)+' -> ITEM '+COD_ITEM;
                      VL_ITEM          := 0;
                      VL_DESC          := 0;
                      NAT_BC_CRED      := bccOutrasOpeComDirCredito;
                      IND_ORIG_CRED    := opcMercadoInterno;
                      CST_PIS          := stpisOutrasOperacoes;
                      VL_BC_PIS        := 0;
                      ALIQ_PIS         := 0;
                      VL_PIS           := 0;
                      CST_COFINS       := stcofinsOutrasOperacoes;
                      VL_BC_COFINS     := 0;
                      ALIQ_COFINS      := 0;
                      VL_COFINS        := 0;
                      COD_CTA          := '01';
                      COD_CCUS         := '';
//                      COD_CCUS         := '123'; //Para usar o COD_CCUS é necessário gerar, primeiro, um registro 0600 correspondente.
                    end; //Fim dos Itens;
                 end;
              end;
              if cbConcomitante.Checked then
              begin
                 if (INotas mod BNotas) = 0 then   // Gravar a cada N notas
                 begin
                    // Grava registros na memoria para o TXT, e limpa memoria
                    ACBrSPEDPisCofins1.WriteBloco_A( False );  // False, NAO fecha o Bloco
                    ProgressBar1.Position := INotas;
                    Application.ProcessMessages;
                 end;
              end;
           end;
         end;
      end;
   end;
   if cbConcomitante.Checked then
   begin
      ACBrSPEDPisCofins1.WriteBloco_A(True);  // True, fecha o Bloco
      LoadToMemo;
   end;

   ProgressBar1.Visible := False ;
end;

end.
